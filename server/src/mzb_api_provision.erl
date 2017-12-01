-module(mzb_api_provision).

-export([
    provision_nodes/2,
    clean_nodes/3,
    ensure_file_content/5,
    ensure_dir/4
]).

-include_lib("mzbench_language/include/mzbl_types.hrl").

-define(MICROSEC_IN_SEC, 1000000).

provision_nodes(Config, Logger) ->
    #{
        provision_nodes := ProvisionNodes,
        director_host := DirectorHost,
        worker_hosts := WorkerHosts,
        user_name := UserName,
        env := Env
    } = Config,

    UniqHosts = lists:usort([DirectorHost|WorkerHosts]),
    Logger(info, "Provisioning ~p nodes with config:~n~p", [length(UniqHosts), Config]),
    RootDir = mzb_api_bench:remote_path("", Config),
    ok = ensure_dir(UserName, UniqHosts, RootDir, Logger),

    _TimeDifferences = ntp_check(UserName, UniqHosts, Logger),

    NodeDeployPath = mzb_api_paths:node_deployment_path(),

    catch mzb_subprocess:remote_cmd(
        UserName, UniqHosts,
        io_lib:format(
            "ps -ef | grep beam | grep -v grep | grep -v mzbench_api && ~s/mzbench/bin/mzbench stop; true",
            [NodeDeployPath]),
        [], Logger),

    case ProvisionNodes of
        true ->
            ok = install_node(UniqHosts, Config, Logger),
            install_workers(UniqHosts, Config, Logger, Env);
        _ -> ok
    end,
    DirectorNode = nodename(director_sname(Config), 0),
    WorkerNodes = [nodename(worker_sname(Config), N) || N <- lists:seq(1, length(WorkerHosts))],
    Nodes = [DirectorNode | WorkerNodes],
    ensure_vm_args([DirectorHost|WorkerHosts], Nodes, Config, Logger),
    _ = mzb_subprocess:remote_cmd(
        UserName,
        [DirectorHost|WorkerHosts],
        io_lib:format("cd ~s && ~s/mzbench/bin/mzbench start", [RootDir, NodeDeployPath]),
        [],
        Logger),
    NodePids = mzb_subprocess:remote_cmd(
        UserName,
        [DirectorHost|WorkerHosts],
        io_lib:format("cd ~s && ~s/mzbench/bin/mzbench getpid", [RootDir, NodeDeployPath]),
        [],
        Logger),
    InterconnectPorts = mzb_lists:pmap(fun(H) ->
        [Res] = release_rpcterms(UserName, H, RootDir, "mzb_interconnect_sup", "get_port", Logger),
        erlang:list_to_integer(Res) end, WorkerHosts),

    {lists:zip(Nodes, [DirectorHost|WorkerHosts]), InterconnectPorts, NodePids, get_management_port(Config, Logger)}.

get_management_port(Config = #{director_host:= DirectorHost, user_name:= UserName}, Logger) ->
    [Res] = release_rpcterms(UserName, DirectorHost, mzb_api_bench:remote_path("", Config),
        "mzb_management_tcp_protocol", "get_port", Logger),
    Logger(info, "Management port: ~s", [Res]),
    erlang:list_to_integer(Res).

release_rpcterms(UserName, Host, ConfigPath, Module, Function, Logger) ->
    mzb_subprocess:remote_cmd(
        UserName,
        [Host],
        io_lib:format("cd ~s && ~s/mzbench/bin/mzbench",
            [ConfigPath, mzb_api_paths:node_deployment_path()]),
        ["rpcterms", Module, Function],
        Logger, []).

% couldn't satisfy both erl 18 and 19 dialyzers, spec commented
%-spec clean_nodes(#{director_host:=_, purpose:=atom() | binary() | [atom() | [any()] | char()], user_name:=_, worker_hosts:=_, _=>_}, fun((_,_,_) -> any())) -> ok.
clean_nodes(NodePids, Config, Logger) ->
    #{
        user_name:= UserName,
        director_host:= DirectorHost,
        worker_hosts:= WorkerHosts} = Config,
    RootDir = mzb_api_bench:remote_path("", Config),
    Codes = mzb_subprocess:remote_cmd(
        UserName,
        [DirectorHost|WorkerHosts],
        io_lib:format("cd ~s; timeout 30s ~s/mzbench/bin/mzbench stop > /dev/null 2>&1; echo $?",
            [RootDir, mzb_api_paths:node_deployment_path()]),
        [],
        Logger),
    _ = kill_nodes(NodePids, [DirectorHost|WorkerHosts], Codes, UserName, Logger),
    length(RootDir) > 1 andalso mzb_subprocess:remote_cmd(UserName, [DirectorHost|WorkerHosts], io_lib:format("rm -rf ~s", [RootDir]), [], Logger),
    ok.

kill_nodes([], _, _, _, _) -> ok;
kill_nodes(_, [], _, _, _) -> ok;
kill_nodes(_, _, [], _, _) -> ok;
kill_nodes([Pid | NodePids], [H|Hosts], [Code|StopResults], UserName, Logger) ->
    IsStoppedAlready =
        try erlang:list_to_integer(Code) of
            0 -> true;
            _ -> false
        catch
            _:Error ->
                lager:error("Bad node stop code: ~p~nReason: ~p", [Code, Error]),
                false
        end,

    case IsStoppedAlready of
        false -> mzb_subprocess:remote_cmd(UserName, [H],
                    io_lib:format("kill -9 ~p; true", [Pid]), [], Logger);
        true -> ok
    end,
    kill_nodes(NodePids, Hosts, StopResults, UserName, Logger).

ntp_check(_, [H], Logger) ->
    Logger(info, "There's only one host, no need to make ntp check", []),
    [{H, 0}];
ntp_check(UserName, Hosts, Logger) ->
    case application:get_env(mzbench_api, ntp_max_timediff_s, undefined) of
        undefined -> [{H, undefined} || H <- Hosts];
        MaxTimeDiff ->
            NTPRes = mzb_subprocess:remote_cmd(UserName, Hosts, "ntpdate -q pool.ntp.org", [], Logger),
            Offsets = lists:map(
                    fun(X) ->
                        [_, T | _] = lists:reverse(string:tokens(X, " \n")),
                        {F, []} = string:to_float(T),
                        erlang:round(?MICROSEC_IN_SEC*F)
                    end, NTPRes),
            TimeDiff = lists:max(Offsets) - lists:min(Offsets),
            Logger(info, "NTP time diffs are: ~p, max distance is ~p microsecond", [Offsets, TimeDiff]),
            case ?MICROSEC_IN_SEC * MaxTimeDiff >= TimeDiff of
                true -> ok;
                false ->
                    Logger(error, "NTP CHECK FAILED, max time different is ~p microseconds", [TimeDiff]),
                    erlang:error({ntp_check_failed, TimeDiff})
            end,
            lists:zip(Hosts, Offsets)
    end.

nodename(Name, N) ->
    erlang:list_to_atom(mzb_string:format("~s_~b@127.0.0.1", [Name, N])).

ensure_vm_args(Hosts, Nodenames, Config, Logger) ->
    _ = mzb_lists:pmap(
        fun ({H, N}) ->
            ensure_file_content([H], vm_args_content(N, Config), "vm.args", Config, Logger)
        end, lists:zip(Hosts, Nodenames)),
    ok.

ensure_file_content(Hosts, Content, Filepath,
                    #{user_name:= UserName} = Config, Logger) ->
    Localfile = mzb_file:tmp_filename(),
    Remotefile =
        case Filepath of
            "~/" ++ _ -> Filepath;
            _ -> mzb_api_bench:remote_path(Filepath, Config)
        end,
    Logger(debug, "Ensure file content on hosts: ~p~nLocal filename: ~p~nContent: ~s~nRemote path: ~p", [Hosts, Localfile, Content, Remotefile]),
    ok = file:write_file(Localfile, Content),
    ok = ensure_file(UserName, Hosts, Localfile, Remotefile, Logger),
    ok = file:delete(Localfile).

ensure_file(_UserName, [Host], LocalPath, RemotePath, Logger) when Host == "localhost"; Host == "127.0.0.1" ->
    Logger(info, "[ COPY ] ~s -> ~s", [LocalPath, RemotePath]),
    {ok, _} = file:copy(LocalPath, RemotePath),
    ok;
ensure_file(UserName, Hosts, LocalPath, RemotePath, Logger) ->
    UserNameParam =
        case UserName of
            undefined -> "";
            _ -> io_lib:format("~s@", [UserName])
        end,
    Logger(info, "[ SCP ] ~s -> ~s<HOST>:~s~n  for ~p", [LocalPath, UserNameParam, RemotePath, Hosts]),
    _ = mzb_lists:pmap(
        fun (Host) ->
            mzb_subprocess:exec_format("scp -o StrictHostKeyChecking=no ~s ~s~s:~s",
                [LocalPath, UserNameParam, Host, RemotePath], [stderr_to_stdout], fun (_, _, _) -> ok end)
        end, Hosts),
    ok.

-spec ensure_dir(undefined | string(), [string()], string(), fun((atom(), string(), [term()]) -> ok)) -> ok.
ensure_dir(_User, [Local], Dir, Logger) when Local == "localhost"; Local == "127.0.0.1" ->
    Logger(info, "[ MKDIR ] ~s", [Dir]),
    % The trailing slash is needed, otherwise it will only
    % create Dir's parent, but not Dir itself
    ok = filelib:ensure_dir(Dir ++ "/");
ensure_dir(User, Hosts, Dir, Logger) ->
    _ = mzb_subprocess:remote_cmd(User, Hosts, "mkdir", ["-p", Dir], Logger, [stderr_to_stdout]),
    ok.

director_sname(#{id:= Id}) -> "mzb_director" ++ integer_to_list(Id).
worker_sname(#{id:= Id})   -> "mzb_worker" ++ integer_to_list(Id).

vm_args_content(NodeName, #{node_log_port:= LogPort, node_management_port:= Port,
    vm_args:= ConfigArgs, node_log_user_port:= LogUserPort,
    node_interconnect_port:= InterconnectPort} = Config) ->
    UpdateIntervalMs = mzb_bc:maps_get(metric_update_interval_ms, Config, undefined),
    NewArgs =
        [mzb_string:format("-name ~s", [NodeName]),
         mzb_string:format("-mzbench node_management_port ~b", [Port]),
         mzb_string:format("-mzbench node_log_port ~b", [LogPort]),
         mzb_string:format("-mzbench node_log_user_port ~b", [LogUserPort]),
         mzb_string:format("-mzbench node_interconnect_port ~b", [InterconnectPort])] ++
        [mzb_string:format("-mzbench metric_update_interval_ms ~b", [UpdateIntervalMs]) || UpdateIntervalMs /= undefined],

    io_lib:format(string:join([A ++ "~n" || A <- NewArgs ++ ConfigArgs], ""), []).

get_host_os_id(UserName, Hosts, Logger) ->
    OSIds = mzb_subprocess:remote_cmd(UserName, Hosts, "uname -sr", [], Logger, []),
    [string:to_lower(mzb_string:char_substitute(lists:flatten(ID), $ , $-)) || ID <- OSIds].

get_host_erts_version(UserName, Hosts, Logger) ->
    Versions = mzb_subprocess:remote_cmd(UserName, Hosts,
        "erl -noshell -eval 'io:fwrite(\\\"~s\\\", [erlang:system_info(version)]).' -s erlang halt",
        [], Logger, []),
    [lists:flatten(V) || V <- Versions].

get_host_system_id(UserName, Hosts, Logger) ->
    OSIds = get_host_os_id(UserName, Hosts, Logger),
    ERTSVersions = get_host_erts_version(UserName, Hosts, Logger),
    [{H, mzb_string:format("~s_erts-~s", [Id, ERTS])} || {H, Id, ERTS} <- lists:zip3(Hosts, OSIds, ERTSVersions)].

download_file(User, Host, FromFile, ToFile, Logger) ->
    _ = case Host of
        Local when Local == "localhost"; Local == "127.0.0.1" ->
            Logger(info, "[ COPY ] ~s <- ~s", [ToFile, FromFile]),
            {ok, _} = file:copy(FromFile, ToFile);
        _ ->
            UserNameParam =
                case User of
                    undefined -> "";
                    _ -> io_lib:format("~s@", [User])
                end,
            % we can't copy to target file directly because there might be several processes doing it
            % instead we copy to tmp file and after that we atomically rename it to target file
            % also we can't create tmp file in /tmp because it is often being
            % mounted to a different fs than target file and it makes file:rename to fail
            TmpFile = mzb_file:tmp_filename(filename:dirname(ToFile)),
            _ = mzb_subprocess:exec_format("scp -o StrictHostKeyChecking=no ~s~s:~s ~s",
                [UserNameParam, [Host], FromFile, TmpFile], [stderr_to_stdout], Logger),
            Logger(info, "[ MV ] ~s -> ~s", [TmpFile, ToFile]),
            ok = file:rename(TmpFile, ToFile)
    end,
    ok.

package_version(InstallSpec, Logger) ->
    case InstallSpec of
        #git_install_spec{repo = Repo, branch = Branch} ->
            mzb_git:get_git_short_sha1(Repo, Branch, Logger);
        #rsync_install_spec{} ->
            {A, B, C} = os:timestamp(),
            mzb_string:format("~p.~p.~p", [A, B, C])
    end.

-spec install_package([string()], string(), install_spec(), string(), term(), fun()) -> ok.
install_package(Hosts, PackageName, InstallSpec, InstallationDir, Config, Logger) ->
    Version = case application:get_env(mzbench_api, auto_update_deployed_code, enable) of
                enable -> package_version(InstallSpec, Logger);
                _ -> "someversion"
    end,
    #{user_name:= User} = Config,
    PackagesDirs = mzb_api_paths:tgz_packages_dirs(),
    HostsAndOSs = case application:get_env(mzbench_api, custom_os_code_builds, enable) of
                        enable -> case InstallSpec of
                                    #git_install_spec{build = "local"} -> [{H, "noarch"} || H <- Hosts];
                                    _ -> get_host_system_id(User, Hosts, Logger)
                                end;
                        _ -> [{H, "someos"} || H <- Hosts]
    end,
    ok = filelib:ensure_dir(hd(PackagesDirs) ++ "/"),
    UniqueOSs = lists:usort([OS || {_Host, OS} <- HostsAndOSs]),
    NeededTarballs =
        [{OS, find_package(PackagesDirs, mzb_string:format("~s-~s-~s.tgz", [PackageName, Version, OS]))}
        || OS <- UniqueOSs],
    MissingTarballs = [{OS, T} || {OS, T} <- NeededTarballs, not filelib:is_file(T)],
    Logger(info, "Missing tarballs: ~p", [MissingTarballs]),
    OSsWithMissingTarballs = case [OS || {OS, _} <- MissingTarballs] of
        ["noarch"] -> Logger(info, "Building package ~s on api server", [PackageName]),
                      [TarballPath] = [T || {_, T} <- MissingTarballs],
                      build_package_on_host("127.0.0.1", User, TarballPath, InstallSpec, Logger),
                      [];
                 L -> L
        end,
    _ = mzb_lists:pmap(fun({Host, OS}) ->
            {OS, LocalTarballPath} = lists:keyfind(OS, 1, NeededTarballs),
            RemoteTarballPath = mzb_file:tmp_filename() ++ ".tgz",
            ExtractDir = mzb_file:tmp_filename(),
            try
                case lists:member(OS, OSsWithMissingTarballs) of
                    true ->
                        build_package_on_host(Host, User, RemoteTarballPath, InstallSpec, Logger),
                        case lists:keyfind(OS, 2, HostsAndOSs) of
                            {Host, OS} ->
                                Logger(info, "Downloading package ~s from ~s", [PackageName, Host]),
                                download_file(User, Host, RemoteTarballPath, LocalTarballPath, Logger);
                            _ ->
                                ok
                        end;
                    false ->
                        ensure_file(User, [Host], LocalTarballPath, RemoteTarballPath, Logger)
                end,
                % Extract tgz to tmp directory and then rsync it to the installation directory in order to prevent
                % different nodes provisioning to affect each other (if we ran several nodes on one host)
                InstallationCmd = mzb_string:format("mkdir -p ~s && cd ~s && tar xzf ~s && mkdir -p ~s && rsync -aW ~s/ ~s",
                    [ExtractDir, ExtractDir, RemoteTarballPath, InstallationDir, ExtractDir, InstallationDir]),
                _ = mzb_subprocess:remote_cmd(User, [Host], InstallationCmd, [], Logger)
            after
                RemoveCmd = mzb_string:format("rm -rf ~s; rm -rf ~s; true", [RemoteTarballPath, ExtractDir]),
                _ = mzb_subprocess:remote_cmd(User, [Host], RemoveCmd, [], Logger)
            end
        end,
        HostsAndOSs),
    ok.

find_package(PackagesDirs, PackageName) ->
    lists:foldl(
        fun(Dir, LastKnown) ->
            Current = filename:join(Dir, PackageName),
            case filelib:is_file(Current) of
                true -> Current;
                false -> LastKnown
            end
        end, filename:join(hd(PackagesDirs), PackageName), PackagesDirs).

build_package_on_host(Host, User, RemoteTarballPath, InstallSpec, Logger) ->
    DeploymentDirectory = mzb_file:tmp_filename(),
    case InstallSpec of
        #git_install_spec{repo = GitRepo, branch = GitBranch, dir = GitSubDir} ->
            Cmd = mzb_string:format("git clone ~s deployment_code && cd deployment_code && git checkout ~s && cd ./~s", [GitRepo, GitBranch, GitSubDir]),
            GenerationCmd = io_lib:format("mkdir -p ~s && cd ~s && ~s "
                                          "&& make generate_tgz && mv *.tgz ~s",
                                          [DeploymentDirectory, DeploymentDirectory,
                                           Cmd, RemoteTarballPath]),
            _ = mzb_subprocess:remote_cmd(User, [Host], GenerationCmd, [], Logger);
        #rsync_install_spec{remote = Remote, excludes = Excludes, dir = SubDir} ->
            TargetFolder = DeploymentDirectory ++ "/deployment_code",
            Cmd = mzb_string:format("rsync -aW --rsync-path='mkdir -p ~s && rsync' ~s ~s/ ~s",
                [TargetFolder, string:join(["--exclude=" ++ E || E <- Excludes], " "), Remote, User ++ "@" ++ Host ++ ":" ++ TargetFolder]),
            mzb_subprocess:exec_format(Cmd, [], [], Logger),
            GenerationCmd = io_lib:format("cd ~s/~s && make generate_tgz && mv *.tgz ~s",
                                          [TargetFolder, SubDir, RemoteTarballPath]),
            _ = mzb_subprocess:remote_cmd(User, [Host], GenerationCmd, [], Logger)
    end,
    ok.

install_node(Hosts, #{node_install_spec:= InstallSpec} = Config, Logger) ->
    install_package(
        Hosts,
        "node",
        InstallSpec,
        application:get_env(mzbench_api, node_deployment_path, ""),
        Config,
        Logger).

-spec get_worker_name(install_spec()) -> string().
get_worker_name(#git_install_spec{repo = GitRepo, dir = GitSubDir}) ->
    Base = filename:basename(GitSubDir),
    case re:replace(Base, "\\W", "", [global, {return, list}]) of
        [] -> filename:basename(GitRepo, ".git");
        BaseSanitized -> BaseSanitized
    end;
get_worker_name(#rsync_install_spec{remote = Remote, dir = ""}) -> filename:basename(Remote);
get_worker_name(#rsync_install_spec{dir = SubDir}) -> filename:basename(SubDir).

install_worker(Hosts, InstallSpec, Config, Logger) ->
    WorkerName = get_worker_name(InstallSpec),
    install_package(Hosts, WorkerName, InstallSpec, application:get_env(mzbench_api, worker_deployment_path, ""), Config, Logger).

install_workers(Hosts, #{script:= Script} = Config, Logger, Env) ->
    #{ body := Body } = Script,
    AST = mzbl_script:read_from_string(binary_to_list(Body)),
    NormEnv = mzbl_script:normalize_env(Env),
    _ = [install_worker(Hosts, IS, Config, Logger) || IS <- mzbl_script:extract_install_specs(AST, NormEnv)],
    ok.
