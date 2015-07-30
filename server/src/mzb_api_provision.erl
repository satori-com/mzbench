-module(mzb_api_provision).

-export([
    provision_nodes/2,
    clean_nodes/2,
    ensure_file_content/5
]).

-include_lib("mzbench_language/include/mzbl_types.hrl").

provision_nodes(Config, Logger) ->
    #{
        provision_nodes := ProvisionNodes,
        director_host := DirectorHost,
        worker_hosts := WorkerHosts,
        user_name := UserName,
        remote_dir := RootDir,
        env := Env
    } = Config,

    UniqHosts = lists:usort([DirectorHost|WorkerHosts]),
    Logger(info, "Provisioning nodes: ~p~nWith config: ~p", [UniqHosts, Config]),
    _ = mzb_subprocess:remote_cmd(UserName, UniqHosts, io_lib:format("mkdir -p ~s", [RootDir]), [], Logger),

    CheckResult = (catch ntp_check(UserName, UniqHosts, Logger)),
    Logger(info, "NTP check result: ~p", [CheckResult]),

    catch mzb_subprocess:remote_cmd(UserName, UniqHosts, "~/mz/mzbench/bin/mzbench stop; true", [], Logger),

    case ProvisionNodes of
        true ->
            install_node(UniqHosts, Config, Logger),
            install_workers(UniqHosts, Config, Logger, Env),
            ensure_cookie(UserName, UniqHosts, Config, Logger);
        _ -> ok
    end,

    [DirNode] = [nodename(director_sname(Config), H) || H <- get_hostnames(UserName, [DirectorHost], Logger)],
    WorkerNodes = [nodename(worker_sname(Config), H) || H <- get_hostnames(UserName, WorkerHosts, Logger)],

    ensure_vm_args([DirectorHost|WorkerHosts], [DirNode|WorkerNodes], Config, Logger),
    _ = mzb_subprocess:remote_cmd(UserName, [DirectorHost|WorkerHosts], io_lib:format("cd ~s && ~~/mz/mzbench/bin/mzbench start", [RootDir]), [], Logger),

    _ = mzb_subprocess:remote_cmd(UserName, [DirectorHost], "~/mz/mzbench/bin/wait_cluster_start.escript", ["30000", DirNode | WorkerNodes], Logger),
    DirNode.

clean_nodes(Config, Logger) ->
    #{
        user_name:= UserName,
        remote_dir:= RootDir,
        director_host:= DirectorHost,
        worker_hosts:= WorkerHosts} = Config,
    _ = mzb_subprocess:remote_cmd(UserName, [DirectorHost|WorkerHosts], io_lib:format("cd ~s && ~~/mz/mzbench/bin/mzbench stop", [RootDir]), [], Logger),
    length(RootDir) > 1 andalso mzb_subprocess:remote_cmd(UserName, [DirectorHost|WorkerHosts], io_lib:format("rm -rf ~s", [RootDir]), [], Logger).

ntp_check(UserName, Hosts, Logger) ->
    Offsets = lists:map(fun(X) ->
        Logger(info, "ntpdate response: ~p", [X]),
        [_, T | _] = lists:reverse(string:tokens(X, " \n")),
        {F, []} = string:to_float(T), F end,
        mzb_subprocess:remote_cmd(UserName, Hosts, "ntpdate -q pool.ntp.org", [], Logger)),
    TimeDiff = lists:max(Offsets) - lists:min(Offsets),
    Logger(info, "NTP time diffs are: ~p, max distance is ~p", [Offsets, TimeDiff]),
    case TimeDiff < application:get_env(mzbench_api, ntp_max_timediff, 0.1) of
        true -> ok;
        _ -> erlang:error({ntp_check_failed, TimeDiff})
    end.

nodename(Name, Host) ->
    Name ++ "@" ++ Host.

get_hostnames(UserName, Hosts, Logger) ->
    Hostnames = mzb_subprocess:remote_cmd(UserName, Hosts, "hostname", [], Logger, []),
    Logger(debug, "fqdn for ~p: ~p", [Hosts, Hostnames]),
    Res = [ hd(string:tokens(FName, ".")) || FName <- Hostnames],
    Logger(info, "Shortnames for ~p are ~p", [Hosts, Res]),
    Res.

ensure_cookie(UserName, Hosts, #{purpose:= Cookie} = Config, Logger) ->
    CookieFile = "~/.erlang.cookie",
    NotLocalhosts = [H || H <- Hosts, H =/= "localhost"],
    ensure_file_content(NotLocalhosts, Cookie, CookieFile, Config, Logger),
    _ = mzb_subprocess:remote_cmd(UserName, NotLocalhosts, "chmod", ["go-rwx", CookieFile], Logger),
    ok.

ensure_vm_args(Hosts, Nodenames, Config, Logger) ->
    _ = mzb_lists:pmap(
        fun ({H, N}) ->
            ensure_file_content([H], vm_args_content(N), "vm.args", Config, Logger)
        end, lists:zip(Hosts, Nodenames)),
    ok.

ensure_file_content(Hosts, Content, Filepath,
                    #{user_name:= UserName, remote_dir:= RemoteRoot}, Logger) ->
    Localfile = mzb_file:tmp_filename(),
    Remotefile =
        case Filepath of
            "~/" ++ _ -> Filepath;
            _ -> filename:join(RemoteRoot, Filepath)
        end,
    Logger(debug, "Ensure file content on hosts: ~p~nLocal filename: ~p~nContent: ~s~nRemote path: ~p", [Hosts, Localfile, Content, Remotefile]),
    ok = file:write_file(Localfile, Content),
    ok = ensure_file(UserName, Hosts, Localfile, Remotefile, Logger),
    ok = file:delete(Localfile).

ensure_file(UserName, Hosts, LocalPath, RemotePath, Logger) ->
    _ = mzb_lists:pmap(
        fun ("localhost") ->
                mzb_subprocess:exec_format("cp ~s ~s", [LocalPath, RemotePath], [stderr_to_stdout], Logger);
            (Host) ->
                mzb_subprocess:exec_format("scp -o StrictHostKeyChecking=no ~s ~s@~s:~s", [LocalPath, UserName, Host, RemotePath], [stderr_to_stdout], Logger)
        end, Hosts),
    ok.

director_sname(#{id:= Id}) -> "mzb_director" ++ integer_to_list(Id).
worker_sname(#{id:= Id})   -> "mzb_worker" ++ integer_to_list(Id).

vm_args_content(NodeName) ->
    io_lib:format("-sname ~s~n", [NodeName]).

get_git_sha1(GitRepo, GitRef, Logger) ->
    case string:tokens(lists:flatten(mzb_subprocess:exec_format("git ls-remote ~s ~s", [GitRepo, GitRef], [stderr_to_stdout], Logger)), "\t") of
        [Commit | _] -> Commit;
        _ -> GitRef
    end.

substitute(String, OldSubStr, NewSubStr) ->
    string:join(string:tokens(String, OldSubStr), NewSubStr).

get_git_short_sha1(#git_install_spec{repo = GitRepo, branch = GitRef}, Logger) ->
    lists:sublist(get_git_sha1(GitRepo, GitRef, Logger), 7).

get_host_os_id(UserName, Host, Logger) ->
    string:to_lower(substitute(lists:flatten(mzb_subprocess:remote_cmd(UserName, [Host], "uname -sr", [], Logger, [])), " ", "-")).

download_file(User, Host, FromFile, ToFile, Logger) ->
    TmpFile = mzb_file:tmp_filename(),
    _ = case Host of
        "localhost" -> mzb_subprocess:exec_format("cp ~s ~s", [FromFile, TmpFile], [stderr_to_stdout], Logger);
        _ -> mzb_subprocess:exec_format("scp -o StrictHostKeyChecking=no ~s@~s:~s ~s",
            [User, [Host], FromFile, TmpFile], [stderr_to_stdout], Logger)
    end,
    _ = mzb_subprocess:exec_format("mv ~s ~s", [TmpFile, ToFile], [stderr_to_stdout], Logger),
    ok.

-spec install_package([string()], string(), install_spec(), string(), term(), fun()) -> ok.
install_package(Hosts, PackageName, InstallSpec, InstallationDir, Config, Logger) ->
    Version = case InstallSpec of
        #git_install_spec{} -> get_git_short_sha1(InstallSpec, Logger);
        #rsync_install_spec{} ->
            {A, B, C} = os:timestamp(),
            lists:flatten(io_lib:format("~p.~p.~p", [A, B, C]))
    end,
    #{user_name:= User} = Config,
    HostsAndOSs = mzb_lists:pmap(fun (Host) -> {Host, get_host_os_id(User, Host, Logger)} end, Hosts),
    CloneAndCDCommand = case InstallSpec of
        #git_install_spec{repo = GitRepo, branch = GitBranch, dir = GitSubDir} ->
            lists:flatten(io_lib:format("git clone ~s deployment_code && cd deployment_code/~s && git checkout ~s", [GitRepo, GitSubDir, GitBranch]));
        #rsync_install_spec{remote = Remote, excludes = Excludes} ->
            lists:flatten(io_lib:format("rsync -aW ~s ~s deployment_code && cd deployment_code",
                [string:join(["--exclude=" ++ E || E <- Excludes], " "), Remote]))
    end,
    PackagesDir = application:get_env(mzbench_api, tgz_packages_dir, undefined),
    _ = mzb_subprocess:exec_format("mkdir -p ~s", [PackagesDir], [stderr_to_stdout], Logger),
    UniqueOSs = mzb_lists:unique([OS || {_Host, OS} <- HostsAndOSs]),
    PackagesDir = application:get_env(mzbench_api, tgz_packages_dir, undefined),
    NeededTarballs =
        [{OS, filename:join(PackagesDir, lists:flatten(io_lib:format("~s-~s-~s.tgz", [PackageName, Version, OS])))}
        || OS <- UniqueOSs],
    MissingTarballs = [{OS, T} || {OS, T} <- NeededTarballs, not filelib:is_file(T)],
    OSsWithMissingTarballs = [OS || {OS, _} <- MissingTarballs],
    _ = mzb_lists:pmap(fun({Host, OS}) ->
            LocalTarballPath = lists:keyfind(NeededTarballs, 1, OS),
            RemoteTarballPath = mzb_file:tmp_filename() ++ ".tgz",
            case lists:member(OS, OSsWithMissingTarballs) of
                true ->
                    DeploymentDirectory = mzb_file:tmp_filename(),
                    GenerationCmd = io_lib:format("mkdir -p ~s && cd ~s && ~s "
                                                  "&& make generate_tgz && mv *.tgz ~s",
                                                  [DeploymentDirectory, DeploymentDirectory,
                                                   CloneAndCDCommand, RemoteTarballPath]),
                    lager:info("Building package ~s on ~s", [PackageName, Host]),
                    _ = mzb_subprocess:remote_cmd(User, [Host], GenerationCmd, [], Logger),
                    case lists:keyfind(OS, 2, HostsAndOSs) of
                        {Host, OS} ->
                            lager:info("Downloading package ~s from ~s", [PackageName, Host]),
                            download_file(User, Host, RemoteTarballPath, LocalTarballPath, Logger);
                        _ -> ok
                    end;
                false ->
                    lager:info("Uploading package ~s to ~s", [PackageName, Host]),
                    ensure_file(User, [Host], LocalTarballPath, RemoteTarballPath, Logger)
            end,
            InstallationCmd = lists:flatten(io_lib:format("mkdir -p ~s && cd ~s && tar xzf ~s", [InstallationDir, InstallationDir, RemoteTarballPath])),
            _ = mzb_subprocess:remote_cmd(User, [Host], InstallationCmd, [], Logger)
        end,
        HostsAndOSs).

install_node(Hosts, Config, Logger) ->
    #{node_git:= GitRepo, node_commit:= GitBranch} = Config,
    Logger(info, "Node repo: ~s ~s", [GitRepo, GitBranch]),
    Branch = case GitBranch of
        undefined ->
            {ok, GitRev} = application:get_key(mzbench_api, vsn),
            GitRev;
        _ -> GitBranch
    end,

    install_package(
        Hosts,
        "node",
        mzbl_script:make_git_install_spec(GitRepo, Branch, "node"),
        application:get_env(mzbench_api, node_deployment_path, ""),
        Config,
        Logger).

-spec get_worker_name(install_spec()) -> string().
get_worker_name(#git_install_spec{repo = GitRepo, dir = ""}) -> filename:basename(GitRepo, ".git");
get_worker_name(#git_install_spec{dir = GitSubDir}) -> filename:basename(GitSubDir);
get_worker_name(#rsync_install_spec{remote = Remote}) -> filename:basename(Remote).

install_worker(Hosts, InstallSpec, Config, Logger) ->
    WorkerName = get_worker_name(InstallSpec),
    install_package(Hosts, WorkerName, InstallSpec, application:get_env(mzbench_api, worker_deployment_path, ""), Config, Logger).

install_workers(Hosts, #{script:= Script} = Config, Logger, Env) ->
    #{ body := Body } = Script,

    % AutoEnv here has dummy values, but they are sufficient
    % for parsing the script and extracting make_install specs.
    AutoEnv = [{"nodes_num", length(Hosts)},
               {"bench_hosts", []},
               {"bench_script_dir", ""},
               {"bench_workers_dir", []}],
    DeepBinaryToString =
        fun Recur(L) when is_list(L) -> lists:map(Recur, L);
            Recur({Key, Value}) -> {Recur(Key), Recur(Value)};
            Recur(B) when is_binary(B) -> binary_to_list(B);
            Recur(X) -> X
        end,
    AST = mzbl_script:read_from_string(binary_to_list(Body), AutoEnv ++ DeepBinaryToString(Env)),
    _ = [install_worker(Hosts, IS, Config, Logger) || IS <- mzbl_script:extract_install_specs(AST)],
    ok.

