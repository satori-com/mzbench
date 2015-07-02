-module(mzb_api_provision).

-export([
    provision_nodes/2,
    clean_nodes/2,
    remote_cmd/5,
    remote_cmd/6,
    ensure_file_content/5,
    exec_format/4,
    exec_format/7
]).

-include_lib("mz_bench_language/include/mzbl_types.hrl").

provision_nodes(Config, Logger) ->
    #{
        dont_provision_nodes := DontProvisionNodes,
        director_host := DirectorHost,
        worker_hosts := WorkerHosts,
        user_name := UserName,
        remote_dir := RootDir,
        env := Env
    } = Config,

    UniqHosts = lists:usort([DirectorHost|WorkerHosts]),
    log(Logger, info, "Provisioning nodes: ~p~nWith config: ~p", [UniqHosts, Config]),
    _ = remote_cmd(UserName, UniqHosts, io_lib:format("mkdir -p ~s", [RootDir]), [], Logger),

    CheckResult = (catch ntp_check(UserName, UniqHosts, Logger)),
    log(Logger, info, "NTP check result: ~p", [CheckResult]),

    catch remote_cmd(UserName, UniqHosts, "~/mz/mz_bench/bin/mz_bench stop; true", [], Logger),

    case DontProvisionNodes of
        false ->
            install_node(UniqHosts, Config, Logger),
            install_workers(UniqHosts, Config, Logger, Env),
            ensure_cookie(UserName, UniqHosts, Config, Logger);
        _ -> ok
    end,

    [DirNode] = [nodename(director_sname(Config), H) || H <- get_hostnames(UserName, [DirectorHost], Logger)],
    WorkerNodes = [nodename(worker_sname(Config), H) || H <- get_hostnames(UserName, WorkerHosts, Logger)],

    ensure_vm_args([DirectorHost|WorkerHosts], [DirNode|WorkerNodes], Config, Logger),
    _ = remote_cmd(UserName, [DirectorHost|WorkerHosts], io_lib:format("cd ~s && ~~/mz/mz_bench/bin/mz_bench start", [RootDir]), [], Logger),

    _ = remote_cmd(UserName, [DirectorHost], "~/mz/mz_bench/bin/wait_cluster_start.escript", ["30000", DirNode | WorkerNodes], Logger),
    DirNode.

clean_nodes(Config, Logger) ->
    #{
        user_name:= UserName,
        remote_dir:= RootDir,
        director_host:= DirectorHost,
        worker_hosts:= WorkerHosts} = Config,
    _ = remote_cmd(UserName, [DirectorHost|WorkerHosts], io_lib:format("cd ~s && ~~/mz/mz_bench/bin/mz_bench stop", [RootDir]), [], Logger),
    length(RootDir) > 1 andalso remote_cmd(UserName, [DirectorHost|WorkerHosts], io_lib:format("rm -rf ~s", [RootDir]), [], Logger).

ntp_check(UserName, Hosts, Logger) ->
    Offsets = lists:map(fun(X) ->
        log(Logger, info, "ntpdate response: ~p", [X]),
        [_, T | _] = lists:reverse(string:tokens(X, " \n")),
        {F, []} = string:to_float(T), F end,
        remote_cmd(UserName, Hosts, "ntpdate -q pool.ntp.org", [], Logger)),
    TimeDiff = lists:max(Offsets) - lists:min(Offsets),
    log(Logger, info, "NTP time diffs are: ~p, max distance is ~p", [Offsets, TimeDiff]),
    case TimeDiff < application:get_env(mz_bench_api, ntp_max_timediff, 0.1) of
        true -> ok;
        _ -> erlang:error({ntp_check_failed, TimeDiff})
    end.

nodename(Name, Host) ->
    Name ++ "@" ++ Host.

get_hostnames(UserName, Hosts, Logger) ->
    Hostnames = remote_cmd(UserName, Hosts, "hostname", [], Logger, []),
    log(Logger, debug, "fqdn for ~p: ~p", [Hosts, Hostnames]),
    Res = [ hd(string:tokens(FName, ".")) || FName <- Hostnames],
    log(Logger, info, "Shortnames for ~p are ~p", [Hosts, Res]),
    Res.

ensure_cookie(UserName, Hosts, #{purpose:= Cookie} = Config, Logger) ->
    CookieFile = "~/.erlang.cookie",
    NotLocalhosts = [H || H <- Hosts, H =/= "localhost"],
    ensure_file_content(NotLocalhosts, Cookie, CookieFile, Config, Logger),
    _ = remote_cmd(UserName, NotLocalhosts, "chmod", ["go-rwx", CookieFile], Logger),
    ok.

ensure_vm_args(Hosts, Nodenames, Config, Logger) ->
    pmap(
        fun ({H, N}) ->
            ensure_file_content([H], vm_args_content(N), "vm.args", Config, Logger)
        end, lists:zip(Hosts, Nodenames)).

director_sname(#{id:= Id}) -> "mzb_director" ++ integer_to_list(Id).
worker_sname(#{id:= Id})   -> "mzb_worker" ++ integer_to_list(Id).

vm_args_content(NodeName) ->
    io_lib:format("-sname ~s~n", [NodeName]).

get_git_sha1(GitRepo, GitRef, Logger) when is_binary(GitRef) ->
    get_git_sha1(GitRepo, erlang:binary_to_list(GitRef), Logger);
get_git_sha1(GitRepo, GitRef, Logger) ->
    case string:tokens(lists:flatten(exec_format("git ls-remote ~s ~s", [GitRepo, GitRef], [stderr_to_stdout], Logger)), "\t") of
        [Commit | _] -> Commit;
        _ -> GitRef
    end.

substitute(String, OldSubStr, NewSubStr) ->
    string:join(string:tokens(String, OldSubStr), NewSubStr).

get_git_short_sha1(#install_spec{repo = GitRepo, branch = GitRef}, Logger) ->
    lists:sublist(get_git_sha1(GitRepo, GitRef, Logger), 7).

get_host_os_id(UserName, Host, Logger) ->
    string:to_lower(substitute(lists:flatten(remote_cmd(UserName, [Host], "uname -sr", [], Logger, [])), " ", "-")).

ensure_tgz_package(User, Host, LocalTarballName, #install_spec{repo = GitRepo, branch = GitBranch, dir = GitSubDir}, Logger) ->
    case filelib:is_file(LocalTarballName) of
        false ->
            DeploymentDirectory = tmp_filename(),
            TmpTgzFile = tmp_filename() ++ ".tgz",
            GenerationCmd = io_lib:format("mkdir -p ~s && cd ~s && git clone ~s deployment_code && "
                                          "cd deployment_code && git checkout ~s && "
                                          "cd ~s && make generate_tgz && mv *.tgz ~s",
                                          [DeploymentDirectory, DeploymentDirectory, GitRepo,
                                           GitBranch, GitSubDir, TmpTgzFile]),
            _ = remote_cmd(User, [Host], GenerationCmd, [], Logger),
            download_file(User, Host, TmpTgzFile, LocalTarballName, Logger);
        _ -> ok
    end.

download_file(User, Host, FromFile, ToFile, Logger) ->
    TmpFile = tmp_filename(),
    case Host of
        "localhost" -> exec_format("cp ~s ~s", [FromFile, TmpFile], [stderr_to_stdout], Logger);
        Host -> exec_format("scp -o StrictHostKeyChecking=no ~s@~s:~s ~s",
            [User, [Host], FromFile, TmpFile], [stderr_to_stdout], Logger)
    end,
    exec_format("mv ~s ~s", [TmpFile, ToFile], [stderr_to_stdout], Logger).

install_package(Hosts, PackageName, InstallSpec, InstallationDir, Config, Logger) ->
    ShortCommit = get_git_short_sha1(InstallSpec, Logger),
    #{remote_dir:= RemoteRoot, user_name:= User} = Config,
    pmap(fun (Host) ->
        HostOSId = get_host_os_id(User, Host, Logger),
        log(Logger, info, "[ mzb_api_provision ] Deploying package: ~s~nfrom: ~p~non: ~s (OS: ~s)", [PackageName, InstallSpec, Host, HostOSId]),
        PackagesDir = application:get_env(mz_bench_api, tgz_packages_dir, "."),
        PackageFileName = lists:flatten(io_lib:format("~s-~s-~s.tgz", [PackageName, ShortCommit, HostOSId])),
        PackageFilePath = filename:join(PackagesDir, PackageFileName),
        RemoteFilePath = filename:join(RemoteRoot, PackageFileName),

        ensure_tgz_package(User, Host, PackageFilePath, InstallSpec, Logger),
        ensure_file(User, [Host], PackageFilePath, RemoteFilePath, Logger),
        InstallationCmd = lists:flatten(io_lib:format("mkdir -p ~s && cd ~s && tar xzf ~s", [InstallationDir, InstallationDir, RemoteFilePath])),
        _ = remote_cmd(User, [Host], InstallationCmd, [], Logger)
    end, Hosts).

install_node(Hosts, Config, Logger) ->
    #{node_git:= GitRepo, node_commit:= GitBranch} = Config,
    log(Logger, info, "Node repo: ~s ~s", [GitRepo, GitBranch]),
    Branch = case GitBranch of
        <<"master">> ->
            {ok, GitRev} = application:get_key(mz_bench_api, vsn),
            GitRev;
        _ -> GitBranch
    end,

    install_package(
        Hosts,
        "node",
        #install_spec{repo = GitRepo, branch = Branch, dir = "node"},
        application:get_env(mz_bench_api, node_deployment_path, ""),
        Config,
        Logger).

get_worker_name(#install_spec{repo = GitRepo, dir = ""}) -> filename:basename(GitRepo, ".git");
get_worker_name(#install_spec{dir = GitSubDir}) -> filename:basename(GitSubDir).

install_worker(Hosts, InstallSpec, Config, Logger) ->
    WorkerName = get_worker_name(InstallSpec),
    install_package(Hosts, WorkerName, InstallSpec, application:get_env(mz_bench_api, worker_deployment_path, ""), Config, Logger).

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

remote_cmd(UserName, Hosts, Executable, Args, Logger) ->
    remote_cmd(UserName, Hosts, Executable, Args, Logger, [stderr_to_stdout]).

remote_cmd(UserName, Hosts, Executable, Args, Logger, Opts) ->
    Args2 = lists:map(
        fun (A) when is_atom(A) -> erlang:atom_to_list(A);
            (A) -> A
        end, Args),
    _ = pmap(
        fun ("localhost") ->
                OrigPath = os:getenv("ORIG_PATH"),
                exec_format("bash -c \"export PATH='~s'; source /etc/profile;~s ~s\"",
                    [OrigPath, Executable, string:join(Args2, " ")], Opts, Logger);
            (Host) ->
                exec_format("ssh -A -o StrictHostKeyChecking=no ~s@~s \"source /etc/profile; ~s ~s\"", 
                    [UserName, Host, Executable, string:join(Args2, " ")], Opts, Logger)
        end, Hosts).

ensure_file_content(Hosts, Content, Filepath,
                    #{user_name:= UserName, remote_dir:= RemoteRoot}, Logger) ->
    Localfile = tmp_filename(),
    Remotefile =
        case Filepath of
            "~/" ++ _ -> Filepath;
            _ -> filename:join(RemoteRoot, Filepath)
        end,
    log(Logger, debug, "Ensure file content on hosts: ~p~nLocal filename: ~p~nContent: ~s~nRemote path: ~p", [Hosts, Localfile, Content, Remotefile]),
    ok = file:write_file(Localfile, Content),
    ok = ensure_file(UserName, Hosts, Localfile, Remotefile, Logger),
    ok = file:delete(Localfile).

ensure_file(UserName, Hosts, LocalPath, RemotePath, Logger) ->
    _ = pmap(
        fun ("localhost") ->
                exec_format("cp ~s ~s", [LocalPath, RemotePath], [stderr_to_stdout], Logger);
            (Host) ->
                exec_format("scp -o StrictHostKeyChecking=no ~s ~s@~s:~s", [LocalPath, UserName, Host, RemotePath], [stderr_to_stdout], Logger)
        end, Hosts),
    ok.

exec_format(Format, Args, Opts, Logger) ->
    Handler = fun (eof, Acc) -> lists:flatten(Acc);
                  (Data, Acc) -> [Acc|Data]
              end,
    exec_format(Format, Args, Opts, Handler, [], Logger).

exec_format(Format, Args, Opts, Handler, InitState, Logger) ->
    Command = io_lib:format(Format, Args),
    log(Logger, info, "[ EXEC ] ~s", [Command]),
    Port = open_port({spawn, lists:flatten(Command)}, [stream, eof, exit_status | Opts]),
    case get_data(Port, Handler, InitState) of
        {0, Output} ->
            log(Logger, debug, "[ EXEC ] OK~nCmd output: ~s", [Output]),
            string:strip(Output, right, $\n);
        {Code, Output} ->
            log(Logger, error, "[ EXEC ] Command execution failed~nCmd: ~s~nExit code: ~p~nOutput: ~s", [Command, Code, Output]),
            erlang:error({cmd_failed, lists:flatten(Command), Code, Output})
    end.

exec_format(Format, Args, Opts, Handler, InitState, Input, Logger) ->
    File = tmp_filename(),
    ok = file:write_file(File, Input),
    try
        % Ports don't send eof to a programm but 'cat' does it
        exec_format("cat ~s | " ++ Format, [File | Args], Opts, Handler, InitState, Logger)
    after
        file:delete(File)
    end.

get_data(Port, Handler, State) ->
    receive
        {Port, {data, Bytes}} ->
            get_data(Port, Handler, Handler(Bytes, State));
        {Port, eof} ->
            Port ! {self(), close},
            get_data(Port, Handler, State);
        stop ->
            Port ! {self(), close},
            get_data(Port, Handler, State);
        {Port, closed} ->
            ExitCode =
                receive
                    {Port, {exit_status, Code}} -> Code
                end,
            {ExitCode, Handler(eof, State)}
    end.

pmap(Fun, List) ->
    Self = self(),
    Monitors = lists:map(fun (Element) ->
        Ref = erlang:make_ref(),

        Pid = erlang:spawn_link(fun () ->
                  Res = try
                      {Ref, {ok, Fun(Element)}}
                  catch
                      C:E -> {Ref, {exception, {C,E,erlang:get_stacktrace()}}}
                  end,
                  Self ! Res
              end),

        Mon = erlang:monitor(process, Pid),

        {Mon, Pid, Ref}
    end, List),
    pmap_results(Monitors, []).

pmap_results([], Res) -> lists:reverse(Res);
pmap_results([{Mon, Pid, Ref}|T], Res) ->
    receive
        {Ref, {ok, R}} ->
            erlang:demonitor(Ref, [flush]),
            pmap_results(T, [R|Res]);
        {Ref, {exception, {C,E,ST}}} ->
            erlang:raise(C, E, ST);
        {'DOWN', Mon, process, Pid, Reason} ->
            erlang:error({pmap_crash_child, Reason})
    end.

tmp_filename() ->
    {N1,N2,N3} = os:timestamp(),
    SafeNodeName = string:join(string:tokens(atom_to_list(node()), "@"), "_"),
    filename:join(["/", "tmp", io_lib:format("bench_~s_~b_~b_~b", [SafeNodeName, N1, N2, N3])]).

log(undefined, debug, F, A) -> lager:debug(F, A);
log(undefined, info, F, A)  -> lager:info(F, A);
log(undefined, error, F, A) -> lager:error(F, A);
log(Logger, Severity, F, A) -> Logger(Severity, F, A).

