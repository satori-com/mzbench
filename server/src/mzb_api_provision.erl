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

provision_nodes(Config, Logger) ->
    #{
        dont_provision_nodes := DontProvisionNodes,
        director_host := DirectorHost,
        worker_hosts := WorkerHosts,
        user_name := UserName,
        remote_dir := RootDir,
        node_git := GitRepo,
        node_commit := GitCommit,
        script := Script
    } = Config,
    
    UniqHosts = lists:usort([DirectorHost|WorkerHosts]),
    log(Logger, info, "Provisioning nodes: ~p~nWith config: ~p", [UniqHosts, Config]),
    _ = remote_cmd(UserName, UniqHosts, io_lib:format("mkdir -p ~s", [RootDir]), [], Logger),
    
    CheckResult = (catch ntp_check(UserName, UniqHosts, Logger)),
    log(Logger, info, "NTP check result: ~p", [CheckResult]),
    
    catch remote_cmd(UserName, UniqHosts, "~/mz/mz_bench/bin/mz_bench stop; true", [], Logger),
    
    case DontProvisionNodes of
        false ->
            git_install(UserName, UniqHosts, GitRepo, GitCommit, "node", Logger),
            ensure_worker_from_repo(UserName, UniqHosts, Script, Logger),
            ensure_cookie(UserName, UniqHosts, Config, Logger);
        _ -> ok
    end,
    
    ensure_vm_args(DirectorHost, WorkerHosts, Config, Logger),
    _ = remote_cmd(UserName, [DirectorHost|WorkerHosts], io_lib:format("cd ~s && ~~/mz/mz_bench/bin/mz_bench start", [RootDir]), [], Logger),
    [DirNode] = [nodename(director_sname(Config), H) || H <- get_hostnames(UserName, [DirectorHost], Logger)],
    WorkerNodes = [nodename(worker_sname(Config), H) || H <- get_hostnames(UserName, WorkerHosts, Logger)],
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

ensure_worker_from_repo(UserName, Hosts, Script, Logger) ->
   case Script of
        #{ body := Body } ->
            Items = parse_script(binary_to_list(Body)),
            _ = [ git_install(
                UserName,
                Hosts,
                case proplists:get_value(git, Opts) of
                    undefined -> erlang:error({make_install_error, "Could't find 'git' atom"});
                    Value -> Value
                end,
                proplists:get_value(branch, Opts, ""),
                proplists:get_value(dir, Opts, ""), Logger)
            || {make_install, Opts} <- Items],
            ok;
        _ -> ok
    end.

parse_script(Body) ->
    case erl_scan:string(Body) of
        {ok, [], _} -> [];
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} ->
                    Term;
                {error, Error} ->
                    erlang:error({parse_error, Error})
            end;
        {error, Error, _} ->
            erlang:error({parse_error, Error})
    end.

ensure_cookie(UserName, Hosts, #{purpose:= Cookie} = Config, Logger) ->
    CookieFile = "~/.erlang.cookie",
    NotLocalhosts = [H || H <- Hosts, H =/= "localhost"],
    ensure_file_content(NotLocalhosts, Cookie, CookieFile, Config, Logger),
    _ = remote_cmd(UserName, NotLocalhosts, "chmod", ["go-rwx", CookieFile], Logger),
    ok.

ensure_vm_args(Director, Workers, Config, Logger) ->
    DirVmArgs = vm_args_content(director_sname(Config)),
    ensure_file_content([Director], DirVmArgs, "vm.args", Config, Logger),
    WorkerVmArgs = vm_args_content(worker_sname(Config)),
    ensure_file_content(Workers, WorkerVmArgs, "vm.args", Config, Logger).

director_sname(#{id:= Id}) -> "mzb_director" ++ integer_to_list(Id).
worker_sname(#{id:= Id})   -> "mzb_worker" ++ integer_to_list(Id).

vm_args_content(NodeName) ->
    io_lib:format("-sname ~s~n", [NodeName]).

git_install(UserName, Hosts, GitRepo, GitBranch, GitSubDir, Logger) ->
    Branch =
        case GitBranch of
            undefined -> "master";
            "" -> "master"; % FIXME: default value should be master
            _ -> GitBranch
        end,
    SubDir =
        case GitSubDir of
            undefined -> ".";
            "" -> "."; % FIXME: default value should be "."
            _ -> GitSubDir
        end,
    DeploymentDirectory = tmp_filename(),
    ProvisionCmd = io_lib:format("mkdir ~s && cd ~s && git clone ~s deployment_code && "
                                    "cd deployment_code && git checkout ~s && "
                                    "cd ~s && make install && cd ../../.. && "
                                    "rm -rf ~s",
                                    [DeploymentDirectory, DeploymentDirectory, GitRepo, Branch, SubDir, DeploymentDirectory]),
    _ = remote_cmd(UserName, Hosts, ProvisionCmd, [], Logger).

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
                exec_format("sh -c \"export PATH='~s'; source /etc/profile;~s ~s\"",
                    [OrigPath, Executable, string:join(Args2, " ")], Opts, Logger);
            (Host) ->
                exec_format("ssh -A -o StrictHostKeyChecking=no ~s@~s \"source /etc/profile; ~s ~s\"", 
                    [UserName, Host, Executable, string:join(Args2, " ")], Opts, Logger)
        end, Hosts).

ensure_file_content(Hosts, Content, Filepath,
                    #{user_name:= UserName, local_dir:= LocalRoot, remote_dir:= RemoteRoot}, Logger) ->
    Filename = filename:basename(Filepath),
    Localfile = filename:join(LocalRoot, Filename),
    Remotefile =
        case Filepath of
            "~/" ++ _ -> Filepath;
            _ -> filename:join(RemoteRoot, Filepath)
        end,
    log(Logger, debug, "Ensure file content on hosts: ~p~nLocal filename: ~p~nContent: ~s~nRemote path: ~p", [Hosts, Localfile, Content, Remotefile]),
    ok = file:write_file(Localfile, Content),
    ok = ensure_file(UserName, Hosts, Localfile, Remotefile, Logger).

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
        {_, Mon} = erlang:spawn_monitor(fun () ->
            Res = try
                {Ref, {ok, Fun(Element)}}
            catch
                C:E -> {Ref, {exception, {C,E,erlang:get_stacktrace()}}}
            end,
            Self ! Res
        end),
        {Mon, Ref}
    end, List),
    pmap_results(Monitors, []).

pmap_results([], Res) -> lists:reverse(Res);
pmap_results([{Mon, Ref}|T], Res) ->
    receive
        {Ref, {ok, R}} ->
            erlang:demonitor(Ref, [flush]),
            pmap_results(T, [R|Res]);
        {Ref, {exception, {C,E,ST}}} ->
            erlang:raise(C, E, ST);
        {'DOWN', Mon, process, _, Reason} ->
            erlang:error({pmap_crash_child, Reason})
    end.

tmp_filename() ->
    {N1,N2,N3} = os:timestamp(),
    filename:join(["/", "tmp", io_lib:format("bench_~s_~b_~b_~b", [node(), N1, N2, N3])]).

log(undefined, debug, F, A) -> lager:debug(F, A);
log(undefined, info, F, A)  -> lager:info(F, A);
log(undefined, error, F, A) -> lager:error(F, A);
log(Logger, Severity, F, A) -> Logger(Severity, F, A).

