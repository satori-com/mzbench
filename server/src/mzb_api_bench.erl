-module(mzb_api_bench).

-behaviour(mzb_pipeline).

-export([
    start_link/2,
    interrupt_bench/1,
    get_status/1,
    change_env/2,
    seconds/0,
    send_email_report/2,
    request_report/2,
    log_file/1,
    metrics_file/1,
    remote_path/2
]).


-ifdef(TEST).
-export([allocate_hosts/2]).
-endif.

-define(DEFLATE_FLUSH_INTERVAL, 10000).

%% mzb_pipeline callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, workflow_config/1, handle_stage/3,
         handle_pipeline_status/2]).

start_link(Id, Params) ->
    mzb_pipeline:start_link(?MODULE, [Id, Params], []).

get_status(Pid) ->
    mzb_pipeline:call(Pid, status).

change_env(Pid, Env) ->
    case mzb_pipeline:call(Pid, {change_env, Env}, 30000) of
        ok -> ok;
        {error, Reason} -> erlang:error(Reason)
    end.

interrupt_bench(Pid) ->
    mzb_pipeline:stop(Pid).

request_report(Pid, Emails) ->
    mzb_pipeline:call(Pid, {request_report, Emails}).

init([Id, Params]) ->
    Now = os:timestamp(),
    StartTime = seconds(Now),
    % Acceptance tests assume the following purpose format: bench-<id>-<timestamp>
    % {{_, M, D}, {H, Mi, S}} = calendar:now_to_universal_time(now()),
    % Purpose = mzb_string:format("bench-~2.10.0B-~2.10.0B-~2.10.0B-~2.10.0B-~2.10.0B-~b", [M,D,H,Mi,S,Id]),
    Purpose = mzb_string:format("bench-~b-~b", [Id, StartTime]),
    Includes = mzb_bc:maps_get(includes, Params, []),
    VMArgs = case maps:find(vm_args, Params) of
        {ok, [_|_] = List} -> List;
        _ -> application:get_env(mzbench_api, vm_args, undefined)
    end,
    #{name := ScriptName} = maps:get(script, Params),
    MetricPrefix = generate_graphite_prefix(mzbl_script:get_benchname(ScriptName)),
    NodeInstallSpec = extract_node_install_spec(Params),
    BenchName =
        case maps:find(benchmark_name, Params) of
            {ok, undefined} -> ScriptName;
            {ok, BN} -> BN;
            error -> ScriptName
        end,
    Config = #{
        id => Id,
        benchmark_name => BenchName,
        nodes_arg => maps:get(nodes, Params),
        script => generate_script_filename(maps:get(script, Params)),
        purpose => Purpose,
        node_install_spec => NodeInstallSpec,
        env => generate_bench_env(MetricPrefix, Params),
        deallocate_after_bench => maps:get(deallocate_after_bench, Params),
        provision_nodes => maps:get(provision_nodes, Params),
        exclusive_node_usage => maps:get(exclusive_node_usage, Params),
        req_host => maps:get(req_host, Params),
        initial_user => maps:get(user, Params),
        director_host => undefined,
        worker_hosts => [],
        emulate_bench_crash => maps:get(emulate_bench_crash, Params),
        log_file => get_env(bench_log_file),
        metrics_file => get_env(bench_metrics_file),
        log_compression => application:get_env(mzbench_api, bench_log_compression, undefined),
        metrics_compression => application:get_env(mzbench_api, bench_metrics_compression, undefined),
        vm_args => VMArgs,
        cloud => mzb_bc:maps_get(cloud, Params, undefined)
    },
    Data = #{
        includes => Includes
    },

    ok = init_data_dir(Config),
    InputBin = erlang:term_to_binary(Params),
    ok = file:write_file(local_path("params.bin", Config), InputBin),
    erlang:process_flag(trap_exit, true),

    LogFile = log_file(Config),
    LogHandler = get_file_writer(LogFile, maps:get(log_compression, Config)),

    MetricsFile = metrics_file(Config),
    MetricsHandler = get_file_writer(MetricsFile, maps:get(metrics_compression, Config)),

    State = #{
        id => Id,
        start_time => StartTime,
        finish_time => undefined,
        status => init,
        config => Config,
        data => Data,
        log_file_handler => LogHandler,
        metrics_file_handler => MetricsHandler,
        collectors => [],
        cluster_connection => undefined,
        deallocator => undefined,
        metrics => #{},
        emails => maps:get(email, Params),
        self => self() % stages are spawned, so we can't get pipeline pid from callback
    },
    info("Node repo: ~p", [NodeInstallSpec], State),
    {ok, State}.

workflow_config(_State) ->
    [{pipeline, [ init,
                  allocating_hosts,
                  provisioning,
                  uploading_script,
                  uploading_includes,
                  starting_collectors,
                  gathering_metric_names,
                  running
                ]},
     {finalize, [ saving_bench_results,
                  sending_email_report,
                  stopping_collectors,
                  cleaning_nodes,
                  deallocating_hosts
                ]}].

get_logger(State) -> fun (S, F, A) -> log(S, F, A, State) end.

handle_stage(pipeline, init, #{start_time:= StartTime, config:= Config} = State) ->
    case maps:find(emulate_bench_crash, Config) of
        {ok, true} -> exit(self(), {emulated_crash, nothing_to_see_here, please_move_along});
        _ -> ok
    end,

    info("Starting benchmark at ~b ~p", [StartTime, Config], State);

handle_stage(pipeline, allocating_hosts, #{config:= Config} = State) ->
    {Hosts, UserName, Deallocator} = allocate_hosts(Config, get_logger(State)),

    info("Allocated hosts: [~p] @ ~p", [UserName, Hosts], State),

    fun (S) ->
        S#{config => Config#{
                        user_name => UserName,
                        director_host => hd(Hosts),
                        worker_hosts => tl(Hosts) },
           deallocator => Deallocator }
    end;

handle_stage(pipeline, provisioning, #{config:= Config} = State) ->
    DirectorNode = mzb_api_provision:provision_nodes(Config, get_logger(State)),
    fun (S) -> S#{director_node => DirectorNode} end;

handle_stage(pipeline, uploading_script, #{config:= Config} = State) ->
    #{script:= Script,
      director_host:= DirectorHost,
      worker_hosts:= WorkerHosts,
      env:= Env} = Config,

    Environ = jiffy:encode({Env}),
    mzb_api_provision:ensure_file_content([DirectorHost|WorkerHosts], Environ, "environ.txt", Config, get_logger(State)),
    case Script of
        #{name := _Name, body := Body, filename := FileName} ->
            mzb_api_provision:ensure_file_content([DirectorHost|WorkerHosts], Body, FileName, Config, get_logger(State));
        _ -> ok
    end;

handle_stage(pipeline, uploading_includes, #{config:= Config, data:= Data} = State) ->
    #{includes:= Includes} = Data,
    #{director_host:= DirectorHost, worker_hosts:= WorkerHosts} = Config,
    lists:foreach(
        fun ({Name, Content}) ->
            mzb_api_provision:ensure_file_content([DirectorHost|WorkerHosts], Content, Name, Config, get_logger(State))
        end, Includes);

handle_stage(pipeline, starting_collectors, #{config:= Config, self:= Self} = State) ->
    #{director_host:= DirectorHost, worker_hosts:= WorkerHosts} = Config,
    LogFileHandler = maps:get(log_file_handler, State),
    LogsCollectors = mzb_api_connection:start_link(logs, [DirectorHost|WorkerHosts], get_env(bench_log_port),
        fun ({message, Msg}) -> LogFileHandler({write, Msg});
            (_) -> ok
        end),
    MetricsFileHandler = maps:get(metrics_file_handler, State),
    [Connection] = mzb_api_connection:start_link(management, [DirectorHost], get_env(bench_metrics_port),
        fun ({message, Msg}) ->
            case erlang:binary_to_term(Msg) of
                {metric_values, Values} -> MetricsFileHandler({write, Values});
                Any -> mzb_pipeline:cast(Self, {director_message, Any})
            end;
            ({error, _}) -> ok
        end),
    fun (S) -> S#{collectors => LogsCollectors, cluster_connection => Connection} end;

handle_stage(pipeline, gathering_metric_names, #{director_node:= DirNode, config:= Config}) ->
    #{user_name:= UserName, director_host:= DirectorHost, script:= Script} = Config,
    [RemoteScript, RemoteEnv] = [remote_path(F, Config) || F <- [script_path(Script), "environ.txt"]],
    MetricsMap = mzb_api_metrics:get_metrics(UserName, DirNode, DirectorHost, RemoteScript, RemoteEnv),
    fun (S) -> S#{metrics => MetricsMap} end;

handle_stage(pipeline, running, #{director_node:= DirNode, config:= Config} = State) ->
    #{user_name:= UserName, director_host:= DirectorHost, script:= Script} = Config,
    ScriptFilePath = script_path(Script),
    _ = mzb_subprocess:remote_cmd(UserName, [DirectorHost],
        "~/mz/mzbench/bin/run.escript",
        [DirNode] ++ [remote_path(F, Config) || F <- [ScriptFilePath, "environ.txt"]],
        get_logger(State)),
    ok;

handle_stage(finalize, saving_bench_results, #{id:= Id} = State) ->
    mzb_api_server:bench_finished(Id, status(State));

handle_stage(finalize, sending_email_report, #{emails:= Emails} = State) ->
    case send_email_report(Emails, status(State)) of
        ok -> ok;
        {error, {Error, Stacktrace}} ->
            error("Send report to ~p failed with reason: ~p~n~p", [Emails, Error, Stacktrace], State)
    end;

handle_stage(finalize, cleaning_nodes, #{config:= #{deallocate_after_bench:= false}} = State) ->
    info("Skip cleaning nodes. Deallocate after bench is false", [], State);
handle_stage(finalize, cleaning_nodes,
    State = #{director_node:= DirNode, config:= Config = #{director_host:= DirectorHost}})
      when DirectorHost /= undefined ->

    NeedToStopNodes = case DirNode of
        undefined -> dont_need_to_stop_nodes;
        _ -> need_to_stop_nodes
    end,
    mzb_api_provision:clean_nodes(Config, get_logger(State), NeedToStopNodes);
handle_stage(finalize, cleaning_nodes, State) ->
    info("Skip cleaning nodes. Unknown nodes", [], State);

handle_stage(finalize, deallocating_hosts, #{config:= #{deallocate_after_bench:= false}} = State) ->
    info("Skip deallocation. Deallocate after bench is false", [], State);
handle_stage(finalize, deallocating_hosts, #{deallocator:= undefined} = State) ->
    info("Skip deallocation. Undefined deallocator.", [], State);
handle_stage(finalize, deallocating_hosts, #{deallocator:= Deallocator} = State) ->
    DeallocWrapper = fun () ->
        try
            info("Deallocator has started", [], State),
            Deallocator(),
            ok
        catch _C:E ->
            ST = erlang:get_stacktrace(),
            error("Deallocation has failed with reason: ~p~nStacktrace: ~p", [E, ST], State),
            retry
        end
    end,
    run_periodically(seconds(), 3 * 60, 10, DeallocWrapper);

handle_stage(finalize, stopping_collectors,
            #{collectors:= Collectors,
              cluster_connection:= ClusterConnection}) ->
    Cons = [ClusterConnection|Collectors],
    _ = [catch mzb_api_connection:send_message(C, close_req) || C <- Cons, C /= undefined],
    _ = [mzb_api_connection:wait_close(C, 30000) || C <- Cons, C /= undefined].

handle_call(status, _From, State) ->
    {reply, status(State), State};

handle_call({request_report, Emails}, _, #{emails:= OldEmails} = State) ->
    {reply, ok, State#{emails:= OldEmails ++ Emails}};

handle_call({change_env, Env}, From, #{status:= running, cluster_connection:= Connection} = State) ->
    info("Change env req received: ~p", [Env], State),
    mzb_api_connection:send_message(Connection,
        {change_env, Env, fun (Res) -> mzb_pipeline:reply(From, Res) end}),
    {noreply, State};

handle_call({change_env, _Env}, _From, #{} = State) ->
    {reply, {error, not_running}, State};

handle_call(_Request, _From, State) ->
    error("Unhandled call: ~p", [_Request], State),
    {noreply, State}.

handle_cast({director_message, {change_env_res, Res, Continuation}}, State) ->
    info("Received change_env response: ~p", [Res], State),
    Continuation(Res),
    {noreply, State};

handle_cast({director_message, Unknown}, State) ->
    lager:error("Unknown director message ~p", [Unknown]),
    {noreply, State};

handle_cast(_Msg, State) ->
    error("Unhandled cast: ~p", [_Msg], State),
    {noreply, State}.

handle_info({'EXIT', _, normal}, State) ->
    {noreply, State};

handle_info({'EXIT', P, Reason}, State) ->
    error("Benchmark received 'EXIT' from ~p with reason ~p, stopping", [P, Reason], State),
    {stop, Reason, State};

handle_info(_Info, State) ->
    error("Unhandled info: ~p", [_Info], State),
    {noreply, State}.


terminate(normal, State) ->
    catch (maps:get(log_file_handler, State))(close),
    catch (maps:get(metrics_file_handler, State))(close);
% something is going wrong there. use special status for bench and run finalize stages again
terminate(Reason, #{id:= Id} = State) ->
    error("Receive terminate while finalize is not completed: ~p", [Reason], State),
    mzb_api_server:bench_finished(Id, status(State)),
    catch (maps:get(log_file_handler, State))(close),
    catch (maps:get(metrics_file_handler, State))(close),
    spawn(
      fun() ->
          {ok, Timer} = timer:kill_after(5 * 60 * 1000),

          Stages = proplists:get_value(finalize, workflow_config(State), []),
          NewState = handle_pipeline_status({final, failed}, State),

          lists:foreach(fun (Stage) ->
                            try
                                handle_stage(finalize, Stage, NewState)
                            catch
                                _C:E ->
                                    ST = erlang:get_stacktrace(),
                                    error("Stage 'finalize - ~s': failed~n~s", [Stage, format_error(Stage, {E, ST})], NewState)
                            end
                        end, Stages),

          timer:cancel(Timer)
      end),
    ok.

handle_pipeline_status(Info, State) ->
    NewState = handle_pipeline_status_ll(Info, State),
    mzb_api_firehose:update_bench(status(NewState)),
    NewState.

handle_pipeline_status_ll({start, Phase, Stage}, State) ->
    info("Stage '~s - ~s': started", [Phase, Stage], State),
    case Phase of
        pipeline -> State#{status => Stage};
        _ -> State
    end;
handle_pipeline_status_ll({complete, Phase, Stage}, State) ->
    info("Stage '~s - ~s': finished", [Phase, Stage], State),
    State;
handle_pipeline_status_ll({exception, Phase, Stage, E, ST}, State) ->
    error("Stage '~s - ~s': failed~n~s", [Phase, Stage, format_error(Stage, {E, ST})], State),
    State;
handle_pipeline_status_ll({final, Final}, State) ->
    info("Bench final: ~s", [Final], State),
    State#{status => Final, finish_time => seconds()}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

extract_node_install_spec(Params) ->
    Get = fun (N) ->
            case maps:find(N, Params) of
                {ok, Value} when Value /= undefined -> Value;
                _ -> application:get_env(mzbench_api, N, undefined)
            end
          end,

    % BC CODE:
        case Get(mzbench_git) of
            undefined -> ok;
            _ -> lager:error("mzbench_git param is obsolete, use node_git instead"),
                 erlang:error(mzbench_git_is_obsolete)
        end,
        case Get(mzbench_rsync) of
            undefined -> ok;
            _ -> lager:error("mzbench_rsync param is obsolete, use node_rsync instead"),
                 erlang:error(mzbench_rsync_is_obsolete)
        end,
    % END OF BC CODE

    case Get(node_rsync) of
        undefined ->
            GitRepo = Get(node_git),
            GitBranch =
                case Get(node_commit) of
                    undefined ->
                        {ok, GitRev} = application:get_key(mzbench_api, vsn),
                        GitRev;
                    B -> B
                end,
            mzbl_script:make_git_install_spec(GitRepo, GitBranch, "node");
        Remote ->
            mzbl_script:make_rsync_install_spec(Remote, "node", ["deps", "ebin", ".make"])
    end.

send_email_report(Emails, #{id:= Id,
                            status:= Status,
                            config:= Config,
                            start_time:= StartTime,
                            finish_time:= FinishTime,
                            metrics:= MetricsMap}) ->
    try
        #{metrics_file:= MetricsFile} = Config,
        BenchTime = FinishTime - StartTime,
        Links = mzb_api_metrics:get_graphite_image_links(MetricsMap, BenchTime),
        lager:info("Metrics links: ~p", [Links]),
        AttachFiles = download_images("graphite_", Links, Config),
        {Subj, Body} = generate_mail_body(Id, Status, Links, Config),
        lager:info("EMail report: ~n~s~n~s~n", [Subj, Body]),
        Attachments = lists:map(
            fun (F) ->
                {ok, Bin} = file:read_file(local_path(F, Config)),
                Filename = filename:basename(F),
                {list_to_binary(Filename), <<"image/png">>, Bin}
            end, [MetricsFile|AttachFiles]),
        lists:foreach(
            fun (Addr) ->
                lager:info("Sending bench results to ~s", [Addr]),
                BAddr = list_to_binary(Addr),
                mzb_api_mail:send(BAddr, Subj, Body, Attachments, application:get_env(mzbench_api, mail, []))
            end, Emails),
        ok
    catch
        _:Error ->
            {error, {Error, erlang:get_stacktrace()}}
    end;
send_email_report(_Emails, Status) ->
    {error, {badarg, Status}}.

status(State) ->
    mzb_bc:maps_with([id, status, start_time, finish_time, config, metrics], State).

add_env([], Env) -> Env;
add_env([H | T], Env) ->
    case application:get_env(H) of
        undefined -> add_env(T, Env);
        {ok, V} -> [{list_to_binary(atom_to_list(H)), list_to_binary(V)} | add_env(T, Env)]
    end.

generate_bench_env(MetricPrefix, Params) ->
    Env = maps:get(env, Params),
    Script = maps:get(script, Params),
    #{name := ScriptName} = Script,
    Env2 = lists:foldl(fun ({K, V}, E) ->
                        case proplists:get_value(K, E) of
                            undefined -> [{K, V}|E];
                            _ -> E
                        end
                       end, Env,
                [{<<"mzb_script_name">>, list_to_binary(ScriptName)},
                 {<<"graphite_prefix">>, list_to_binary(MetricPrefix)}]),

    case proplists:get_value(<<"graphite">>, Env2) of
        undefined -> add_env([graphite, graphite_api_key, graphite_url], Env2);
        _H -> Env2
    end.

script_path(Script) ->
    case Script of
        #{filename := FileName} -> FileName;
        Pkg -> "~/mz/mzbench_workers/" ++ Pkg ++ "/default.erl"
    end.

run_periodically(StartTime, MaxTime, RetryTimeout, Fn) ->
    case Fn() of 
        ok -> ok;
        retry ->
            TimeSinceStart = seconds() -  StartTime,
            case TimeSinceStart =< MaxTime of
                true ->
                    timer:sleep(RetryTimeout),
                    run_periodically(StartTime, MaxTime, RetryTimeout, Fn);
                _ ->
                    erlang:error(max_time_reached)
            end
    end.

allocate_hosts(#{nodes_arg:= N, cloud:= Cloud} = Config, Logger) when is_integer(N), N > 0 ->
    #{purpose:= Purpose,
      initial_user:= User,
      exclusive_node_usage:= Exclusive} = Config,
    Description = mzb_string:format("MZ-Bench cluster:~n~p", [Config]),
    ClusterConfig = #{
        purpose => Purpose,
        user => User,
        description => Description,
        exclusive_node_usage => Exclusive
    },
    % Allocate one supplementary node for the director
    Logger(info, "Allocating ~p hosts in ~p cloud...", [N + 1, Cloud]),
    {ok, ClusterId, UserName, Hosts} = mzb_api_cloud:create_cluster(Cloud, N + 1, ClusterConfig),
    Deallocator =
        fun () ->
            mzb_api_cloud:destroy_cluster(ClusterId)
        end,
    {Hosts, UserName, Deallocator};


allocate_hosts(#{nodes_arg:= [HostsStr]}, _Logger) when is_list(HostsStr) ->
    URIs = string:tokens(HostsStr, ","),

    {Users, Hosts} = lists:foldr(fun (URI, {Users, Hosts}) ->
                                     case string:tokens(URI, "@") of
                                         [User, Host] ->
                                             {[User | Users], [Host | Hosts]};
                                         [Host] ->
                                             {Users, [Host | Hosts]};
                                         _ -> erlang:error({incorrect_hostname, URI})
                                     end
                                 end,
                                 {[], []},
                                 URIs),

    UserName = case lists:usort(Users) of
        [] -> "root";
        [User] -> User;
        Xs -> erlang:error({different_users_for_hosts, Xs})
    end,

    if erlang:length(Hosts) >= 2 ->
            {Hosts, UserName, undefined};
        true -> 
            erlang:error(not_enough_nodes)
    end.

seconds() -> seconds(os:timestamp()).

seconds({N1, N2, _N3}) ->
    N1*1000000 + N2.

log_file(Config = #{log_file:= File}) ->
    local_path(File, Config).

metrics_file(Config = #{metrics_file:= File}) ->
    local_path(File, Config).

remote_path(RelPath, #{purpose:= Purpose}) ->
    filename:join(["/", "tmp", "mz", Purpose, RelPath]).

local_path(RelPath, #{id:= Id}) ->
    DataDir = mzb_api_server:server_data_dir(),
    filename:join([DataDir, integer_to_list(Id), RelPath]).

init_data_dir(Config) ->
    BenchDataDir = local_path("", Config),
    case filelib:is_file(BenchDataDir) of
        false -> ok;
        true  -> erlang:error({data_dir_already_exists, BenchDataDir})
    end,
    case filelib:ensure_dir(filename:join(BenchDataDir, ".")) of
        ok -> ok;
        {error, Reason} -> erlang:error({ensure_dir_error, BenchDataDir, Reason})
    end.

generate_mail_body(Id, Status, Links, Config) ->
    #{env:= Env, script:= Script} = Config,
    #{name := ScriptName, body := ScriptBody} = Script,
    Subject = io_lib:format("Bench report for ~s (~s)", [ScriptName, Status]),
    Chars = io_lib:format(
        "Status: ~s~n~n"
        "Environment:~n~s~n~n"
        "Script body:~n~s~n~n"
        "Benchmark logs:~n  ~s~n~n"
        "Metrics data:~n  ~s~n~n"
        "Graphite links for reference:~n~s~n",
        [Status,
         indent(string:join([io_lib:format("~s = ~s", [K,V]) || {K,V} <- Env], "\n"), 2, "(no env variables)"),
         indent(ScriptBody, 2),
         bench_log_link(Id, Config),
         bench_data_link(Id, Config),
         indent(string:join(Links, "\n"), 2, "(no links available)")
         ]),
    {list_to_binary(Subject), list_to_binary(Chars)}.

bench_data_link(Id, #{req_host:= ServerAddr}) ->
    io_lib:format("http://~s/data?id=~b", [ServerAddr, Id]).

bench_log_link(Id, #{req_host:= ServerAddr}) ->
    io_lib:format("http://~s/logs?id=~b", [ServerAddr, Id]).

indent("", N, Default) -> indent(Default, N);
indent(Str, N, _) -> indent(Str, N).

indent(Binary, N) when is_binary(Binary) ->
    erlang:list_to_binary(indent(erlang:binary_to_list(Binary), N));
indent(Str, N) ->
    Spaces = [$\s || _ <- lists:seq(1, N)],
    string:join([Spaces ++ Line || Line <- string:tokens(Str, "\n")], "\n").

download_images(Prefix, URLs, Config) ->
    Files = mzb_lists:pmap(fun ({N, URL}) ->
        FileName = Prefix ++ integer_to_list(N) ++ ".png",
        FullPath = local_path(FileName, Config),
        _ = ensure_URL_dowloaded(URL, FullPath),
        FileName
    end, mzb_lists:enumerate(URLs)),
    [F || F <- Files, filelib:is_file(local_path(F, Config))].

ensure_URL_dowloaded(URL, ToFile) ->
    case filelib:is_file(ToFile) of
        false ->
            case httpc:request(get, {URL, []}, [{timeout, 5000}], []) of
                {ok, {_, _, Data}} ->
                    TmpFile = mzb_file:tmp_filename(),
                    ok = file:write_file(TmpFile, Data),
                    ok = file:rename(TmpFile, ToFile),
                    lager:info("Downloaded: ~s -> ~s", [URL, ToFile]),
                    ok;
                {error, Reason} ->
                    lager:error("Download failed: ~s with reason: ~p", [URL, Reason]),
                    {error, Reason}
            end;
        true ->
            lager:info("File ~s is already downloaded", [ToFile]),
            ok
    end.

info(Format, Args, State) ->
    log(info, Format, Args, State).

error(Format, Args, State) ->
    log(error, Format, Args, State).

log(Severity, Format, Args, #{log_file_handler:= H, id:= Id}) ->
    Format2 = "[ BENCH #~b ] " ++ Format,
    Args2 = [Id|Args],
    DefaultLogger = mzb_api_app:default_logger(),
    DefaultLogger(Severity, Format2, Args2),
    format_log(H, Severity, Format, Args).

format_log(_Handler, debug, _Format, _Args) -> ok;
format_log(Handler, Severity, Format, Args) ->
    Now = {_, _, Ms} = os:timestamp(),
    {_, {H,M,S}} = calendar:now_to_universal_time(Now),
    _ = Handler({write, io_lib:format("~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0B [~s] [ API ] " ++ Format ++ "~n", [H, M, S, Ms div 1000, Severity|Args])}),
    ok.

format_error(_, {{cmd_failed, Cmd, Code, Output}, _}) ->
    io_lib:format("Command returned ~b:~n ~s~nCommand output: ~s", [Code, Cmd, Output]);
format_error(Op, {E, Stack}) ->
    io_lib:format("Benchmark has failed on ~p with reason:~n~p~n~nStacktrace: ~p", [Op, E, Stack]).

get_env(K) -> application:get_env(mzbench_api, K, undefined).

generate_script_filename(#{name := _Name, body := Body} = Script) ->
    Name = lists:flatten(lists:map(
        fun(Num) -> erlang:integer_to_list(Num, 16) end,
        erlang:binary_to_list(crypto:hash(sha, Body))
    )),
    Script#{filename => mzb_string:format("~s.erl", [Name])}.

get_file_writer(Filename, none) ->
    {ok, H} = file:open(Filename, [write]),
    fun (close) -> file:close(H);
        ({write, Data}) -> file:write(H, Data)
    end;
get_file_writer(Filename, deflate) ->
    P = erlang:spawn_link(fun () -> deflate_process(Filename) end),
    fun (close) ->
            Ref = erlang:monitor(process, P),
            P ! close,
            receive
                {'DOWN', Ref, _, _, _} -> ok
            end;
        ({write, Data}) -> P ! {write, Data}, ok
    end.

deflate_process(Filename) ->
    {ok, Ref} = timer:send_interval(?DEFLATE_FLUSH_INTERVAL, flush),
    {ok, H} = file:open(Filename, [write]),
    Z = zlib:open(),
    ok = zlib:deflateInit(Z),
    erlang:process_flag(trap_exit, true),
    Flush = fun () ->
        try
            file:write(H, zlib:deflate(Z, <<>>, sync))
        catch
            % If there is no data to compress in internal deflate buffer
            % it throws buf_error for some reason
            error:buf_error -> ok
        end
    end,
    Close = fun () ->
        _ = Flush(),
        _ = file:write(H, zlib:deflate(Z, <<>>, finish)),
        zlib:deflateEnd(Z),
        zlib:close(Z),
        _ = timer:cancel(Ref),
        file:close(H)
    end,
    fun D() ->
        receive
            {'EXIT', _, _} -> Close();
            close -> Close();
            flush -> _ = Flush(), D();
            {write, Data} ->
                _ = file:write(H, zlib:deflate(Z, Data, none)),
                D()
        end
    end ().

generate_graphite_prefix(BenchName) ->
    Threshold = application:get_env(mzbench_api, graphite_prefixes_num, undefined),
    _ = ets:insert_new(graphite_prefixes, {BenchName, -1}),
    N = ets:update_counter(graphite_prefixes, BenchName, {2, 1, Threshold - 1, 0}),
    mzb_string:format("~s.~b", [BenchName, N]).

