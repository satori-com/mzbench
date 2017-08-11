-module(mzb_api_bench).

-behaviour(mzb_pipeline).

-export([
    start_link/2,
    interrupt_bench/1,
    get_status/1,
    change_env/2,
    run_command/4,
    seconds/0,
    send_email_report/2,
    request_report/2,
    log_file/1,
    log_user_file/1,
    metrics_file/2,
    remote_path/2,
    update_name/2,
    add_tags/2,
    remove_tags/2
]).


-ifdef(TEST).
-export([allocate_hosts/2]).
-endif.

-define(DEFLATE_FLUSH_INTERVAL, 10000).
-define(DIRECTOR_CALL_TIMEOUT, 30000).
-define(HOOKS_TIMEOUT, 60000).

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

interrupt_bench(Pid) -> mzb_pipeline:stop(Pid).

run_command(Pid, Pool, Percent, Command) ->
    case mzb_pipeline:call(Pid, {run_command, Pool, Percent, Command}, 30000) of
        ok -> ok;
        {error, Reason} -> erlang:error(Reason)
    end.

request_report(Pid, Emails) ->
    mzb_pipeline:call(Pid, {request_report, Emails}).

update_name(Pid, NewName) ->
    mzb_pipeline:call(Pid, {update_name, NewName}).

add_tags(Pid, Tags) ->
    mzb_pipeline:call(Pid, {add_tags, Tags}).

remove_tags(Pid, Tags) ->
    mzb_pipeline:call(Pid, {remove_tags, Tags}).

init([Id, Params]) ->
    CreateTime = seconds(),
    % Acceptance tests assume the following purpose format: bench-<id>-<timestamp>
    % {{_, M, D}, {H, Mi, S}} = calendar:now_to_universal_time(now()),
    % Purpose = mzb_string:format("bench-~2.10.0B-~2.10.0B-~2.10.0B-~2.10.0B-~2.10.0B-~b", [M,D,H,Mi,S,Id]),
    Purpose = mzb_string:format("bench-~b-~b", [Id, CreateTime]),
    Includes = mzb_bc:maps_get(includes, Params, []),
    VMArgs = case maps:find(vm_args, Params) of
        {ok, [_|_] = List} -> List;
        _ -> application:get_env(mzbench_api, vm_args, undefined)
    end,
    #{name := ScriptName} = maps:get(script, Params),
    NodeInstallSpec = extract_node_install_spec(Params),
    BenchName =
        case maps:find(benchmark_name, Params) of
            {ok, undefined} -> ScriptName;
            {ok, BN} -> BN;
            error -> ScriptName
        end,
    Cloud = case mzb_bc:maps_get(cloud, Params, undefined) of
                undefined -> [DefaultCloud | _] = mzb_api_cloud:list_clouds(),
                             DefaultCloud;
                Cl -> Cl
            end,

    Tags = mzb_bc:maps_get(tags, Params, []),

    Config = #{
        id => Id,
        parent => mzb_bc:maps_get(parent, Params, undefined),
        author => mzb_bc:maps_get(author, Params, "anonymous"),
        author_name => mzb_bc:maps_get(author_name, Params, ""),
        benchmark_name => BenchName,
        nodes_arg => maps:get(nodes, Params),
        exclusive => maps:get(exclusive, Params),
        script => generate_script_filename(maps:get(script, Params)),
        purpose => Purpose,
        node_install_spec => NodeInstallSpec,
        env => mzbl_script:normalize_env(generate_bench_env(Id, Params)),
        deallocate_after_bench => maps:get(deallocate_after_bench, Params),
        provision_nodes => maps:get(provision_nodes, Params),
        req_host => maps:get(req_host, Params),
        initial_user => maps:get(user, Params),
        director_host => undefined,
        worker_hosts => [],
        emulate_bench_crash => maps:get(emulate_bench_crash, Params),
        log_file => get_env(bench_log_file),
        log_user_file => get_env(bench_log_user_file),
        metrics_file => get_env(bench_metrics_file),
        log_compression => application:get_env(mzbench_api, bench_log_compression, undefined),
        metrics_compression => application:get_env(mzbench_api, bench_metrics_compression, undefined),
        vm_args => VMArgs,
        cloud => Cloud,
        node_log_port => application:get_env(mzbench_api, node_log_port, undefined),
        node_log_user_port => application:get_env(mzbench_api, node_log_user_port, undefined),
        node_interconnect_port => application:get_env(mzbench_api, node_interconnect_port, undefined),
        metric_update_interval_ms => extract_metric_update_interval(Params),
        node_management_port => application:get_env(mzbench_api, node_management_port, undefined),
        tags => Tags
    },
    Data = #{
        includes => [ {File, erlang:iolist_size(Data), Data} || {File, Data} <- Includes ]
    },

    ok = init_data_dir(Config),
    InputBin = erlang:term_to_binary(Params),
    ok = file:write_file(local_path("params.bin", Config), InputBin),
    erlang:process_flag(trap_exit, true),

    LogFile = log_file(Config),
    LogHandler = get_file_writer(LogFile, maps:get(log_compression, Config)),

    LogUserFile = log_user_file(Config),
    LogUserHandler = get_file_writer(LogUserFile, maps:get(log_compression, Config)),

    State = #{
        id => Id,
        create_time => CreateTime,
        start_time => undefined,
        finish_time => undefined,
        status => init,
        config => Config,
        data => Data,
        log_file_handler => LogHandler,
        log_user_file_handler => LogUserHandler,
        collectors => [],
        interconnect_ports => [],
        node_pids => [],
        cluster_connection => undefined,
        deallocator => undefined,
        metrics => #{},
        emails => maps:get(email, Params),
        self => self(), % stages are spawned, so we can't get pipeline pid from callback
        director_node => undefined,
        previous_status => undefined,
        results => undefined,
        result_str => "",
        user_errors => 0,
        system_errors => 0
    },
    info("Node repo: ~p", [NodeInstallSpec], State),
    {ok, State}.

workflow_config(_State) ->
    [{pipeline, [ init,
                  checking_script,
                  wait_exclusive,
                  allocating_hosts,
                  provisioning,
                  connect_nodes,
                  starting_collectors,
                  uploading_script,
                  uploading_includes,
                  pre_hooks,
                  starting,
                  running,
                  post_hooks
                ]},
     {finalize, [ saving_bench_results,
                  sending_email_report,
                  stopping_collectors,
                  cleaning_nodes,
                  deallocating_hosts,
                  release_exclusive
                ]},
     {unstoppable, [allocating_hosts]}].

get_logger(State) -> fun (S, F, A) -> log(S, F, A, State) end.

handle_stage(pipeline, init, #{config:= Config} = State) ->
    case maps:find(emulate_bench_crash, Config) of
        {ok, true} -> exit(self(), {emulated_crash, nothing_to_see_here, please_move_along});
        _ -> ok
    end,

    info("Starting benchmark~n~p", [Config], State);

handle_stage(pipeline, checking_script, #{config:= Config}) ->
    #{script:= #{body:= ScriptBody}} = Config,
    AST = mzbl_script:read_from_string(binary_to_list(ScriptBody)),
    case mzbl_typecheck:check(AST, list) of
        {false, Reason, Location} ->
            ResStr = mzbl_typecheck:format_error(Reason),
            mzb_pipeline:error({type_error, Reason, Location},
                               fun (S) -> S#{result_str => ResStr} end);
        _ -> ok
    end,
    fun (S) -> S end;

handle_stage(pipeline, wait_exclusive, #{id:= Id, config:= Config}) ->
    Exclusive = mzb_bc:maps_get(exclusive, Config, []),
    mzb_api_exclusive:lock(Id, Exclusive),
    fun (S) -> S#{start_time => seconds()} end;

handle_stage(pipeline, allocating_hosts, #{config:= Config} = State) ->
    {Hosts, UserName, Deallocator} = allocate_hosts(Config, get_logger(State)),

    if length(Hosts) > 1 -> info("Allocated hosts: [~p] @~n~p", [UserName, Hosts], State);
       true -> info("Allocated hosts: [~p] @ ~p", [UserName, Hosts], State)
    end,

    fun (S) ->
        S#{config => Config#{
                        user_name => UserName,
                        director_host => hd(Hosts),
                        worker_hosts => tl(Hosts) },
           deallocator => Deallocator }
    end;

handle_stage(pipeline, provisioning, #{config:= Config, self:= Self} = State) ->
    {[{DirectorNode, _}|_] = Nodes, InterconnectPorts, NodePids, Port} = mzb_api_provision:provision_nodes(Config, get_logger(State)),
    #{director_host:= DirectorHost} = Config,
    Connection = mzb_api_connection:start_and_link_with(
                    Self, management, DirectorHost, Port,
                    fun (Msg, S) -> handle_management_msg(Msg, Self, S) end,
                    #{config => Config, handlers => #{}}),
    fun (S) -> S#{director_node => DirectorNode, cluster_connection => Connection,
                  nodes => Nodes, interconnect_ports => InterconnectPorts,
                  node_pids => NodePids} end;

handle_stage(pipeline, connect_nodes, #{cluster_connection:= Connection, config:= Config,
                                        interconnect_ports:= InterconnectPorts}) ->
    #{worker_hosts:= WorkerHosts} = Config,
    case director_call(Connection, {connect_nodes, lists:zip(WorkerHosts, InterconnectPorts)}) of
        ok -> ok;
        {error, Error} ->
            mzb_pipeline:error({connect_cluster_error, Error},
                               fun (S) -> S#{result_str => "Cluster connect problem"} end)
    end;

handle_stage(pipeline, uploading_script, #{config:= Config} = State) ->
    #{script:= Script,
      director_host:= DirectorHost,
      worker_hosts:= WorkerHosts} = Config,

    case Script of
        #{name := _Name, body := Body, filename := FileName} ->
            mzb_api_provision:ensure_file_content([DirectorHost|WorkerHosts], Body, FileName, Config, get_logger(State));
        _ -> ok
    end;

handle_stage(pipeline, uploading_includes, #{config:= Config, data:= Data} = State) ->
    #{includes:= Includes} = Data,
    #{director_host:= DirectorHost, worker_hosts:= WorkerHosts} = Config,
    lists:foreach(
        fun ({Name, _, Content}) ->
            mzb_api_provision:ensure_file_content([DirectorHost|WorkerHosts], Content, Name, Config, get_logger(State))
        end, Includes);

handle_stage(pipeline, starting_collectors, #{cluster_connection:= Connection, nodes:= Nodes, self:= Self} = State) ->
    StartFun = fun (Call, Handler) ->
        mzb_lists:pmap(fun ({Node, Host}) ->
        {ok, Port} = director_call(Connection, {Call, Node}),
        info("Log collector server: ~p -> ~p:~p", [Node, Host, Port], State),
        mzb_api_connection:start_and_link_with(Self, logs, Host, Port,
            fun ({message, Msg}, S) -> {Handler({write, Msg}), S};
                (_, S) -> {ok, S}
            end, [])
        end, Nodes) end,
    LogsCollectors = StartFun(get_log_port, maps:get(log_file_handler, State)) ++
            StartFun(get_log_user_port, maps:get(log_user_file_handler, State)),
    fun (S) -> S#{collectors => LogsCollectors} end;

handle_stage(pipeline, pre_hooks, #{cluster_connection:= Connection, config:= Config} = State) ->
    #{script:= Script, env:= DefaultEnv, director_host:= DirectorHost, worker_hosts:= WorkerHosts} = Config,
    ScriptFilePath = script_path(Script),
    DirFun = fun (Msg) -> director_call(Connection, Msg, ?HOOKS_TIMEOUT) end,
    {Body, Env} =
        try
            DirFun({read_and_validate, remote_path(ScriptFilePath, Config), mzbl_script:normalize_env(DefaultEnv)})
        catch
            error:{error, List} ->
                ResStr = string:join(List, "\n"),
                error("~s", [ResStr], State),
                mzb_pipeline:error(validation_failed,
                                   fun (S) -> S#{result_str => ResStr} end)

        end,
    ActualWorkerHosts = case WorkerHosts of
        [] -> [DirectorHost];
        _ -> WorkerHosts
    end,
    EnvWithWorkerHosts = [{"worker_hosts", ActualWorkerHosts} | Env],
    NewEnv = mzb_script_hooks:pre_hooks(DirFun, Body, EnvWithWorkerHosts, Config, get_logger(State)),
    NewConfig = maps:put(env, mzbl_script:normalize_env(NewEnv), Config),
    fun (S) -> S#{config => NewConfig} end;

handle_stage(pipeline, starting, #{cluster_connection:= Connection, config:= Config} = State) ->
    #{script:= Script, env:= Env} = Config,
    ScriptFilePath = script_path(Script),
    case director_call(Connection, {start_benchmark, remote_path(ScriptFilePath, Config), Env}) of
        ok -> ok;
        {error, List} ->
            ResStr = mzb_string:format("Start failed: ~s", [string:join(List, "\n")]),
            error(ResStr, [], State),
            mzb_pipeline:error(start_benchmark_failed,
                               fun (S) -> S#{result_str => ResStr} end)
    end;

handle_stage(pipeline, running, #{cluster_connection:= Connection} = State) ->
    try director_call(Connection, get_results, infinity) of
        {ok, Str, {Metrics, Histograms}} ->
            info("Benchmark result: SUCCESS~n~s", [Str], State),
            Res = aggregate_results(Metrics, Histograms, State),
            fun (S) -> S#{results => Res, result_str => Str} end;
        {error, Reason, ReasonStr, {Metrics, Histograms}} ->
            error("Benchmark result: FAILED~n~s", [ReasonStr], State),
            Res = aggregate_results(Metrics, Histograms, State),
            mzb_pipeline:error({benchmark_failed, Reason}, fun (S) -> S#{results => Res, result_str => ReasonStr} end)
    catch
        _:Error ->
            ResStr = mzb_string:format("Benchmark result: EXCEPTION~n~p", [Error]),
            error(ResStr, [], State),
            mzb_pipeline:error({benchmark_failed, Error},
                               fun (S) -> S#{result_str => ResStr} end)
    end;

handle_stage(pipeline, post_hooks, #{cluster_connection:= Connection, config:= Config} = State) ->
    #{script:= Script, env:= Env} = Config,
    ScriptFilePath = script_path(Script),
    DirFun = fun (Msg) -> director_call(Connection, Msg, ?HOOKS_TIMEOUT) end,
    {Body, _} = DirFun({read_and_validate, remote_path(ScriptFilePath, Config), Env}),
    NewEnv = mzb_script_hooks:post_hooks(DirFun, Body, Env, Config, get_logger(State)),
    NewConfig = maps:put(env, NewEnv, Config),
    fun (S) -> S#{config => NewConfig} end;

handle_stage(finalize, saving_bench_results, #{id:= Id, cluster_connection:= Connection, results:= CurRes} = State) ->

    NewRes =
        case CurRes of
            undefined  when Connection /= undefined -> % this happends when user presses stop button

                Metrics =
                    try director_call(Connection, get_metrics) of
                        {ok, M} -> M;
                        {error, _} -> []
                    catch
                        _:_ -> []
                    end,

                Histograms =
                    try director_call(Connection, get_cumulative_histograms) of
                        {ok, H} -> H;
                        {error, _} -> []
                    catch
                        _:_ -> []
                    end,

                aggregate_results(Metrics, Histograms, State);
            _ -> CurRes
        end,

    mzb_api_server:bench_finished(Id, status(State#{results => NewRes})),
    fun (S) -> S#{results => NewRes} end;

handle_stage(finalize, sending_email_report, #{emails:= Emails} = State) ->
    case send_email_report(Emails, status(State)) of
        ok -> ok;
        {error, {Error, Stacktrace}} ->
            error("Send report to ~p failed with reason: ~p~n~p", [Emails, Error, Stacktrace], State)
    end;

handle_stage(finalize, cleaning_nodes, #{config:= #{deallocate_after_bench:= false}} = State) ->
    info("Skip cleaning nodes. Deallocate after bench is false", [], State);
handle_stage(finalize, cleaning_nodes,
    State = #{node_pids := NodePids, config:= Config = #{director_host:= DirectorHost}})
      when DirectorHost /= undefined ->
    mzb_api_provision:clean_nodes(NodePids, Config, get_logger(State));
handle_stage(finalize, cleaning_nodes, State) ->
    info("Skip cleaning nodes. Unknown nodes", [], State);

handle_stage(finalize, release_exclusive, #{id:= Id, config:= Config}) ->
    Exclusive = mzb_bc:maps_get(exclusive, Config, []),
    mzb_api_exclusive:release(Id, Exclusive);

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

handle_call({change_env, Env}, From, #{status:= running, config:= Config, cluster_connection:= Connection} = State) ->
    info("Change env req received: ~p~nOldEnv: ~p", [Env, maps:get(env, Config)], State),
    Self = self(),
    director_async_call(Connection, {change_env, Env},
        fun ({result, Res}) ->
                info("Received change_env response: ~p", [Res], State),
                ok == Res andalso mzb_pipeline:cast(Self, {env_changed, Env}),
                mzb_pipeline:reply(From, Res);
            ({exception, {C, E, ST}}) ->
                error("Change env exception ~p:~p~n~p", [C, E, ST], State),
                mzb_pipeline:reply(From, {error, E})
        end),
    {noreply, State};

handle_call({change_env, _Env}, _From, #{} = State) ->
    {reply, {error, not_running}, State};

handle_call({run_command, Pool, Percent, Command}, From, #{status:= running, cluster_connection:= Connection} = State) ->
    info("Command run req received: ~p pool~p ~p%", [Command, Pool, Percent], State),
    director_async_call(Connection, {run_command, Pool, Percent, Command},
        fun ({result, Res}) ->
                info("Received run_command response: ~p", [Res], State),
                mzb_pipeline:reply(From, Res);
            ({exception, {C, E, ST}}) ->
                error("Run command exception ~p:~p~n~p", [C, E, ST], State),
                mzb_pipeline:reply(From, {error, E})
        end),
    {noreply, State};

handle_call({run_command, _, _, _}, _From, #{} = State) ->
    {reply, {error, not_running}, State};

handle_call({update_name, NewName}, _From, #{config:= Config} = State) ->
    info("Update bench name: ~p", [NewName], State),
    NewState = maps:put(config, maps:put(benchmark_name, NewName, Config), State),
    {reply, ok, NewState};

handle_call({add_tags, Tags}, _From, #{config:= Config} = State) ->
    info("Add tags: ~p / ~p", [Tags, mzb_bc:maps_get(tags, Config, [])], State),
    OldTags = mzb_bc:maps_get(tags, Config, []),
    NewTags = OldTags ++ [T || T <- Tags, not lists:member(T, OldTags)],
    NewState = maps:put(config, maps:put(tags, NewTags, Config), State),
    {reply, ok, NewState};

handle_call({remove_tags, Tags}, _From, #{config:= Config} = State) ->
    info("Remove tags: ~p", [Tags], State),
    NewTags = mzb_bc:maps_get(tags, Config, []) -- Tags,
    NewState = maps:put(config, maps:put(tags, NewTags, Config), State),
    {reply, ok, NewState};

handle_call(get_author, _From, #{config:= Config} = State) ->
    {reply, maps:get(author, Config), State};

handle_call(_Request, _From, State) ->
    error("Unhandled call: ~p", [_Request], State),
    {noreply, State}.

handle_cast({director_message, {new_metrics, NewMetrics}}, #{metrics:= Metrics, config:= Config} = State) ->
    lists:foreach(
        fun (M) ->
            File = metrics_file(M, Config),
            {ok, H} = file:open(File, [write]),
            file:close(H)
        end, mzb_api_metrics:extract_metric_names(NewMetrics) -- mzb_api_metrics:extract_metric_names(Metrics)),
    {noreply, maybe_update_bench(State#{metrics => NewMetrics})};

handle_cast({director_message, Unknown}, State) ->
    lager:error("Unknown director message ~p", [Unknown]),
    {noreply, State};

handle_cast({env_changed, NewEnv}, State = #{config:= Config}) ->
    #{env:= Env} = Config,
    Env2 = lists:foldl(
        fun ({K, V}, Acc) ->
            lists:keystore(K, 1, Acc, {K, V})
        end, Env, NewEnv),
    NewState = State#{config => Config#{env => Env2}},
    {noreply, maybe_update_bench(NewState)};

handle_cast({error_counter, system, Value}, State) ->
    {noreply, maybe_update_bench(State#{system_errors => Value})};

handle_cast({error_counter, user, Value}, State) ->
    {noreply, maybe_update_bench(State#{user_errors => Value})};

handle_cast({error_counter, inc_system}, #{system_errors:= OldValue} = State) ->
    {noreply, maybe_update_bench(State#{system_errors => OldValue + 1})};

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
    catch (maps:get(log_user_file_handler, State))(close);
% something is going wrong there. use special status for bench and run finalize stages again
terminate(Reason, #{id:= Id} = State) ->
    error("Receive terminate while finalize is not completed: ~p", [Reason], State),
    mzb_api_server:bench_finished(Id, status(State)),
    catch (maps:get(log_file_handler, State))(close),
    catch (maps:get(log_user_file_handler, State))(close),
    spawn(
        fun() ->
            try
                lager:info("Starting finalizing process for #~p", [Id]),
                {ok, Timer} = timer:kill_after(5 * 60 * 1000),
                Stages = proplists:get_value(finalize, workflow_config(State), []),
                NewState = handle_pipeline_status({final, failed}, State),

                lists:foreach(fun (Stage) ->
                                try
                                    Res = handle_stage(finalize, Stage, NewState),
                                    Res
                                catch
                                    _C:E ->
                                        ST = erlang:get_stacktrace(),
                                        error("Stage 'finalize - ~s': failed~n~s", [Stage, format_error(Stage, {E, ST})], NewState)
                                end
                            end, Stages),

                timer:cancel(Timer)
            catch
                Class:Error ->
                    Stacktrace= erlang:get_stacktrace(),
                    lager:error("Finalizing process for #~p has crashed with reason: ~p~n~p", [Id, Error, Stacktrace]),
                    erlang:raise(Class, Error, Stacktrace)
            end
        end),
    ok.

handle_pipeline_status(Info, State) ->
    NewState = handle_pipeline_status_ll(Info, State),
    maybe_update_bench(NewState).

maybe_update_bench(State = #{previous_status:= OldStatus}) ->
    case status(State) of
        OldStatus -> State;
        NewStatus ->
            mzb_api_firehose:update_bench(NewStatus),
            State#{previous_status => NewStatus}
    end.

handle_pipeline_status_ll({start, Phase, Stage}, State) ->
    info("Stage '~s - ~s': started", [Phase, Stage], State),
    case Phase of
        pipeline -> State#{status => Stage};
        _ -> State
    end;
handle_pipeline_status_ll({complete, Phase, Stage}, State) ->
    info("Stage '~s - ~s': finished", [Phase, Stage], State),
    State;
handle_pipeline_status_ll({exception, Phase, Stage, E, ST}, #{result_str:= ResStr} = State) ->
    error("Stage '~s - ~s': failed~n~s", [Phase, Stage, format_error(Stage, {E, ST})], State),
    case ResStr of
        "" ->
            Res = mzb_string:format("Stage ~p failed: ~p", [Stage, E]),
            State#{result_str => Res};
        _ ->
            State
    end;
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
            _ ->
                 lager:error("mzbench_git param is obsolete, use node_git instead"),
                 erlang:error(mzbench_git_is_obsolete_use_node_git)
        end,
        case Get(mzbench_rsync) of
            undefined -> ok;
            _ -> lager:error("mzbench_rsync param is obsolete, use node_rsync instead"),
                 erlang:error(mzbench_rsync_is_obsolete_use_node_rsync)
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
            mzbl_script:make_git_install_spec(GitRepo, GitBranch, "node", "");
        Remote ->
            mzbl_script:make_rsync_install_spec(Remote, "node", [])
    end.

extract_metric_update_interval(Params) ->
    case maps:find(metric_update_interval_ms, Params) of
        {ok, Value} when Value /= undefined -> Value;
        _ -> application:get_env(mzbench_api, metric_update_interval_ms, undefined)
    end.

send_email_report(Emails, #{id:= Id,
                            status:= Status,
                            config:= Config,
                            metrics:= Metrics}) ->
    try
        MetricNames = mzb_api_metrics:extract_metric_names(Metrics),
        MetricFilenames = [metrics_file(N, Config) || N <- MetricNames],
        {Subj, Body} = generate_mail_body(Id, Status, Config),
        lager:info("EMail report: ~n~s~n~s~n", [Subj, Body]),
        Attachments = lists:map(
            fun (F) ->
                {ok, Bin} = file:read_file(local_path(F, Config)),
                Filename = filename:basename(F),
                {list_to_binary(Filename), <<"text/plain">>, Bin}
            end, MetricFilenames),
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

status(#{data:= #{includes:= Includes}} = State) ->
    Res = mzb_bc:maps_with([id, status, create_time, start_time, finish_time, config, metrics,
                            results, result_str, user_errors, system_errors], State),
    Filenames = [{Filename, Size} || {Filename, Size, _} <- Includes],
    Res#{includes => Filenames}.

generate_bench_env(Id, Params) ->
    Env = maps:get(env, Params),
    Script = maps:get(script, Params),
    #{name := ScriptName} = Script,
    lists:foldl(fun ({K, V}, E) ->
                        case proplists:get_value(K, E) of
                            undefined -> [{K, V}|E];
                            _ -> E
                        end
                       end, Env,
                [{"mzb_script_name", list_to_binary(ScriptName)}, {"mzb_bench_id", Id}]).

script_path(Script) ->
    case Script of
        #{filename := FileName} -> FileName;
        Pkg -> filename:join(
            [mzb_api_paths:node_deployment_path(), "mzbench_workers", Pkg, "default.erl"])
    end.

run_periodically(StartTime, MaxTime, RetryTimeoutSec, Fn) ->
    case Fn() of
        ok -> ok;
        retry ->
            TimeSinceStart = seconds() -  StartTime,
            case TimeSinceStart =< MaxTime of
                true ->
                    timer:sleep(RetryTimeoutSec * 1000),
                    run_periodically(StartTime, MaxTime, RetryTimeoutSec, Fn);
                _ ->
                    erlang:error(max_time_reached)
            end
    end.

allocate_hosts(#{nodes_arg:= N, cloud:= Cloud} = Config, Logger) when is_integer(N), N > 0 ->
    #{id:= BenchId,
      purpose:= Purpose,
      initial_user:= User} = Config,
    Description = mzb_string:format("MZBench cluster:~n~p", [Config]),
    ClusterConfig = #{
        purpose => Purpose,
        user => User,
        description => Description
    },
    % Allocate one supplementary node for the director
    Logger(info, "Allocating ~p hosts in ~p cloud...", [N + 1, Cloud]),
    {ok, ClusterId, UserName, Hosts} = mzb_api_cloud:create_cluster(BenchId, Cloud, N + 1, ClusterConfig),
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

log_user_file(Config = #{log_user_file:= File}) ->
    local_path(File, Config).

metrics_file(Name, Config = #{metrics_file:= File}) ->
    local_path(mzb_string:format(File, [re:replace(Name, "\\W", "_", [global, {return, list}])]), Config).

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

generate_mail_body(Id, Status, Config) ->
    #{env:= Env, script:= Script} = Config,
    #{name := ScriptName, body := ScriptBody} = Script,
    Subject = io_lib:format("Bench report for ~s (~s)", [ScriptName, Status]),
    Chars = io_lib:format(
        "Status: ~s~n~n"
        "Environment:~n~s~n~n"
        "Script body:~n~s~n~n"
        "Benchmark logs:~n  ~s~n~n"
        "Metrics data:~n  ~s~n~n",
        [Status,
         indent(string:join([io_lib:format("~p = ~p", [K,V]) || {K,V} <- Env], "\n"), 2, "(no env variables)"),
         indent(ScriptBody, 2),
         bench_log_link(Id, Config),
         bench_data_link(Id, Config)
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

info(Format, Args, State) ->
    log(info, Format, Args, State).

error(Format, Args, State) ->
    log(error, Format, Args, State).

log(Severity, Format, Args, #{log_file_handler:= H, id:= Id, self:= Self}) ->
    Format2 = "[ BENCH #~b ] " ++ Format,
    Args2 = [Id|Args],
    DefaultLogger = mzb_api_app:default_logger(),
    DefaultLogger(Severity, Format2, Args2),
    (Severity == error) andalso mzb_pipeline:cast(Self, {error_counter, inc_system}),
    format_log(H, Severity, Format, Args).

format_log(_Handler, debug, _Format, _Args) -> ok;
format_log(Handler, Severity, Format, Args) ->
    Now = {_, _, Ms} = os:timestamp(),
    {_, {H,M,S}} = calendar:now_to_universal_time(Now),
    _ = Handler({write, io_lib:format("~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0B [~s] [ API ] ~p " ++ Format ++ "~n", [H, M, S, Ms div 1000, Severity, self()|Args])}),
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
        ({write, Data}) ->
            _ = file:write(H, Data),
            ok
    end;
get_file_writer(Filename, deflate) ->
    P = erlang:spawn_link(fun () -> deflate_process(Filename) end),
    fun (close) ->
            Ref = erlang:monitor(process, P),
            P ! close,
            receive
                {'DOWN', Ref, _, _, _} -> ok
            after
                10000 ->
                    erlang:exit(P, kill)
            end;
        ({write, Data}) ->
            Ref = erlang:make_ref(),
            P ! {write_sync, self(), Ref, Data},
            Ref2 = erlang:monitor(process, P),
            receive
                {write_res, Ref, ok} ->
                    erlang:demonitor(Ref2, [flush]),
                    ok;
                {'DOWN', Ref2, _, _, _} -> ok
            end
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
    fun
        D(N) when (N rem 100) == 0 ->
            Flush(),
            D(N + 1);
        D(N) ->
            receive
                {'EXIT', _, _} -> Close();
                close -> Close();
                flush -> _ = Flush(), D(N + 1);
                {write_sync, From, Ref1, Data} ->
                    _ = file:write(H, zlib:deflate(Z, Data, none)),
                    From ! {write_res, Ref1, ok},
                    D(N + 1);
                {write, Data} ->
                    _ = file:write(H, zlib:deflate(Z, Data, none)),
                    D(N + 1)
            end
    end (0).

director_async_call(Connection, Msg, Continuation) ->
    mzb_api_connection:send_message(Connection, {request, Continuation, Msg}).

director_call(Connection, Msg) ->
    director_call(Connection, Msg, ?DIRECTOR_CALL_TIMEOUT).

director_call(Connection, Msg, Timeout) ->
    Ref = erlang:make_ref(),
    Self = self(),
    Cont = fun (Res) -> Self ! {Ref, Res} end,
    director_async_call(Connection, Msg, Cont),
    Mon = mzb_api_connection:monitor(Connection),
    case Timeout of
        infinity ->
            receive
                {Ref, {result, Res}} ->
                    mzb_api_connection:demonitor(Mon),
                    Res;
                {Ref, {exception, {C, E, ST}}} ->
                    erlang:raise(C, E, ST);
                {'DOWN', Mon, _, _, _} ->
                    erlang:error(director_connection_down)
            end;
        _ ->
            receive
                {Ref, {result, Res}} ->
                    mzb_api_connection:demonitor(Mon),
                    Res;
                {Ref, {exception, {C, E, ST}}} ->
                    erlang:raise(C, E, ST);
                {'DOWN', Mon, _, _, _} ->
                    erlang:error(director_connection_down)
            after Timeout ->
                mzb_api_connection:demonitor(Mon),
                erlang:error({director_call_timeout, Msg})
            end
    end.

handle_management_msg({message, Msg}, Self, S) ->
    case erlang:binary_to_term(Msg) of
        {metric_value, "errors.user" = Name, Timestamp, Value} ->
            mzb_pipeline:cast(Self, {error_counter, user, Value}),
            report_metrics(Name, Timestamp, Value, S);
        {metric_value, "errors.system" = Name, Timestamp, Value} ->
            mzb_pipeline:cast(Self, {error_counter, system, Value}),
            report_metrics(Name, Timestamp, Value, S);
        {metric_value, Name, Timestamp, Value} ->
            report_metrics(Name, Timestamp, Value, S);
        {response, Continuation, Res} ->
            Continuation(Res),
            {ok, S};
        Any ->
            mzb_pipeline:cast(Self, {director_message, Any}),
            {ok, S}
    end;
handle_management_msg({error, _}, _, S = #{handlers:= Handlers}) ->
    _ = [ H(close) || {_, H} <- maps:to_list(Handlers)],
    {ok, S#{handlers => #{}}}.

report_metrics(Name, Timestamp, Value, #{config:= Config, handlers:= Handlers} = S) ->
    ToWrite = io_lib:format("~B\t~p~n", [Timestamp, Value]),
    case maps:find(Name, Handlers) of
        {ok, H} -> {H({write, ToWrite}), S};
        error ->
            MetricsFile = metrics_file(Name, Config),
            H = get_file_writer(MetricsFile, none),
            {H({write, ToWrite}), S#{handlers => maps:put(Name, H, Handlers)}}
    end.

aggregate_results(Metrics, Histograms, #{config:= Config} = State) ->

    Percentiles = application:get_env(mzbench_api, final_metrics_percentiles, []),

    Flatten = [ M || {group, _, Graphs} <- Metrics, {graph, #{metrics:= Ms}} <- Graphs, M <- Ms],

    Res = lists:flatmap(
        fun (M) ->
            try
                aggregate_results_for_metric(M, Config, Percentiles, Histograms)
            catch
                _:Error ->
                    error("Aggregating result for ~p failed: ~p~nStacktrace: ~p", [M, Error, erlang:get_stacktrace()], State),
                    []
            end
        end, Flatten),
    info("Bench final metrics: ~300p", [Res], State),
    Res.

aggregate_results_for_metric({Name, counter, _}, Config, Percentiles, _) ->
    File = mzb_api_bench:metrics_file(Name, Config),
    FinalValue = metric_file_fold(File, fun (_, Value, _) -> Value end, undefined),
    FileRPS = mzb_api_bench:metrics_file(Name ++ ".rps", Config),
    Data = metric_file_fold(FileRPS, fun (_, Value, Acc) -> [Value|Acc] end, []),
    RPSFinal = statistics(Data, Percentiles),
    [{Name, counter, {FinalValue, RPSFinal}}];
aggregate_results_for_metric({Name, Type, _}, Config, Percentiles, _) when Type == gauge; Type == derived ->
    File = mzb_api_bench:metrics_file(Name, Config),
    Data = metric_file_fold(File, fun (_, Value, Acc) -> [Value|Acc] end, []),
    Final = statistics(Data, Percentiles),
    [{Name, gauge, Final}];
aggregate_results_for_metric({Name, histogram, _}, _, Percentiles, Histograms) ->
    case proplists:get_value(Name, Histograms, undefined) of
        undefined -> [];
        Values ->
            {ok, Ref} = hdr_histogram:from_binary(Values),
            try
                PValues = lists:map(
                    fun (min) -> {"min", hdr_histogram:min(Ref)};
                        (max) -> {"max", hdr_histogram:max(Ref)};
                        (mean) -> {"mean", hdr_histogram:mean(Ref)};
                        (median) -> {"median", hdr_histogram:median(Ref)};
                        (N) when N =< 100 -> {integer_to_list(N), hdr_histogram:percentile(Ref, erlang:float(N))}
                    end, Percentiles),
                [{Name, histogram, PValues}]
            after
                hdr_histogram:close(Ref)
            end
    end.

statistics([], _) -> [];
statistics(Data, Percentiles) ->
    Sorted = lists:sort(Data),
    Len = length(Sorted),
    lists:map(fun
        (mean) ->   {"mean", lists:sum(Sorted) / Len};
        (median) -> {"median", percentile(Sorted, Len, 50)};
        (min) ->    {"min", hd(Sorted)};
        (max) ->    {"max", lists:last(Sorted)};
        (P) when 0 < P, P =< 100 -> {integer_to_list(P), percentile(Sorted, Len, P)}
    end, Percentiles).

percentile(Sorted, Len, P) ->
    K = mzb_utility:int_ceil(P*Len/100),
    lists:nth(K, Sorted).

metric_file_fold(File, Fun, InitAcc) ->
    case file:open(File, [raw, read, binary]) of
        {ok, H} ->
            try
                fun R(Acc) ->
                    case file:read_line(H) of
                        {ok, <<>>} -> R(Acc);
                        {ok, D} ->
                            [TimestampBin, ValueBin] = binary:split(D, <<"\t">>),
                            Timestamp = erlang:binary_to_integer(TimestampBin),
                            Value = mzb_utility:any_to_num(string:strip(erlang:binary_to_list(ValueBin), right, $\n)),
                            NewAcc = Fun(Timestamp, Value, Acc),
                            R(NewAcc);
                        eof -> Acc;
                        {error, Reason} ->
                            erlang:error({metrics_read_error, Reason})
                    end
                end (InitAcc)
            after
                file:close(H)
            end;
        {error, enoent} -> InitAcc;
        {error, Error} ->
            erlang:error(Error)
    end.
