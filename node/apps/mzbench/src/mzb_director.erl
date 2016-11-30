-module(mzb_director).

-export([start_link/6,
         pool_report/3,
         change_env/1,
         attach/0,
         notify/1,
         compile_and_load/2,
         is_alive/0
         ]).

-behaviour(gen_server).
-export([init/1,
         handle_cast/2,
         handle_call/3,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-include_lib("mzbench_language/include/mzbl_types.hrl").

-record(state, {
    super_pid  = undefined,
    failed     = 0,
    succeed    = 0,
    stopped    = 0,
    pools      = [],
    owner      = undefined,
    bench_name = undefined,
    stop_reason = undefined,
    script     = undefined,
    env        = undefined,
    assertions = [],
    nodes      = [],
    continuation = undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(SuperPid, BenchName, Script, Nodes, Env, Continuation) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [SuperPid, BenchName, Script, Nodes, Env, Continuation], []).

pool_report(PoolPid, Info, IsFinal) ->
    gen_server:cast(?MODULE, {pool_report, PoolPid, Info, IsFinal}).

change_env(Env) ->
    gen_server:call(?MODULE, {change_env, Env}, infinity).

attach() ->
    gen_server:call(?MODULE, attach, infinity).

notify(Message) ->
    gen_server:cast(?MODULE, {notification, Message}).

is_alive() ->
    case whereis(?MODULE) of
        undefined -> false;
        Pid when is_pid(Pid) -> true
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([SuperPid, BenchName, Script, Nodes, Env, Continuation]) ->
    system_log:info("[ director ] Bench name ~p, director node ~p", [BenchName, erlang:node()]),
    {Pools, Env2, Asserts} = mzbl_script:extract_info(Script, Env),
    system_log:info("[ director ] Pools: ~p, Env: ~p, Asserts: ~p", [Pools, Env2, Asserts]),

    {_, []} = mzb_interconnect:multi_call(Nodes, {set_signaler_nodes, Nodes}),
    gen_server:cast(self(), start_pools),
    _ = mzb_lists:pmap(fun(Node) ->
        ok = mzb_interconnect:call(Node, {mzb_watchdog, activate})
    end, lists:usort(Nodes)),
    {ok, #state{
        script = Pools,
        env = Env2,
        assertions = Asserts,
        nodes = Nodes,
        bench_name = BenchName,
        super_pid = SuperPid,
        continuation = Continuation
    }}.

handle_call({change_env, NewEnv}, _From, #state{script = Script, env = Env, nodes = Nodes} = State) ->
    system_log:info("Changing env: ~p", [NewEnv]),
    MergedEnv = lists:foldl(
        fun ({K, V}, Acc) ->
            lists:keystore(K, 1, Acc, {K, V})
        end, Env, mzbl_script:normalize_env(NewEnv)),
    try
        {[{_, {ok, _}}|_], []} = mzb_interconnect:multi_call(lists:usort([node()|Nodes]), {compile_env, Script, MergedEnv}),
        {reply, ok, State#state{env = MergedEnv}}
    catch
        _:E ->
            system_log:error("Change env failed with reason ~p~nEnv:~p~nStacktrace:~p", [E, MergedEnv, erlang:get_stacktrace()]),
            {reply, {error, {internal_error, E}}, State}
    end;

handle_call(attach, From, State) ->
    maybe_report_and_stop(State#state{owner = From});

handle_call(Req, _From, State) ->
    system_log:error("Unhandled call: ~p", [Req]),
    {stop, {unhandled_call, Req}, State}.

handle_cast(start_pools, #state{nodes = []} = State) ->
    system_log:error("[ director ] There are no alive nodes to start workers"),
    {stop, empty_nodes, State};

handle_cast(start_pools, #state{script = Script, env = Env, nodes = Nodes} = State) ->
    try start_metrics(State) of
        ok ->
            {[{_, {ok, NewScript}}|_], []} = mzb_interconnect:multi_call(lists:usort([node()|Nodes]), {compile_env, Script, Env}),
            StartedPools = start_pools(NewScript, Env, Nodes, []),
            maybe_stop(State#state{pools = StartedPools})
    catch
        _:E ->
            maybe_stop(State#state{stop_reason = {start_metrics_failed, E}})
    end;

handle_cast({pool_report, PoolPid, Info, true}, #state{pools = Pools} = State) ->
    NewState = handle_pool_report(Info, State),
    catch mzb_interconnect:demonitor(proplists:get_value(PoolPid, Pools), [flush]),
    NewPools = proplists:delete(PoolPid, Pools),
    maybe_stop(NewState#state{pools = NewPools});

handle_cast({pool_report, _PoolPid, Info, false}, #state{} = State) ->
    {noreply, handle_pool_report(Info, State)};

handle_cast({notification, {assertions_failed, _} = Reason}, #state{} = State) ->
    maybe_stop(State#state{stop_reason = Reason});

handle_cast({notification, server_connection_closed = Reason}, #state{} = State) ->
    maybe_stop(State#state{stop_reason = Reason});

handle_cast(Req, State) ->
    system_log:error("Unhandled cast: ~p", [Req]),
    {stop, {unhandled_cast, Req}, State}.

handle_info({'DOWN', _Ref, _, Pid, Reason}, #state{pools = Pools} = State) ->
    system_log:error("Received DOWN from pool ~p with reason ~p", [Pid, Reason]),
    case Reason of
        normal ->
            NewPools = proplists:delete(Pid, Pools),
            maybe_stop(State#state{pools = NewPools});
        _ ->
            {stop, pool_crashed, State}
    end;

handle_info(Req, State) ->
    system_log:error("Unhandled info: ~p", [Req]),
    {noreply, State}.

-spec handle_pool_report(Info :: [{K :: atom(), V :: term()}], #state{}) -> #state{}.
handle_pool_report(Info, #state{succeed = Ok, failed = NOk, stopped = Stopped} = State) ->
    NewOk = Ok + proplists:get_value(succeed_workers, Info, 0),
    NewNOk = NOk + proplists:get_value(failed_workers, Info, 0),
    NewStopped = Stopped + proplists:get_value(stopped_workers, Info, 0),
    State#state{succeed = NewOk, failed = NewNOk, stopped = NewStopped}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_metrics(#state{script = Script, assertions = Asserts, nodes = Nodes, super_pid = SuperPid}) ->
    LoopAssertMetrics = mzbl_script:get_loop_assert_metrics(Script),
    {ok, _} = supervisor:start_child(SuperPid,
        {mzb_metrics,
         {mzb_metrics, start_link, [Asserts, LoopAssertMetrics, Nodes]},
         transient, 5000, worker, [mzb_metrics]}),
    {NodeSystemMetrics, []} = mzb_interconnect:multi_call(lists:usort([node()|Nodes]), get_system_metrics, _DefaultTimeout = 60000),
    SystemMetrics = lists:append([M || {_, M} <- NodeSystemMetrics]),
    WorkerMetrics = mzb_script_metrics:script_metrics(Script, Nodes),
    mzb_metrics:declare_metrics(WorkerMetrics ++ SystemMetrics),
    ok.

start_pools([], _, _, Acc) ->
    system_log:info("[ director ] Started all pools"),
    Acc;
start_pools([Pool | Pools], Env, Nodes, Acc) ->
    #operation{args = [PoolOpts, _]} = Pool,
    [SizeExpr] = mzbl_ast:find_operation_and_extract_args(size, PoolOpts, [undefined]),
    SizeU = mzbl_interpreter:eval_std(SizeExpr, Env),
    Size = mzb_utility:to_integer_with_default(SizeU, undefined),
    NumberedNodes = lists:zip(lists:seq(1, length(Nodes)), Nodes),
    Self = self(),
    NewRef = mzb_lists:pmap(fun({Num, Node}) ->
            mzb_interconnect:spawn_monitor(Node, Self,
                {start_pool, Pool, Env, length(Nodes), Num})
        end, NumberedNodes),
    system_log:info("Start pool monitors: ~p", [NewRef]),
    Results = [{Pid, Mon} || {_, Pid, _} = Mon <- NewRef],
    start_pools(Pools, Env, shift(Nodes, Size), Results ++ Acc).

stop_pools(Pools) ->
    _ = [catch mzb_interconnect:demonitor(Mon, [flush]) || {_, Mon} <- Pools],
    [catch mzb_interconnect:call(node(P), {stop_pool, P}) || P <- Pools],
    ok.

shift(Nodes, undefined) -> Nodes;
shift(Nodes, 0) -> Nodes;
shift(Nodes, Size) when length(Nodes) < Size -> shift(Nodes, Size rem length(Nodes));
shift(Nodes, Size) when Size > 0 -> {F, T} = lists:split(Size, Nodes), T ++ F.

maybe_stop(#state{stop_reason = Reason, succeed = Ok, failed = NOk, stopped = Stopped} = State) when Reason /= undefined ->
    system_log:info("[ director ] Received stop signal with reason: ~p", [Reason]),
    system_log:info("[ director ] Succeed/Failed workers = ~p/~p", [Ok, NOk]),
    system_log:info("[ director ] Stopped workers = ~p", [Stopped]),
    stop_pools(State#state.pools),
    maybe_report_and_stop(State#state{pools = []});

maybe_stop(#state{pools = [], succeed = Ok, failed = NOk, stopped = Stopped} = State) ->
    ok = mzb_metrics:final_trigger(),
    system_log:info("[ director ] All pools have finished, stopping mzb_director_sup ~p", [State#state.super_pid]),
    system_log:info("[ director ] Succeed/Failed workers = ~p/~p", [Ok, NOk]),
    system_log:info("[ director ] Stopped workers = ~p", [Stopped]),
    FailedAsserts = mzb_metrics:get_failed_asserts(),
    Reason =
        case FailedAsserts of
            [] -> normal;
            _ ->
                AssertMessages = [Msg || {_, Msg} <- FailedAsserts],
                system_log:error("[ director ] Failed assertions:~n~s", [string:join(AssertMessages, "\n")]),
                {assertions_failed, FailedAsserts}
        end,
    maybe_report_and_stop(State#state{stop_reason = Reason});

maybe_stop(#state{} = State) ->
    {noreply, State}.

maybe_report_and_stop(#state{stop_reason = undefined} = State) ->
    {noreply, State};
maybe_report_and_stop(#state{stop_reason = server_connection_closed, continuation = Continuation} = State) ->
    Continuation(),
    system_log:error("[ director ] Stop benchmark because server connection is down"),
    erlang:spawn(fun mzb_sup:stop_bench/0),
    {stop, normal, State};
maybe_report_and_stop(#state{owner = undefined} = State) ->
    system_log:info("[ director ] Waiting for someone to report results..."),
    {noreply, State};
maybe_report_and_stop(#state{owner = Owner, continuation = Continuation} = State) ->
    Continuation(),
    system_log:info("[ director ] Reporting benchmark results to ~p", [Owner]),
    Res = format_results(State),
    gen_server:reply(Owner, Res),
    erlang:spawn(fun mzb_sup:stop_bench/0),
    {stop, normal, State}.

format_results(#state{stop_reason = normal, succeed = Ok, failed = 0, stopped = 0}) ->
    {ok, mzb_string:format("SUCCESS~n~b workers have finished successfully", [Ok]), get_stats_data()};
format_results(#state{stop_reason = normal, succeed = Ok, failed = 0, stopped = Stopped}) ->
    {ok, mzb_string:format("SUCCESS~n~b workers have finished successfully (~b workers have been stopped)", [Ok, Stopped]), get_stats_data()};
format_results(#state{stop_reason = normal, succeed = Ok, failed = NOk, stopped = 0}) ->
    {error, {workers_failed, NOk},
        mzb_string:format("FAILED~n~b of ~b workers failed", [NOk, Ok + NOk]), get_stats_data()};
format_results(#state{stop_reason = normal, succeed = Ok, failed = NOk, stopped = Stopped}) ->
    {error, {workers_failed, NOk},
        mzb_string:format("FAILED~n~b of ~b workers failed and ~b workers have been stopped", [NOk, Ok + NOk, Stopped]), get_stats_data()};
format_results(#state{stop_reason = {assertions_failed, dynamic_deadlock}}) ->
    Str = mzb_string:format("FAILED~nDynamic deadlock detected~n", []),
    {error, {asserts_failed, 1}, Str, get_stats_data()};
format_results(#state{stop_reason = {assertions_failed, FailedAsserts}}) ->
    AssertsStr = string:join([S||{_, S} <- FailedAsserts], "\n"),
    Str = mzb_string:format("FAILED~n~b assertions failed~n~s",
                        [length(FailedAsserts), AssertsStr]),
    {error, {asserts_failed, length(FailedAsserts)}, Str, get_stats_data()};
format_results(#state{stop_reason = {start_metrics_failed, E}}) ->
    Str = mzb_string:format("FAILED~nstart metrics subsystem failed: ~p", [E]),
    {error, start_metrics_failed, Str, get_stats_data()}.

get_stats_data() ->
    try
        {mzb_metrics:get_metrics(), mzb_metrics:get_histogram_data()}
    catch
        _:Error ->
            system_log:error("Get stats data exception: ~p~n~p", [Error, erlang:get_stacktrace()]),
            erlang:error(Error)
    end.

compile_and_load(Script, Env) ->
    {NewScript, ModulesToLoad} = mzb_compiler:compile(Script, Env),
    [{module, _} = code:load_binary(Mod, mzb_string:format("~s.erl", [Mod]), Bin) || {Mod, Bin} <- ModulesToLoad],
    {ok, NewScript}.

