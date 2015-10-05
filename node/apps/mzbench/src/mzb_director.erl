-module(mzb_director).

-export([start_link/5,
         pool_report/4,
         attach/1]).

-behaviour(gen_server).
-export([init/1,
         handle_cast/2,
         handle_call/3,
         handle_info/2,
         terminate/2,
         code_change/3,
         stop_benchmark/2
        ]).

-include_lib("mzbench_language/include/mzbl_types.hrl").

-record(state, {
    super_pid  = undefined,
    failed     = 0,
    succeed    = 0,
    pools      = [],
    owner      = undefined,
    bench_name = undefined,
    stop_reason = undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(SuperPid, BenchName, Script, Nodes, Env) ->
    gen_server:start_link(?MODULE, [SuperPid, BenchName, Script, Nodes, Env], []).

pool_report(DirectorPid, PoolPid, Info, IsFinal) ->
    gen_server:cast(DirectorPid, {pool_report, PoolPid, Info, IsFinal}).

attach(DirectorPid) ->
    gen_server:call(DirectorPid, attach, infinity).

stop_benchmark(DirectorPid, Reason) ->
    gen_server:cast(DirectorPid, {stop_benchmark, Reason}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([SuperPid, BenchName, Script, Nodes, Env]) ->
    lager:info("[ director ] Bench name ~p, director node ~p", [BenchName, erlang:node()]),
    {Pools, Env2} = mzbl_script:extract_pools_and_env(Script, Env),
    lager:info("[ director ] Pools: ~p, Env: ~p", [Pools, Env2]),
    _ = mzb_signaler:set_nodes(Nodes),
    gen_server:cast(self(), {start_pools, Pools, Env2, Nodes}),
    {ok, #state{
        bench_name = BenchName,
        super_pid = SuperPid
    }}.

handle_call(attach, From, State) ->
    maybe_report_and_stop(State#state{owner = From});

handle_call(Req, _From, State) ->
    lager:error("Unhandled call: ~p", [Req]),
    {stop, {unhandled_call, Req}, State}.

handle_cast({start_pools, _Pools, _Env, []}, State) ->
    lager:error("[ director ] There are no alive nodes to start workers"),
    {stop, empty_nodes, State};
handle_cast({start_pools, Pools, Env, Nodes}, #state{super_pid = SuperPid} = State) ->
    Metrics = mzb_script_metrics:script_metrics(Pools, Nodes),
    Prefix = proplists:get_value("graphite_prefix", Env),
    {ok, _} = supervisor:start_child(SuperPid,
        {mzb_metrics,
         {mzb_metrics, start_link, [Prefix, Env, Metrics, Nodes, self()]},
         transient, 5000, worker, [mzb_metrics]}),
    StartedPools = start_pools(Pools, Env, Nodes, []),
    maybe_stop(State#state{pools = StartedPools});

handle_cast({pool_report, PoolPid, Info, true}, #state{pools = Pools} = State) ->
    NewState = handle_pool_report(Info, State),
    catch erlang:demonitor(proplists:get_value(PoolPid, Pools), [flush]),
    NewPools = proplists:delete(PoolPid, Pools),
    maybe_stop(NewState#state{pools = NewPools});

handle_cast({pool_report, _PoolPid, Info, false}, #state{} = State) ->
    {noreply, handle_pool_report(Info, State)};

handle_cast({stop_benchmark, Reason}, #state{} = State) ->
    maybe_stop(State#state{stop_reason = Reason});

handle_cast(Req, State) ->
    lager:error("Unhandled cast: ~p", [Req]),
    {stop, {unhandled_cast, Req}, State}.

handle_info({'DOWN', _Ref, _, Pid, Reason}, #state{pools = Pools} = State) ->
    lager:error("Received DOWN from pool ~p with reason ~p", [Pid, Reason]),
    case Reason of
        normal ->
            NewPools = proplists:delete(Pid, Pools),
            maybe_stop(State#state{pools = NewPools});
        _ ->
            {stop, pool_crashed, State}
    end;

handle_info(Req, State) ->
    lager:error("Unhandled info: ~p", [Req]),
    {noreply, State}.

-spec handle_pool_report(Info :: [{K :: atom(), V :: term()}], #state{}) -> #state{}.
handle_pool_report(Info, #state{succeed = Ok, failed = NOk} = State) ->
    NewOk = Ok + proplists:get_value(succeed_workers, Info, 0),
    NewNOk = NOk + proplists:get_value(failed_workers, Info, 0),
    State#state{succeed = NewOk, failed = NewNOk}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_pools([], _, _, Acc) ->
    lager:info("[ director ] Started all pools"),
    Acc;
start_pools([Pool | Pools], Env, Nodes, Acc) ->
    #operation{args = [PoolOpts, _]} = Pool,
    [SizeU] = mzbl_ast:find_operation_and_extract_args(size, PoolOpts, [undefined]),
    Size = mzb_utility:to_integer_with_default(SizeU, undefined),
    NumberedNodes = lists:zip(lists:seq(1, length(Nodes)), Nodes),
    Self = self(),
    Results = mzb_lists:pmap(fun({Num, Node}) ->
            rpc:call(Node, mzb_bench_sup, start_pool, 
                [[Self, Pool, Env, length(Nodes), Num]])
        end, NumberedNodes),
    lager:info("Start pool results: ~p", [Results]),
    NewRef = lists:map(fun({ok, Pid}) -> {Pid, erlang:monitor(process, Pid)} end, Results),
    start_pools(Pools, Env, shift(Nodes, Size), NewRef ++ Acc).

stop_pools(Pools) ->
    _ = [catch erlang:demonitor(Mon, [flush]) || {_, Mon} <- Pools],
    _ = mzb_lists:pmap(fun ({Pid, _}) -> catch mzb_pool:stop(Pid) end, Pools),
    ok.

shift(Nodes, undefined) -> Nodes;
shift(Nodes, 0) -> Nodes;
shift(Nodes, Size) when length(Nodes) < Size -> shift(Nodes, Size rem length(Nodes));
shift(Nodes, Size) when Size > 0 -> {F, T} = lists:split(Size, Nodes), T ++ F.

maybe_stop(#state{stop_reason = Reason, succeed = Ok, failed = NOk} = State) when Reason /= undefined ->
    lager:info("[ director ] Received stop signal with reason: ~p", [Reason]),
    lager:info("[ director ] Succeed/Failed workers = ~p/~p", [Ok, NOk]),
    stop_pools(State#state.pools),
    maybe_report_and_stop(State#state{pools = []});

maybe_stop(#state{pools = [], succeed = Ok, failed = NOk} = State) ->
    ok = mzb_metrics:final_trigger(),
    lager:info("[ director ] All pools have finished, stopping mzb_director_sup ~p", [State#state.super_pid]),
    lager:info("[ director ] Succeed/Failed workers = ~p/~p", [Ok, NOk]),
    FailedAsserts = mzb_metrics:get_failed_asserts(),
    Reason =
        case FailedAsserts of
            [] -> normal;
            _ ->
                AssertMessages = [Msg || {_, Msg} <- FailedAsserts],
                lager:error("[ director ] Failed assertions:~n~s", [string:join(AssertMessages, "\n")]),
                {assertions_failed, FailedAsserts}
        end,
    maybe_report_and_stop(State#state{stop_reason = Reason});

maybe_stop(#state{} = State) ->
    {noreply, State}.

maybe_report_and_stop(#state{stop_reason = undefined} = State) ->
    {noreply, State};
maybe_report_and_stop(#state{owner = undefined} = State) ->
    lager:info("[ director ] Waiting for someone to report results..."),
    {noreply, State};
maybe_report_and_stop(#state{owner = Owner} = State) ->
    lager:info("[ director ] Reporting benchmark results to ~p", [Owner]),
    Res = format_results(State),
    gen_server:reply(Owner, Res),
    erlang:spawn(fun mzb_sup:stop_bench/0),
    {stop, normal, State}.

format_results(#state{stop_reason = normal, succeed = Ok, failed = 0}) ->
    {ok, mzb_string:format("SUCCESS~n~b workers have finished successfully", [Ok])};
format_results(#state{stop_reason = normal, succeed = Ok, failed = NOk}) ->
    {error, {workers_failed, NOk},
        mzb_string:format("FAILED~n~b of ~b workers failed", [NOk, Ok + NOk])};
format_results(#state{stop_reason = {assertions_failed, FailedAsserts}}) ->
    AssertsStr = string:join([S||{_, S} <- FailedAsserts], "\n"),
    Str = mzb_string:format("FAILED~n~b assertions failed~n~s",
                        [length(FailedAsserts), AssertsStr]),
    {error, {asserts_failed, length(FailedAsserts)}, Str}.
