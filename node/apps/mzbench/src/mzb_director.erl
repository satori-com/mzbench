-module(mzb_director).

-export([start_link/6,
         pool_report/4,
         attach/1]).

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
    reportfile = undefined,
    pools      = [],
    owner      = undefined,
    bench_name = undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(SuperPid, BenchName, Script, Nodes, Env, ReportFile) ->
    gen_server:start_link(?MODULE, [SuperPid, BenchName, Script, Nodes, Env, ReportFile], []).

pool_report(DirectorPid, PoolPid, Info, IsFinal) ->
    gen_server:cast(DirectorPid, {pool_report, PoolPid, Info, IsFinal}).

attach(DirectorPid) ->
    gen_server:call(DirectorPid, attach, infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([SuperPid, BenchName, Script, Nodes, Env, ReportFile]) ->
    lager:info("[ director ] Bench name ~p, director node ~p", [BenchName, erlang:node()]),
    {Pools, Env2} = mzbl_script:extract_pools_and_env(Script, Env),
    lager:info("[ director ] Pools: ~p, Env: ~p", [Pools, Env2]),
    _ = mzb_signaler:set_nodes(Nodes),
    gen_server:cast(self(), {start_pools, Pools, Env2, Nodes}),
    {ok, #state{
        bench_name = BenchName,
        super_pid = SuperPid,
        reportfile = ReportFile
    }}.

handle_call(attach, From, State) ->
    maybe_stop(State#state{owner = From});

handle_call(Req, _From, State) ->
    lager:error("Unhandled call: ~p", [Req]),
    {stop, {unhandled_call, Req}, State}.

handle_cast({start_pools, _Pools, _Env, []}, State) ->
    lager:error("[ director ] There are no alive nodes to start workers"),
    {stop, empty_nodes, State};
handle_cast({start_pools, Pools, Env, Nodes}, #state{super_pid = SuperPid, bench_name = BenchName} = State) ->
    Metrics = get_metric_names(Pools, Nodes),
    {ok, _} = supervisor:start_child(SuperPid,
        {mzb_metrics,
         {mzb_metrics, start_link, [BenchName, Env, Metrics, Nodes, SuperPid]},
         transient, 5000, worker, [mzb_metrics]}),
    {noreply, State#state{
        pools = start_pools(Pools, Env, Nodes, [])
    }};

handle_cast({pool_report, PoolPid, Info, true}, #state{pools = Pools} = State) ->
    NewState = handle_pool_report(Info, State),
    catch erlang:demonitor(proplists:get_value(PoolPid, Pools), [flush]),
    NewPools = proplists:delete(PoolPid, Pools),
    maybe_stop(NewState#state{pools = NewPools});

handle_cast({pool_report, _PoolPid, Info, false}, #state{} = State) ->
    {noreply, handle_pool_report(Info, State)};

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

shift(Nodes, undefined) -> Nodes;
shift(Nodes, 0) -> Nodes;
shift(Nodes, Size) when length(Nodes) < Size -> shift(Nodes, Size rem length(Nodes));
shift(Nodes, Size) when Size > 0 -> {F, T} = lists:split(Size, Nodes), T ++ F.

maybe_stop(#state{pools = [], succeed = Ok, failed = NOk} = State) ->
    ok = mzb_metrics:final_trigger(),
    lager:info("[ director ] All pools have finished, stopping mzb_director_sup ~p", [State#state.super_pid]),
    lager:info("[ director ] Succeed/Failed workers = ~p/~p", [Ok, NOk]),
    FailedAsserts = mzb_metrics:get_failed_asserts(),
    case FailedAsserts of
        [] -> ok;
        _ ->
            AssertMessages = [Msg || {_, Msg} <- FailedAsserts],
            lager:error("[ director ] Failed assertions:~n~s", [string:join(AssertMessages, "\n")])
    end,
    case report_results(FailedAsserts, State) of
        ok ->
            erlang:spawn(fun mzb_sup:stop_bench/0),
            {stop, normal, State};
        {error, no_listener} ->
            lager:info("[ director ] Waiting for someone to report results..."),
            {noreply, State}
    end;

maybe_stop(#state{} = State) ->
    {noreply, State}.

report_results(_, #state{owner = undefined}) -> {error, no_listener};
report_results(FailedAsserts, #state{owner = Owner} = State) ->
    lager:info("[ director ] Reporting benchmark results to ~p", [Owner]),
    ok = report_file(State),
    Res = format_results(FailedAsserts, State),
    gen_server:reply(Owner, Res),
    ok.

format_results([], #state{succeed = Ok, failed = 0}) ->
    {ok, lists:flatten(io_lib:format("SUCCESS~n~b workers have finished successfully", [Ok]))};
format_results([], #state{succeed = Ok, failed = NOk}) ->
    {error, {workers_failed, NOk},
        lists:flatten(io_lib:format("FAILED~n~b of ~b workers failed", [NOk, Ok + NOk]))};
format_results(FailedAsserts, #state{}) when is_list(FailedAsserts) ->
    {error, {asserts_failed, length(FailedAsserts)},
        lists:flatten(io_lib:format("FAILED~n~b assertions failed", [length(FailedAsserts)]))}.

report_file(#state{reportfile = undefined}) -> ok;
report_file(#state{reportfile = File, succeed = Ok, failed = NOk}) ->
    file:write_file(File, io_lib:fwrite("~p/~p", [Ok, NOk])).

get_metric_names(Pools, Nodes) ->
    mzb_script_metrics:script_metrics(Pools, Nodes)
    ++ case lists:member(erlang:node(), Nodes) of
        true -> [];
        false -> mzb_system_load_monitor:metric_names([erlang:node()])
    end.
