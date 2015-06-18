-module(mzb_pool).

-export([start_link/5,
         stop/1
        ]).

-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-include("mzb_types.hrl").
-include("mzb_ast.hrl").

-record(s, {
    director = undefined,
    workers  = [],
    succeed  = 0,
    failed   = 0,
    name     = undefined,
    worker_starter = undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Director, Pool, Env, NumNodes, Offset) ->
    gen_server:start_link(?MODULE, [Director, Pool, Env, NumNodes, Offset], []).

stop(Pid) ->
    gen_server:call(Pid, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Director, Pool, Env, NumNodes, Offset]) ->
    Tid = ets:new(pool_workers, [public, {keypos, 1}]),
    _ = random:seed(now()),
    State = #s{workers = Tid, director = Director},
    {ok, start_workers(Pool, Env, NumNodes, Offset, State)}.

handle_call(stop, _From, #s{workers = Tid, name = Name} = State) ->
    lager:info("[ ~p ] Received stop signal", [Name]),
    ets:foldl(
        fun ({Pid, Ref}, Acc) ->
            erlang:demonitor(Ref, [flush]),
            erlang:exit(Pid, kill),
            Acc
        end, [], Tid),
    ets:delete_all_objects(Tid),
    {stop, normal, ok, State = #s{}};

handle_call(Req, _From, State) ->
    lager:error("Unhandled call: ~p", [Req]),
    {stop, {unhandled_call, Req}, State}.

handle_cast({start_worker, WorkerScript, Env, Worker, Node, WId, NumWorkers, Self}, #s{workers = Tid} = State) ->
    {P, Ref} = erlang:spawn_monitor(fun() ->
        mzb_worker_runner:run_worker_script(WorkerScript, Env, Worker, Self, false)
        end),
    ets:insert(Tid, {P, Ref}),
    if
        WId < 4 orelse WId =:= NumWorkers -> lager:info("Starting worker on ~p no ~p", [Node, WId]);
        WId =:= 4 -> lager:info("Starting remaining workers...", []);
        true -> ok
    end,
    {noreply, State};

handle_cast(Msg, State) ->
    lager:error("Unhandled cast: ~p", [Msg]),
    {stop, {unhandled_cast, Msg}, State}.

handle_info({'DOWN', Ref, _, _, normal}, #s{worker_starter = {_, Ref}} = State) ->
    maybe_stop(State#s{worker_starter = undefined});

handle_info({'DOWN', Ref, _, _, Reason}, #s{worker_starter = {_, Ref}} = State) ->
    lager:error("Worker starter has crashed with the reason: ~p", [Reason]),
    {stop, Reason, State};

handle_info({worker_result, _Pid, {ok, _}}, #s{} = State) ->
    {noreply, State#s{succeed = State#s.succeed + 1}};

handle_info({worker_result, Pid, Res}, #s{} = State) ->
    maybe_report_error(Pid, Res),
    {noreply, State#s{failed = State#s.failed + 1}};

handle_info({'DOWN', _Ref, _, Pid, normal}, #s{workers = Workers} = State) ->
    ets:delete(Workers, Pid),
    maybe_stop(State);

handle_info({'DOWN', _Ref, _, Pid, Reason}, #s{workers = Workers, name = Name} = State) ->
    NewState = State#s{failed = State#s.failed + 1},
    case ets:lookup(Workers, Pid) of
        [{Pid, Ref}] ->
            lager:error("[ ~p ] Received DOWN from worker ~p with reason ~p", [Name, Pid, Reason]),
            ets:delete(Workers, Pid),
            erlang:demonitor(Ref, [flush]);
        _ ->
            lager:error("[ ~p ] Received DOWN from unknown process: ~p / ~p", [Name, Pid, Reason])
    end,
    maybe_stop(NewState);

handle_info(Info, State) ->
    lager:error("Unhandled info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_workers(Pool, Env, NumNodes, Offset, #s{} = State) ->
    #operation{name = pool, args = [PoolOpts, Script], meta = Meta} = Pool,
    Name = proplists:get_value(pool_name, Meta),
    [Size] = mzb_mproplists:get_value(size, PoolOpts, [undefined]),
    [PerNode] = mzb_mproplists:get_value(per_node, PoolOpts, [undefined]),
    [StartDelay] = mzb_mproplists:get_value(worker_start, PoolOpts, [undefined]),
    Size2 = case [mzb_utility:to_integer_with_default(Size, undefined), mzb_utility:to_integer_with_default(PerNode, undefined)] of
                        [undefined, undefined] -> 1;
                        [undefined, PN] -> PN * NumNodes;
                        [S, undefined] -> S;
                        [S, PN] when PN * Offset > S -> 0;
                        [S, PN] when NumNodes * PN >= S -> S;
                        [S, PN] ->
                            lager:error("Need more nodes, required = ~p, actual = ~p", 
                                [mzb_utility:int_ceil(S/PN), NumNodes]),
                            erlang:error({not_enough_nodes})
                    end,
    lager:info("Size, PerNode, Size2, Offset, NumNodes: ~p, ~p, ~p, ~p, ~p",
        [Size, PerNode, Size2, Offset, NumNodes]),
    Worker = mzb_script:extract_worker(PoolOpts),
    Self = self(),

    load_worker(Worker),

    Numbers = lists:seq(0, mzb_utility:int_ceil((Size2 - Offset + 1)/NumNodes) - 1),
    lager:info("Worker offsets: ~p", [Numbers]),

    WorkerStarter =
        erlang:spawn_monitor(fun () ->
            Node = node(),
            lists:map(fun(N) -> worker_start_delay(StartDelay, NumNodes),
                            WId = N * NumNodes + Offset,
                            WorkerScript = mzb_ast:add_meta(Script, [{worker_id, WId}]),
                            gen_server:cast(Self, {start_worker, WorkerScript, Env, Worker, Node, WId, Size2, Self})
                        end, Numbers)
        end),
    State#s{name = Name, worker_starter = WorkerStarter}.

load_worker({WorkerProvider, Worker}) ->
    case erlang:apply(WorkerProvider, load, [Worker]) of
        ok -> ok;
        {error, Reason} ->
            lager:error("Worker ~p load failed with reason: ~p", [Worker, Reason]),
            erlang:error({application_start_failed, Worker, Reason})
    end.

maybe_stop(#s{workers = Workers, name = Name, director = Director, worker_starter = undefined} = State) ->
    case ets:first(Workers) == '$end_of_table' of
        true ->
            lager:info("[ ~p ] All workers have finished", [Name]),
            Info = [{succeed_workers, State#s.succeed},
                    {failed_workers,  State#s.failed}],
            mzb_director:pool_report(Director, self(), Info, true),
            {stop, normal, State};
        false ->
            {noreply, State}
    end;
maybe_stop(#s{} = State) ->
    {noreply, State}.

maybe_report_error(_, {ok, _}) -> ok;
maybe_report_error(Pid, {error, Reason}) ->
    lager:error("Worker ~p has finished abnormally: ~p", [Pid, Reason]);
maybe_report_error(Pid, {exception, Node, {_C, E, ST}}) ->
    lager:error("Worker ~p on ~p has crashed: ~p~nStacktrace: ~p", [Pid, Node, E, ST]).

worker_start_delay(undefined, _) -> ok;
worker_start_delay(#operation{name = poisson, args = [#constant{value = Lambda, units = rps}]}, Factor) ->
    % The time between each pair of consecutive events has an exponential
    % distribution with parameter λ and each of these inter-arrival times
    % is assumed to be independent of other inter-arrival times.
    % (http://en.wikipedia.org/wiki/Poisson_process)
    SleepTime = -(1000*Factor*math:log(random:uniform()))/Lambda,
    timer:sleep(erlang:round(SleepTime));
worker_start_delay(#operation{name = linear, args = [#constant{value = RPS, units = rps}]}, Factor) ->
    timer:sleep(1000*Factor div RPS).

