-module(mzb_signaler).

-export([start_link/0,
         add_signal/1,
         add_signal/2,
         check_signal/1,
         check_signal/2,
         check_signal/3,
         set_nodes/1,
         get_all_signals/0,
         add_local_signal/2]).

-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(s, {
    nodes = [] :: [node()],
    queue = [] :: [{string(), node(), non_neg_integer()}]
}).

-define(TICK_TIMER, 1000).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

check_signal(Name) ->
    check_signal(Name, 1).

check_signal(Name, Count) ->
    check_signal(Name, Count, infinity).

check_signal(Name, Count, Timeout) when Count > 0, is_number(Count) ->
    case ets:lookup(?MODULE, {signal, Name}) of
        [{_, Cn}] when Cn >= Count -> ok;
        _ ->
            try
                gen_server:call(?MODULE, {check, Name, Count}, Timeout)
            catch
                exit:{timeout, _} -> erlang:error({timeout, {wait_signal, Name}})
            end
    end;
check_signal(_, 0, _) -> ok;
check_signal(Name, Count, _) -> erlang:error({badarg, {wait_signal, Name, Count}}).

add_signal(Name) -> add_signal(Name, 1).

add_signal(Name, Count) ->
    case ets:lookup(?MODULE, {counter, Name}) of
        [] -> 
            {ok, Ref} = mz_counter:create_raw(),
            case ets:insert_new(?MODULE, {{counter, Name}, Ref}) of
                true -> mz_counter:notify_raw(Ref, Count);
                false ->
                    mz_counter:notify_raw(ets:lookup_element(?MODULE, {counter, Name}, 2), Count)
            end;
        [{_, Ref}] ->
            mz_counter:notify_raw(Ref, Count)
    end.

add_local_signal(Name, Count) ->
    gen_server:cast(?MODULE, {add_local, Name, Count}).

set_nodes(Nodes) ->
    gen_server:call(?MODULE, {set_nodes, Nodes}).

get_all_signals() ->
    ets:foldl(
        fun ({{signal, Name}, Value}, Acc) ->
            [{Name, Value} | Acc];
            (_, Acc) -> Acc
        end, [], ?MODULE).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    system_log:info("Signal server has been started"),
    _ = ets:new(?MODULE, [set, named_table, public, {read_concurrency, true}]),
    timer:send_after(?TICK_TIMER, self(), tick),
    {ok, #s{}}.

handle_call({set_nodes, Nodes}, _From, #s{} = State) ->
    {reply, {ok}, State#s{nodes = Nodes}};
handle_call({check, Name}, From, #s{queue = Q} = State) ->
    case length(ets:lookup(?MODULE, {signal, Name})) > 0 of
        true -> {reply, ok, State};
        _ -> _ = mzb_metrics:notify({"blocked.workers", counter}, 1),
             {noreply, State#s{queue = [{Name, From, 1} | Q]}}
    end;
handle_call({check, Name, Count}, From, #s{queue = Q} = State) ->
    case ets:lookup(?MODULE, {signal, Name}) of
        [{Name, Cn}] when Cn >= Count -> {reply, ok, State};
        _ -> _ = mzb_metrics:notify({"blocked.workers", counter}, 1),
             {noreply, State#s{queue = [{Name, From, Count} | Q]}}
    end;
handle_call(Req, _From, State) ->
    system_log:error("Unhandled call: ~p", [Req]),
    {stop, {unhandled_call, Req}, State}.

handle_cast({add_local, Name, Count}, #s{queue = Q} = State) ->
    NewC = case ets:lookup(?MODULE, {signal, Name}) of
        [{_, _Old}] -> ets:update_counter(?MODULE, {signal, Name}, Count);
        _ -> ets:insert(?MODULE, {{signal, Name}, Count}), Count
    end,
    W = [F || {N, F, C} <- Q, N == Name, C =< NewC],
    _ = mzb_metrics:notify({"blocked.workers", counter}, -length(W)),
    _ = lists:map(fun(From) -> gen_server:reply(From, ok) end, W),
    {noreply, State#s{queue = [{N, F, C} || {N, F, C} <- Q, (N =/= Name) or (C > NewC)]}};
handle_cast(Msg, State) ->
    system_log:error("Unhandled cast: ~p", [Msg]),
    {stop, {unhandled_cast, Msg}, State}.

handle_info(tick, #s{nodes = Nodes} = State) ->
    _ = ets:foldl(
        fun ({{counter, Name}, Ref}, Acc) ->
                Value = mz_counter:get_value_raw(Ref),
                mz_counter:notify_raw(Ref, -Value),
                Value /= 0 andalso (_ =  mzb_interconnect:abcast(Nodes, {add_signal, Name, Value})),
                Acc;
            (_, Acc) -> Acc
        end, [], ?MODULE),
    timer:send_after(?TICK_TIMER, self(), tick),
    {noreply, State};

handle_info(Info, State) ->
    system_log:error("Unhandled info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
