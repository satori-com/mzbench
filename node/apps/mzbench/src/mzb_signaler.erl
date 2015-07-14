-module(mzb_signaler).

-export([start_link/0,
         add_signal/1,
         add_signal/2,
         check_signal/1,
         check_signal/2,
         set_nodes/1,
         get_all_signals/0]).

-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(s, {
    nodes = [],
    queue = []
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

check_signal(Name) ->
    check_signal(Name, 1).

check_signal(Name, Count) ->
    case ets:lookup(?MODULE, Name) of
        [{_, Cn}] when Cn >= Count -> ok;
        _ -> gen_server:call(?MODULE, {check, Name, Count}, infinity)
    end.

add_signal(Name) ->
    gen_server:cast(?MODULE, {add, Name}).

add_signal(Name, Count) when Count > 0, is_number(Count) ->
    gen_server:cast(?MODULE, {add, Name, Count}).

set_nodes(Nodes) ->
    Expected = {[{N, {ok}} || N <- Nodes], []},
    Expected = gen_server:multi_call(Nodes, ?MODULE, {set_nodes, Nodes}).

get_all_signals() ->
    ets:foldl(fun(Signal, Acc) -> [Signal | Acc] end, [], ?MODULE).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    lager:info("Signal server has been started"),
    _ = ets:new(?MODULE, [set, named_table, {read_concurrency, true}]),
    {ok, #s{}}.

handle_call({set_nodes, Nodes}, _From, #s{} = State) ->
    lager:info("Nodes are: ~p~n", [Nodes]),
    {reply, {ok}, State#s{nodes = Nodes}};
handle_call({check, Name}, From, #s{queue = Q} = State) ->
    case length(ets:lookup(?MODULE, Name)) > 0 of
        true -> {reply, ok, State};
        _ -> {noreply, State#s{queue = [{Name, From, 1} | Q]}}
    end;
handle_call({check, Name, Count}, From, #s{queue = Q} = State) ->
    case ets:lookup(?MODULE, Name) of
        [{Name, Cn}] when Cn >= Count -> {reply, ok, State};
        _ -> {noreply, State#s{queue = [{Name, From, Count} | Q]}}
    end;
handle_call(Req, _From, State) ->
    lager:error("Unhandled call: ~p", [Req]),
    {stop, {unhandled_call, Req}, State}.

handle_cast({add, Name}, #s{nodes = Nodes} = State) ->
    _ = gen_server:abcast(Nodes, ?MODULE, {add_local, Name, 1}),
    {noreply, State};
handle_cast({add, Name, Count}, #s{nodes = Nodes} = State) ->
    _ = gen_server:abcast(Nodes, ?MODULE, {add_local, Name, Count}),
    {noreply, State};
handle_cast({add_local, Name, Count}, #s{queue = Q} = State) ->
    NewC = case ets:lookup(?MODULE, Name) of
        [{_, Old}] -> ets:update_counter(?MODULE, Name, Count);
        _ -> ets:insert(?MODULE, {Name, Count}), Count
    end,
    W = [F || {N, F, C} <- Q, N == Name, C =< NewC],
    _ = lists:map(fun(From) -> gen_server:reply(From, ok) end, W),
    {noreply, State#s{queue = [{N, F, C} || {N, F, C} <- Q, (N =/= Name) or (C > NewC)]}};
handle_cast(Msg, State) ->
    lager:error("Unhandled cast: ~p", [Msg]),
    {stop, {unhandled_cast, Msg}, State}.

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
