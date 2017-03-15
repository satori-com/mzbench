-module(mzb_api_exclusive).

-export([start_link/0,
         lock/2,
         release/2]).

-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(s, {
    locks = #{} :: #{term() => integer()},
    queue = [] :: [{list(), integer(), pid()}]
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

lock(Id, Exclusive) ->
    gen_server:call(?MODULE, {lock, Id, Exclusive}, infinity).

release(Id, Exclusive) ->
    gen_server:call(?MODULE, {release, Id, Exclusive}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #s{}}.

handle_call({lock, Id, Exclusive}, From, #s{locks = Locks, queue = Queue} = State) ->
    case take_lock(Exclusive, Id, Locks) of
        {ok, NewLocks} -> {reply, ok, State#s{locks = NewLocks}};
        fail -> {noreply, State#s{queue = Queue ++ [{Exclusive, Id, From}]}}
    end;

handle_call({release, BenchId, Exclusive}, _From, #s{locks = Locks, queue = Queue} = State) ->
    UsedLockList = lists:filter(fun(X) -> mzb_bc:maps_get(X, Locks, -1) == BenchId end,
                                          string:tokens(Exclusive, ", ")),
    NewLocks = maps:without(UsedLockList, Locks),
    NewQueue = lists:filter(fun({_, Id, _}) -> Id /= BenchId end, Queue),
    {NewLocks2, NewQueue2} = lists:foldl(
        fun({E, Id, Pid}, {L, Q}) ->
            case take_lock(E, Id, L) of
                {ok, NewL} -> catch gen_server:reply(Pid, ok), {NewL, Q};
                fail -> {L, [{E, Id, Pid} | Q]}
            end
        end, {NewLocks, []}, NewQueue),
    {reply, ok, State#s{locks = NewLocks2, queue = lists:reverse(NewQueue2)}};

handle_call(Req, _From, State) ->
    lager:error("Unhandled call: ~p", [Req]),
    {stop, {unhandled_call, Req}, State}.

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
%%% internal functions
%%%===================================================================

take_lock(Exclusive, Id, Locks) ->
    LockList = string:tokens(Exclusive, ", "),
    case lists:any(fun(X) -> maps:is_key(X, Locks) end, LockList) of
        true -> fail;
        false -> {ok, lists:foldl(fun(X, A) -> maps:put(X, Id, A) end, Locks, LockList)}
    end.
