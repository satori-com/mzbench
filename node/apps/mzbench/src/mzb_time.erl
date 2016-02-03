-module(mzb_time).

-export([start_link/0,
         timestamp/0,
         get_offset/0,
         synchronize_time_with_director/1, 
         get_local_timestamp/0]).

-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
    offset = 0 :: integer()
}).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec timestamp() -> {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
timestamp() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    {MegaSecs, Secs, MicroSecs + get_offset()}.

-spec get_offset() -> integer().
get_offset() ->
    {ok, Offset} = gen_server:call(?MODULE, get_offset),
    Offset.

-spec synchronize_time_with_director(string()) -> ok.
synchronize_time_with_director(DirectorNode) ->
    gen_server:cast(?MODULE, {synchronize_time_with_director, DirectorNode}).

-spec get_local_timestamp() -> {ok, {non_neg_integer(), non_neg_integer(), non_neg_integer()}}.
get_local_timestamp() ->
    {ok, os:timestamp()}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec init([]) -> {ok, #state{}}.
init([]) ->
    {ok, #state{}}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> term().
handle_call(get_offset, _From, #state{offset = Offset} = State) ->
    {reply, {ok, Offset}, State};
handle_call(Req, _From, State) ->
    system_log:error("Unhandled call: ~p", [Req]),
    {stop, {unhandled_call, Req}, State}.

-spec handle_cast(term(), #state{}) -> term().
handle_cast({synchronize_time_with_director, DirectorNode}, State) ->
    LocalTimestamp1 = os:timestamp(),
    {ok, DirectorTimestamp} = rpc:call(DirectorNode, mzb_time, get_local_timestamp, []),
    LocalTimestamp2 = os:timestamp(),
    
    RTT = timer:now_diff(LocalTimestamp2, LocalTimestamp1),
    Offset = timer:now_diff(DirectorTimestamp, LocalTimestamp1) + RTT/2,
    
    system_log:info("[ mzb_time ] Timestamp offset between the node ~p and the director is ~p microseconds", [erlang:node(), Offset]),
    {noreply, State#state{offset = Offset}};
handle_cast(Msg, State) ->
    system_log:error("Unhandled cast: ~p", [Msg]),
    {stop, {unhandled_cast, Msg}, State}.

-spec handle_info(timeout | term(), #state{}) -> term().
handle_info(Info, State) ->
    system_log:error("Unhandled info: ~p", [Info]),
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
