-module(mzb_time).

-export([start_link/0,
         timestamp/0,
         system_time/1,
         get_offset/0]).

-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-compile({inline,[get_offset/0]}).

-record(state, {
    sync_interval_ms
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

-spec system_time(Units :: micro_seconds) -> non_neg_integer().
system_time(micro_seconds) ->
    erlang:system_time(micro_seconds) + get_offset().

-spec get_offset() -> integer().
get_offset() ->
    case erlang:get(mzb_time_offset_ref) of
        undefined ->
            Ref = ets:lookup_element(?MODULE, offset_ref, 2),
            erlang:put(mzb_time_offset_ref, Ref),
            mz_counter:get_value_raw(Ref);
        Ref ->
            mz_counter:get_value_raw(Ref)
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec init([]) -> {ok, #state{}}.
init([]) ->
    _ = ets:new(?MODULE, [set, named_table, {read_concurrency, true}]),
    {ok, CounterRef} = mz_counter:create_raw(),
    ets:insert(?MODULE, {offset_ref, CounterRef}),
    random:seed(erlang:now()),
    ok = perform_sync(),
    TimeSyncIntervalMS = 1000 * application:get_env(mzbench, time_sync_interval_s, undefined),
    erlang:send_after(random:uniform(TimeSyncIntervalMS), self(), sync),
    {ok, #state{sync_interval_ms = TimeSyncIntervalMS}}.

handle_call(Req, _From, State) ->
    system_log:error("Unhandled call: ~p", [Req]),
    {stop, {unhandled_call, Req}, State}.

-spec handle_cast(term(), #state{}) -> term().
handle_cast(Msg, State) ->
    system_log:error("Unhandled cast: ~p", [Msg]),
    {stop, {unhandled_cast, Msg}, State}.

-spec handle_info(timeout | term(), #state{}) -> term().
handle_info(sync, #state{sync_interval_ms = SyncIntervalMs} = State) ->
    ok = perform_sync(),
    erlang:send_after(SyncIntervalMs + random:uniform(SyncIntervalMs div 10), self(), sync),
    {noreply, State};

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

-spec perform_sync() -> ok.
perform_sync() ->
    case mzb_interconnect:get_director() of
        undefined -> ok;
        Director when Director == node() -> ok;
        Director ->
            Offset = evaluate_time_offset(Director, 20),
            set_offset(Offset)
    end,
    ok.

set_offset(Offset) ->
    Ref = ets:lookup_element(?MODULE, offset_ref, 2),
    OldOffset = mz_counter:get_value_raw(Ref),
    mz_counter:notify_raw(Ref, Offset - OldOffset).

-spec evaluate_time_offset(Node :: atom(), SleepBetweenAttempts :: non_neg_integer()) -> Offset :: integer().
evaluate_time_offset(Node, SleepInterval) ->
    CalcOffset =
        fun () ->
            LocalTimestamp1 = os:timestamp(),
            DirectorTimestamp = mzb_interconnect:call(Node, get_local_timestamp),
            LocalTimestamp2 = os:timestamp(),

            RTT = timer:now_diff(LocalTimestamp2, LocalTimestamp1),
            Offset = timer:now_diff(DirectorTimestamp, LocalTimestamp1) - RTT div 2,
            timer:sleep(SleepInterval),
            {RTT, Offset}
        end,

    Res = [ CalcOffset() || _ <- lists:seq(1, 10) ],
    {_RoundTripTime, TimeOffset} = lists:min(Res),

    TimeOffset.

