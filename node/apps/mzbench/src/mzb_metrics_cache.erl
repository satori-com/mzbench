-module(mzb_metrics_cache).

-export([start_link/0,
         check_cached_declare/1,
         get_value/1]).

-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(ETS_FOR_VALUES, mzb_metrics_cache_vals).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

check_cached_declare(Groups) ->
    case ets:lookup(?MODULE, Groups) of
        [] -> ets:insert(?MODULE, {Groups, true}),
              false;
        _  -> true
    end.

get_value(Name) ->
    case ets:lookup(?ETS_FOR_VALUES, Name) of
        [] -> 0;
        [{_, Val}] -> Val
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    _ = ets:new(?MODULE, [set, public, named_table, {read_concurrency, true}]),
    _ = ets:new(?ETS_FOR_VALUES, [set, public, named_table, {read_concurrency, true}]),
    {ok, nostate}.

handle_call(Req, _From, State) ->
    system_log:error("Unhandled call: ~p", [Req]),
    {stop, {unhandled_call, Req}, State}.

handle_cast({cache_metric, Name, Value}, State) ->
    ets:insert(?ETS_FOR_VALUES, {Name, Value}),
    {noreply, State};

handle_cast(Msg, State) ->
    system_log:error("Unhandled cast: ~p", [Msg]),
    {stop, {unhandled_cast, Msg}, State}.

handle_info(Info, State) ->
    system_log:error("Unhandled info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
