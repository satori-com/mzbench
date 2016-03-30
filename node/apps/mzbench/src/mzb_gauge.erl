-module(mzb_gauge).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    create/1,
    notify/2,
    get_value/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(s, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create(Name) ->
    notify(Name, 0).

notify(Name, Value) ->
    ets:insert(?MODULE, {Name, Value}),
    ok.

get_value(Name) ->
    case ets:lookup(?MODULE, Name) of
        [{_, R}] -> R;
        [] -> erlang:error(not_found)
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    _ = ets:new(?MODULE, [named_table, set, public]),
    {ok, #s{}}.

handle_call(_Request, _From, State) ->
   {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

