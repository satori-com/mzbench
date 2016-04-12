-module(mzb_gauge).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    create/1,
    notify/2,
    get_value/1,
    take_value/1
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

%% In order to support atomic take_value (and since there is no support for
%% ets:take/2 in R17 yet) we have to do notify and take_value synchronously.
%% Gauges are not suppposed to be notified very often so this should work
notify(Name, Value) ->
    gen_server:call(?MODULE, {notify, Name, Value}, infinity).

take_value(Name) ->
    case gen_server:call(?MODULE, {take_value, Name}, infinity) of
        {ok, V} -> V;
        not_found -> erlang:error(not_found)
    end.

get_value(Name) ->
    case ets:lookup(?MODULE, Name) of
        [{_, R}] -> R;
        [] -> erlang:error(not_found)
    end.

% Uncomment when R17 support is dropped (ets:take is unupported in R17)
%notify(Name, Value) ->
%    ets:insert(?MODULE, {Name, Value}),
%    ok.
%
%
%take_value(Name) ->
%    case ets:take(?MODULE, Name) of
%        [{_, R}] -> R;
%        [] -> erlang:error(not_found)
%    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    _ = ets:new(?MODULE, [named_table, set, public]),
    {ok, #s{}}.

handle_call({notify, Name, Value}, _From, State) ->
    ets:insert(?MODULE, {Name, Value}),
    {reply, ok, State};

handle_call({take_value, Name}, _From, State) ->
    case ets:lookup(?MODULE, Name) of
        [{_, R}] ->
            ets:delete(?MODULE, Name),
            {reply, {ok, R}, State};
        [] ->
            {reply, not_found, State}
    end;

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

