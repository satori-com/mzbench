-module(mzb_metrics_tcp_protocol).

-behaviour(ranch_protocol).
-behaviour(gen_server).

%% API
-export([start_link/4]).

%% gen_server
-export([init/1,
         init/4,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(TIMEOUT, 60000).

-record(state, {socket, transport}).

%% API.

start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

init([State]) -> {ok, State}.

init(Ref, Socket, Transport, _Opts = []) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}, {packet, 4}]),

    gen_event:add_handler(metrics_event_manager, {mzb_exometer_report_apiserver, self()}, [self()]),
    gen_server:enter_loop(?MODULE, [], #state{socket=Socket, transport=Transport}, ?TIMEOUT).

handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};

handle_info(timeout, State) ->
    {stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
    lager:warning("~p was closed with error: ~p", [?MODULE, Reason]),
    {stop, Reason, State};

handle_info(Info, State) ->
    lager:error("~p has received unexpected info: ~p", [?MODULE, Info]),
    {stop, normal, State}.

handle_cast(Msg, State) ->
    lager:error("~p has received unexpected cast: ~p", [?MODULE, Msg]),
    {noreply, State}.

handle_call({report, [Probe, DataPoint, Value]}, _From, State = #state{socket=Socket, transport=Transport}) ->
    Name = name([P || P <- Probe, P /= []], DataPoint),
    Metric = io_lib:format("~B\t~s\t~p~n", [unix_time(), Name, Value]),
    Transport:send(Socket, Metric),
    {reply, ok, State};

handle_call(Request, _From, State) ->
    lager:error("~p has received unexpected call: ~p", [?MODULE, Request]),
    {reply, ignore, State}.

terminate(_Reason, _State) ->
    gen_event:delete_handler(metrics_event_manager, {mzb_exometer_report_apiserver, self()}, []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

name(Probe, DataPoint) -> [[[str(I), $.] || I <- Probe], str(DataPoint)].

str(V) when is_atom(V) -> atom_to_list(V);
str(V) when is_binary(V) -> binary_to_list(V);
str(V) when is_integer(V) -> integer_to_list(V);
str(V) when is_list(V) -> V.

unix_time() ->
    {Mega, Secs, _} = os:timestamp(),
    Mega * 1000000 + Secs.
