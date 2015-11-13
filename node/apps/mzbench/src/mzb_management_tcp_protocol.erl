-module(mzb_management_tcp_protocol).

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
         code_change/3,
         get_port/0]).

-define(TIMEOUT, 60000).

-record(state, {
    socket :: any(),
    transport :: module()
    }).

%% API.

start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

dispatch({request, Ref, Msg}, State) ->
    lager:info("Received request: ~p", [Msg]),
    case handle_message(Msg) of
        {ok, Res} -> send_message({response, Ref, Res}, State);
        {error, Reason} -> lager:error("Api server message handling error: ~p, Reason: ~p", [Msg, Reason])
    end,
    {noreply, State};

dispatch(close_req, #state{socket = Socket, transport = Transport} = State) ->
    Transport:close(Socket),
    {stop, normal, State};

dispatch(Unhandled, State) ->
    lager:error("Unhandled tcp message: ~p", [Unhandled]),
    {noreply, State}.

get_port() ->
    ranch:get_port(management_tcp_server).

handle_message({change_env, Env}) ->
    {ok, mzb_director:change_env(Env)};

handle_message({get_log_port, Node}) ->
    case rpc:call(Node, mzb_lager_tcp_protocol, get_port, []) of
        {badrpc, Reason} -> {error, {badrpc, Node, Reason}};
        Port -> {ok, Port}
    end;

handle_message(Msg) ->
    {error, {unhandled, Msg}}.

init([State]) -> {ok, State}.

init(Ref, Socket, Transport, _Opts = []) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, true}, {packet, 4}, binary]),

    gen_event:add_handler(metrics_event_manager, {mzb_exometer_report_apiserver, self()}, [self()]),
    gen_server:enter_loop(?MODULE, [], #state{socket=Socket, transport=Transport}, ?TIMEOUT).

handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};

handle_info(timeout, State) ->
    {stop, normal, State};

handle_info({tcp, Socket, Msg}, State = #state{socket = Socket}) ->
    dispatch(erlang:binary_to_term(Msg), State);

handle_info({tcp_error, _, Reason}, State) ->
    lager:warning("~p was closed with error: ~p", [?MODULE, Reason]),
    {stop, Reason, State};

handle_info(Info, State) ->
    lager:error("~p has received unexpected info: ~p", [?MODULE, Info]),
    {stop, normal, State}.

handle_cast(Msg, State) ->
    lager:error("~p has received unexpected cast: ~p", [?MODULE, Msg]),
    {noreply, State}.

handle_call({report, [Probe, DataPoint, Value]}, _From, State = #state{}) ->
    Name = name([P || P <- Probe, P /= []], DataPoint),
    Metric = io_lib:format("~B\t~s\t~p~n", [unix_time(), Name, Value]),
    send_message({metric_values, Metric}, State),
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

send_message(Msg, #state{socket = Socket, transport = Transport}) ->
    Transport:send(Socket, erlang:term_to_binary(Msg)).
