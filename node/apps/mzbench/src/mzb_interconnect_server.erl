-module(mzb_interconnect_server).

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

-record(state, {
    socket,
    transport,
    init_timer
}).

-define(INIT_WAIT_MSEC, 5000).

start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).


init([State]) -> {ok, State}.

init(Ref, Socket, Transport, _Opts) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}, {packet, 4}, {keepalive, true}, binary]),
    Timer = erlang:send_after(?INIT_WAIT_MSEC, self(), init_timer_expired),
    gen_server:enter_loop(?MODULE, [], #state{socket=Socket, transport=Transport, init_timer = Timer}).

dispatch({init, NodeName, Role}, #state{socket = Socket, transport = Transport, init_timer = Timer} = State) ->
    system_log:info("Received init from ~p ~p", [Role, NodeName]),
    Sender = fun (Msg) -> send(Transport, Socket, Msg) end,
    case mzb_interconnect:accept_connection(NodeName, Role, self(), Sender) of
        {ok, MyRole} ->
            erlang:cancel_timer(Timer),
            send(Transport, Socket, {init, node(), MyRole}),
            {noreply, State#state{init_timer = undefined}};
        {error, _} ->
            Transport:close(Socket),
            {stop, normal, State}
    end;

dispatch(Msg, State) ->
    mzb_interconnect:handle(Msg),
    {noreply, State}.

handle_info(init_timer_expired, #state{socket = Socket, transport = Transport} = State) ->
    Transport:close(Socket),
    {stop, normal, State};

handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};

handle_info(timeout, State) ->
    {stop, normal, State};

handle_info({tcp, Socket, Msg}, State = #state{socket = Socket}) ->
    inet:setopts(Socket, [{active,once}]),
    dispatch(erlang:binary_to_term(Msg), State);

handle_info({tcp_error, _, Reason}, State) ->
    system_log:warning("~p was closed with reason: ~p", [?MODULE, Reason]),
    {stop, Reason, State};

handle_info(Info, State) ->
    system_log:error("~p has received unexpected info: ~p", [?MODULE, Info]),
    {stop, normal, State}.

handle_cast(Msg, State) ->
    system_log:error("~p has received unexpected cast: ~p", [?MODULE, Msg]),
    {noreply, State}.

handle_call(Request, _From, State) ->
    system_log:error("~p has received unexpected call: ~p", [?MODULE, Request]),
    {reply, ignore, State}.

terminate(_Reason, #state{} = _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

send(Transport, Socket, Msg) ->
    Transport:send(Socket, erlang:term_to_binary(Msg)).
