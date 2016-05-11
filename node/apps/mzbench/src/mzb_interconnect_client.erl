-module(mzb_interconnect_client).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(s, {
    socket,
    init_timer
}).

-define(CONNECT_TIMEOUT_MSEC, 5000).
-define(RECONNECT_TIMEOUT_MSEC, 5000).
-define(INIT_WAIT_MSEC, 5000).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Host, Port) ->
    gen_server:start_link(?MODULE, [Host, Port], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Host, Port]) ->
    gen_server:cast(self(), {connect, Host, Port}),
    {ok, #s{}}.

dispatch({init, NodeName}, #s{socket = Socket, init_timer = Timer} = State) ->
    case mzb_interconnect:accept_connection(NodeName, self(), fun (Term) -> send(Socket, Term) end) of
        true ->
            erlang:cancel_timer(Timer),
            {noreply, State#s{init_timer = undefined}};
        false ->
            gen_tcp:close(Socket),
            {stop, normal, State}
    end.

handle_call(_Request, _From, State) ->
   {noreply, State}.

handle_cast({connect, Host, Port}, State) ->
    system_log:info("Connecting to node at ~p:~p", [Host, Port]),
    case gen_tcp:connect(Host, Port, [{packet, 4}, {active, once}, binary], ?CONNECT_TIMEOUT_MSEC) of
        {ok, Socket} ->
            system_log:info("Connected to node at ~p:~p", [Host, Port]),
            send(Socket, {init, node()}),
            {noreply, State#s{socket = Socket, init_timer = erlang:send_after(?INIT_WAIT_MSEC, self(), init_timer_expired)}};
        {error, Reason} ->
            system_log:error("Could not connect to node at ~p:~p with reason: ~p", [Host, Port, Reason]),
            timer:sleep(?RECONNECT_TIMEOUT_MSEC),
            {stop, {connect_failed, Host, Port, Reason}, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Data}, #s{socket = Socket} = State) ->
    inet:setopts(Socket, [{active,once}]),
    dispatch(erlang:binary_to_term(Data), State);

handle_info({tcp_closed, Socket}, #s{socket = Socket} = State) ->
    {stop, normal, State};

handle_info({tcp_error, Socket, Reason}, #s{socket = Socket} = State) ->
    system_log:error("Socket closed with reason: ~p", [Reason]),
    {stop, {socket_error, Reason}, State};

handle_info(init_timer_expired, #s{socket = Socket} = State) ->
    gen_tcp:close(Socket),
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

send(Socket, Term) -> gen_tcp:send(Socket, erlang:term_to_binary(Term)).
