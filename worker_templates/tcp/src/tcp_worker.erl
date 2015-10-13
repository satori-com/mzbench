-module(tcp_worker).
-export([
    initial_state/0,
    metrics/0,
    connect/4,
    connect/5,
    send/3,
    recv/2,
    recv/3,
    start_receivers/6,
    start_receivers/7,
    disconnect/2,
    start_accepter/3
]).

-include("../../common_apps/mzbench_language/include/mzbl_types.hrl").
-include("../../node/apps/mzbench/include/mzb_ast.hrl").

-define(default_connect_timeout, 10000).
-define(default_recv_timeout, 5000).
-define(default_backlog, 20).

-record(s, {
    socket        = undefined,
    address       = undefined,
    port          = undefined,
    timeout       = undefined,
    last_msg_sent = undefined
}).

initial_state() -> #s{}.

metrics() ->
    [{"reconnect", counter},
     {"succ", counter},
     {"rcv-throughput", counter},
     {"reconnect", counter},
     {"timeout", counter},
     {"snd-throughput", counter},
     {"latency_us", histogram}
    ].

connect(State, Meta, Address, Port) ->
    connect(State, Meta, Address, Port, ?default_connect_timeout).

connect(#s{} = State, _Meta, Address, Port, Timeout) ->
    lager:info("Connecting to ~p:~p", [Address, Port]),
    reconnect(State#s{address = Address, port = Port, timeout = Timeout}).

reconnect(#s{socket = S, address = Address, port = Port, timeout = Timeout} = State) ->
    catch gen_tcp:close(S),
    try gen_tcp:connect(Address, Port, [{active, false}, {packet, 4}], Timeout) of
        {ok, Socket} ->
            {ok, State#s{socket = Socket}};

        {error, Reason} ->
            lager:error("Tcp connect failed with reason ~p: ~p", [Reason, inet:format_error(Reason)]),
            erlang:error({tcp_connect_failed, Reason})
    catch
        C:E ->
            ST = erlang:get_stacktrace(),
            lager:error("Tcp connect has crashed: ~p~n~p", [E, ST]),
            erlang:raise(C,E,ST)
    end.

disconnect(#s{socket = S} = State, _Meta) ->
    catch gen_tcp:close(S),
    {ok, State#s{socket = undefined}}.

send(#s{socket = S} = State, Meta, Message) ->
    Now = timestamp_us(),
    try gen_tcp:send(S, Message) of
        ok ->
            mzb_metrics:notify("snd-throughput", iolist_size(Message)),
            mzb_metrics:notify("succ", 1),
            {ok, State#s{last_msg_sent = Now}};
        {error, closed} ->
            mzb_metrics:notify(Meta, "reconnect", 1),
            reconnect(State);
        {error, Reason} ->
            case inet:sockname(S) of
                {error, einval} ->
                    mzb_metrics:notify("reconnect", 1),
                    reconnect(State);
                _ ->
                    lager:error("Tcp send failed with reason ~p: ~s", [Reason, inet:format_error(Reason)]),
                    erlang:error({tcp_send_failed, Reason})
            end
    catch
        C:E ->
            ST = erlang:get_stacktrace(),
            lager:error("Tcp send has crashed: ~p~n~p", [E, ST]),
            erlang:raise(C,E,ST)
    end.

recv(State, Meta) ->
    recv(State, Meta, ?default_recv_timeout).

recv(#s{socket = S, last_msg_sent = Then} = State, _Meta, Timeout) ->
    case gen_tcp:recv(S, 0, Timeout) of
        {ok, Packet} ->
            Now = timestamp_us(),
            PacketLength = if is_list(Packet)   -> length(Packet);
                              is_binary(Packet) -> byte_size(Packet)
                           end,
            mzb_metrics:notify("rcv-throughput", PacketLength),
            mzb_metrics:notify({"latency_us", histogram}, Now - Then),
            {ok, State};
        {error, closed} ->
            mzb_metrics:notify("reconnect", 1),
            reconnect(State);
        {error, timeout} ->
            mzb_metrics:notify("timeout", 1),
            {ok, State};
        {error, Reason} ->
            case inet:sockname(S) of
                {error, einval} ->
                    mzb_metrics:notify("reconnect", 1),
                    reconnect(State);
                _ ->
                    lager:error("Tcp recv failed with reason ~p: ~s", [Reason, inet:format_error(Reason)]),
                    erlang:error({tcp_recv_failed, Reason})
            end
    end.

%% WorkingTime
%%     = ignore  (ignore data sent, default)
%%     | ack     (acknowledge with 'ok')
start_receivers(State, Meta, Port, ReceiversNum, ReconnectTimeout, Time) ->
    start_receivers(State, Meta, Port, ReceiversNum, ReconnectTimeout, Time, ignore).

start_receivers(#s{} = State, _Meta, Port, ReceiversNum, ReconnectTimeout, Time, RecvAction) when ReceiversNum >= 0 ->
    #constant{value = WorkingTime, units = ms} = mzb_literals:convert(Time),
    ListenSock = spawn_receivers(Port, ReceiversNum, ReconnectTimeout, RecvAction),
    timer:sleep(WorkingTime),
    gen_tcp:close(ListenSock),
    {ok, State}.

spawn_receivers(Port, ReceiversNum, ReconnectTimeout, RecvAction) ->
    Options =
        [{active, false},
         {packet, 4},
         {backlog, ?default_backlog},
         {ip, {0,0,0,0}},
         {reuseaddr, true}],

    lager:info("Listening to ~p...", [Port]),
    try gen_tcp:listen(Port, Options) of
        {ok, ListenSock} ->
            start_servers(ReceiversNum, ListenSock, ReconnectTimeout, RecvAction, []),
            ListenSock;
        {error,Reason} ->
            lager:error("Tcp listen failed with reason: ~p, ~s", [Reason, inet:format_error(Reason)]),
            erlang:error({tcp_listen_failed, Reason})
    catch
        C:E ->
            ST = erlang:get_stacktrace(),
            lager:error("Tcp listen has crashed: ~p~n~p", [E, ST]),
            erlang:raise(C,E,ST)
    end.

start_servers(0, _, _, _, Servers) -> Servers;
start_servers(ReceiversNum, ListenSock, ReconnectTimeout, RecvAction, Servers) ->
    Pid = spawn_link(?MODULE, start_accepter, [ListenSock, ReconnectTimeout, RecvAction]),
    start_servers(ReceiversNum - 1, ListenSock, ReconnectTimeout, RecvAction, [Pid|Servers]).

start_accepter(ListenSock, ReconnectTimeout, RecvAction) ->
    accepter(ListenSock, ReconnectTimeout, RecvAction).

accepter(ListenSock, ReconnectTimeout, RecvAction) ->
    case gen_tcp:accept(ListenSock) of
        {ok, S} when ReconnectTimeout == infinity ->
            receiver(S, RecvAction),
            accepter(ListenSock, ReconnectTimeout, RecvAction);
        {ok, S} ->
            {ok, TRef} = timer:apply_after(ReconnectTimeout, gen_tcp, close, [S]),
            receiver(S, RecvAction),
            timer:cancel(TRef),
            accepter(ListenSock, ReconnectTimeout, RecvAction);
        {error, closed} ->
            ok;
        {error, Reason} ->
            lager:error("Tcp accept failed with reason ~p: ~s", [Reason, inet:format_error(Reason)]),
            erlang:error({tcp_accept_failed, Reason})
    end.

receiver(S, Action) ->
    case gen_tcp:recv(S, 0) of
        {ok, _Data} ->
            case Action of
                ignore -> ok;
                ack -> gen_tcp:send(S, <<"ok">>)
            end,
            receiver(S, Action);
        {error, closed} ->
            ok;
        {error, Reason} ->
            lager:error("Tcp recv failed with reason ~p", [Reason]),
            catch gen_tcp:close(S)
    end.

%% Get microseconds since epoch.
timestamp_us() ->
    {Megasecs, Secs, Microsecs} = os:timestamp(),
    (Megasecs*1000000 + Secs)*1000000 + Microsecs.
