-module(xmpp_worker).

-export([
    initial_state/0,
    metrics/0
]).

-export([
    connect/6,
    initial_presence/2,
    initial_presence/3,
    close/2,
    enter_room/3,
    enter_room/4,
    send_message/4,
    send_message/5,
    send_muc_message/4,
    send_muc_message/5,
    spawn_stream_parser/5,
    multi_enter_room/4,
    round_robin_room_id/2,
    set_muc_service/3,
    pool_id/2,
    iname/4,
    get_room_ids/4,
    recepient/5,
    marker/2,
    marker/3
]).

-define(XMPP_RESOURCE, "mzbench").
-define(XMPP_PASS, "password").
-define(XMPP_DEFAULT_MSG_SIZE, 64).

-record(state, {socket, muc_service, jid, username, domain, stream_reader, room_ids = [], current_room_ids = []}).

metrics() ->
    [
        {group, "General", [
            {graph, #{metrics => [{"xmpp.send-packets", counter}]}},
            {graph, #{metrics => [{"xmpp.recv-stanzas", counter}]}},
            {graph, #{metrics => [{"xmpp.connection-closed", counter}]}}
        ]},

        {group, "XMPP Sender", [
            {graph, #{title   => "Handshakes",
                      metrics => [{"xmpp.send.handshake", counter}]}},
            {graph, #{title   => "Enter Room",
                      metrics => [{"xmpp.send.enter-room", counter}]}},
            {graph, #{title   => "Enter Room Latency",
                      metrics => [{"xmpp.send.enter-room-latency", histogram}]}},
            {graph, #{title   => "Initial Presence Latency",
                      metrics => [{"xmpp.send.initial-presence-latency", histogram}]}},
            {graph, #{title   => "Messages",
                      metrics => [{"xmpp.send.msg", counter}]}},
            {graph, #{title   => "MUC Messages",
                      metrics => [{"xmpp.send.muc-msg", counter}]}}
        ]},

        {group, "XMPP Parser (you should enable corresonding parsers to see metrics)", [
            {graph, #{title   => "Message latencies",
                      units   => "msec",
                      metrics => [{"xmpp.parser.latency", histogram}]}},
            {graph, #{title   => "MUC Messages",
                      metrics => [{"xmpp.parser.muc-msg", counter}]}},
            {graph, #{title   => "Private Messages",
                      metrics => [{"xmpp.parser.msg", counter}]}},
            {graph, #{title   => "Error Messages",
                      metrics => [{"xmpp.parser.error-msg", counter}]}}]}
    ].

initial_state() -> #state{}.

connect(State, _Meta, Username, Domain, Host, Port) ->
    Socket = xmpp_protocol:connect(Host, Port),
    xmpp_protocol:handshake(Username, ?XMPP_PASS, ?XMPP_RESOURCE, Domain, Socket),
    mzb_metrics:notify({"xmpp.send.handshake", counter}, 1),
    {ok, State#state{socket = Socket, domain = Domain, username = Username}}.

initial_presence(State, Meta) -> initial_presence(State, Meta, "<presence").
initial_presence(State = #state{socket = Socket}, _Meta, ResponsePolicy) ->
    StartTime = xmpp_protocol:ts_to_longts(os:timestamp()),
    xmpp_protocol:send_packet({presence}, ResponsePolicy, Socket),
    EndTime = xmpp_protocol:ts_to_longts(os:timestamp()),
    mzb_metrics:notify({"xmpp.send.initial-presence-latency", histogram}, EndTime - StartTime),
    {ok, State}.

close(State = #state{socket = Socket, stream_reader = Pid}, _Meta) ->
    close_stream_reader(Pid),
    xmpp_protocol:send_packet({close}, skip, Socket),
    catch gen_tcp:close(Socket),
    {ok, State#state{socket = undefined, stream_reader = undefined}}.


enter_room(State, Meta, RoomName) -> enter_room(State, Meta, RoomName, any).
enter_room(#state{username = Username, socket = Socket} = State, _Meta, RoomName, ResponsePolicy) ->
    StartTime = xmpp_protocol:ts_to_longts(os:timestamp()),

    MucService = get_muc_service(State),
    xmpp_protocol:send_packet({muc_join, RoomName, MucService, Username}, ResponsePolicy, Socket),
    mzb_metrics:notify({"xmpp.send.enter-room", counter}, 1),

    EndTime = xmpp_protocol:ts_to_longts(os:timestamp()),
    mzb_metrics:notify({"xmpp.send.enter-room-latency", histogram}, EndTime - StartTime),
    {ok, State}.

send_message(State, Meta, ToUser, Message) ->
    send_message(State, Meta, ToUser, Message, any).
send_message(#state{domain = Domain, socket = Socket} = State, _Meta, ToUser, Message, ResponsePolicy) ->
    Service = [Domain, "/", ?XMPP_RESOURCE],
    xmpp_protocol:send_packet({message, ToUser, Service, Message}, ResponsePolicy, Socket),
    mzb_metrics:notify({"xmpp.send.msg", counter}, 1),
    {ok, State}.

send_muc_message(State, Meta, Room, Message) ->
    send_muc_message(State, Meta, Room, Message, any).
send_muc_message(#state{socket = Socket} = State, _Meta, Room, Message, ResponsePolicy) ->
    MucService = get_muc_service(State),
    xmpp_protocol:send_packet({muc_chat, Room, MucService, Message}, ResponsePolicy, Socket),
    mzb_metrics:notify({"xmpp.send.muc-msg", counter}, 1),
    {ok, State}.

spawn_stream_parser(#state{socket = Socket} = State, _Meta, Buffer, Timeout, ParserKinds) ->
    ParserFns = [xmpp_protocol:get_stanza_parser(P) || P <- ParserKinds],
    Pid = spawn_link(xmpp_protocol, stream_parser, [Socket, Buffer, Timeout, <<>>, ParserFns]),
    {ok, State#state{stream_reader = Pid}}.

multi_enter_room(State, Meta, Name, Ids) ->
    lists:foreach(fun (Id) ->
        {RoomName, State} = iname(State, Meta, Name, Id),
        {ok, State} = enter_room(State, Meta, RoomName, any)
    end, Ids),
    {ok, State#state{room_ids = Ids}}.

round_robin_room_id(#state{current_room_ids = [], room_ids = []}, _Meta) ->
    erlang:error({empty_room_ids, "You should set room ids first"});
round_robin_room_id(#state{current_room_ids = [], room_ids = [Id | Rest]} = State, _Meta) ->
    {Id, State#state{current_room_ids = Rest}};
round_robin_room_id(#state{current_room_ids = [Id | Rest]} = State, _Meta) ->
    {Id, State#state{current_room_ids = Rest}}.

set_muc_service(State, _Meta, MucService) ->
    {ok, State#state{muc_service = MucService}}.

get_muc_service(#state{muc_service = MucService, domain = Domain}) ->
    case MucService of
        T when T /= undefined -> T;
        undefined when Domain /= undefined -> ["chat.", Domain];
        _ -> undefined
    end.

pool_id(State, Meta) ->
    {proplists:get_value(worker_id, Meta), State}.

iname(State, _Meta, Name, Id) -> {[Name, "-", integer_to_list(Id)], State}.

get_room_ids(State, _Meta, N, MaxId) ->
    {choose_random_ll(N, 1, MaxId, sets:new()), State}.

recepient(#state{username = Me} = State, Meta, Name, MinId, MaxId) ->
    Id = crypto:rand_uniform(MinId, MaxId),
    case iname(State, Meta, Name, Id) of
        {Me, _} ->
            NewId = case (Id + 1) of
                MaxId -> MinId;
                I -> I
            end,
            iname(State, Meta, Name, NewId);
        R -> R
    end.

marker(State, Meta) -> marker(State, Meta, ?XMPP_DEFAULT_MSG_SIZE).
marker(State, _Meta, Size) -> {xmpp_protocol:generate_marker(Size), State}.

choose_random_ll(0, _, _, Set) -> sets:to_list(Set);
choose_random_ll(N, MinId, MaxId, Set) ->
    Id = crypto:rand_uniform(MinId, MaxId),
    case sets:is_element(Id, Set) of
        true -> choose_random_ll(N, MinId, MaxId, Set);
        false ->
            Set1 = sets:add_element(Id, Set),
            choose_random_ll(N - 1, MinId, MaxId, Set1)
    end.

close_stream_reader(undefined) -> ok;
close_stream_reader(Pid) ->
    unlink(Pid),
    exit(Pid, kill).
