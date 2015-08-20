-module(xmpp_protocol).

-export([connect/2,
         handshake/5,
         send_packet/3,
         get_stanza_parser/1,
         generate_marker/1,
         stream_parser/5,
         find_stanzas/2,
         ts_to_longts/1
        ]).

-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_ACK_TIMEOUT, 120000).


connect(Host, Port) ->
    case gen_tcp:connect(Host, Port, [{active, false}, {reuseaddr, true}, binary]) of
        {ok, Socket} -> Socket;
        {error, Reason} ->
            lager:error("Tcp connect failed with reason ~p: ~p", [Reason, inet:format_error(Reason)]),
            erlang:error({tcp_connect_failed, Reason})
    end.

handshake(Username, Password, Resource, Domain, Socket) ->
    send_packet({connect, Domain}, "<stream:features>", Socket),
    send_packet({auth_sasl, Username, Password}, "<success", Socket),
    send_packet({connect, Domain}, "<stream:features>", Socket),
    send_packet({auth_sasl_bind, Resource}, "<jid>", Socket),
    send_packet({auth_sasl_session, Domain}, "<session", Socket).

-spec send_packet(tuple(), string() | skip | any, term()) -> ok.
send_packet(Message, ResponseRegExp, Socket) ->
    try
        XML = get_xml_message(Message),
        mzb_metrics:notify({"xmpp.send-packets", counter}, 1),
        ok = send(Socket, XML),
        wait_response(ResponseRegExp, Socket)
    catch _C:E ->
        erlang:error({send_packet_error, Message, ResponseRegExp, E})
    end.

generate_marker(Size) ->
    Id = ts_to_longts(os:timestamp()),
    Marker = [<<"***">>, node_marker(), <<",">>, integer_to_binary(Id), <<"***">>],
    Filler = duplicate(Size - erlang:iolist_size(Marker), [$ ]),
    [Filler, Marker].

get_stanza_parser({latency}) ->
    CurrentNode = node_marker(),
    {ok, CompiledRegExp} =  re:compile("\\*\\*\\*([^\\*]+)\\*\\*\\*"),
    fun (Stanza) ->
        case re:run(Stanza, CompiledRegExp, [{capture, all_but_first, list}]) of
            {match, [Stamp]} ->
                [Node, Ts] = string:tokens(Stamp, ","),
                case Node of
                    CurrentNode -> % same node
                        Latency = ts_to_longts(os:timestamp()) - list_to_integer(Ts),
                        mzb_metrics:notify({"xmpp.parser.latency", histogram}, Latency div 1000),
                        ok;
                    _ -> nothing
                end;
            _ -> nothing
        end
    end;
get_stanza_parser({muc_message}) ->
    get_stanza_parser({regexp, "<message[^>]*type=[\"\']groupchat[\"\']", "xmpp.parser.muc-msg"});
get_stanza_parser({message}) ->
    get_stanza_parser({regexp, "<message[^>]*type=[\"\']chat[\"\']", "xmpp.parser.msg"});
get_stanza_parser({errors}) ->
    get_stanza_parser({regexp, "<message[^>]*type=[\"\']error[\"\']", "xmpp.parser.error-msg"});
get_stanza_parser({regexp, RegExp, Metric}) ->
    {ok, CompiledRegExp} = re:compile(RegExp),
    fun (Stanza) ->
        case re:run(Stanza, CompiledRegExp, [{capture, first}]) of
            {match, _} -> mzb_metrics:notify({Metric, counter}, 1);
            _ -> nothing
        end
    end.

stream_parser(Socket, Buffer, Timeout, ParserState, StanzaParsers) ->
    Data = recv(Socket, Buffer, Timeout),
    {NewParserState, Stanzas} = find_stanzas(Data, ParserState),
    case Stanzas of
       [] -> ok;
       _  -> mzb_metrics:notify({"xmpp.recv-stanzas", counter}, length(Stanzas))
    end,
    [Parser(S) || S <- Stanzas, Parser <- StanzaParsers],
    stream_parser(Socket, Buffer, Timeout, NewParserState, StanzaParsers).

%%%
%%% Internals
%%%

wait_response(skip, _Socket) -> ok;
wait_response(RegExp, Socket) ->
    Data = recv(Socket, 0, ?DEFAULT_ACK_TIMEOUT),
    case match(RegExp, Data) of
        true -> ok;
        false -> wait_response(RegExp, Socket)
    end.

match(any, _Data) -> true;
match(RegExp, Data) ->
    case re:run(Data, RegExp) of
       {match,_} -> true;
       _ -> false
    end.

get_xml_message({connect, Domain}) ->
    list_to_binary([
        "<stream:stream  to='", Domain, "'",
        " xmlns='jabber:client' version='1.0'",
        " xmlns:stream='http://etherx.jabber.org/streams'>"]);
get_xml_message({close}) ->
    list_to_binary("</stream:stream>");
get_xml_message({auth_sasl, Username, Password}) ->
    S = <<0>>,
    N = list_to_binary(Username),
    P = list_to_binary(Password),
    list_to_binary(["<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' mechanism='PLAIN' >",
                    base64:encode(<<S/binary,N/binary,S/binary,P/binary>>) ,"</auth>"]);
get_xml_message({auth_sasl_bind, Resource}) ->
    list_to_binary([
        "<iq type='set' id='sasl-bind'>",
            "<bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'>",
                "<resource>", Resource, "</resource>",
            "</bind>",
        "</iq>"]);
get_xml_message({auth_sasl_session, Domain}) ->
    list_to_binary([
        "<iq to='", Domain, "' type='set' id='sasl-session'>",
             "<session xmlns='urn:ietf:params:xml:ns:xmpp-session' />",
        "</iq>"]);
get_xml_message({message, Name, Service, Message}) ->
    % according to RFC 'id' is recommended but not required for 'message' and 'presence' stanzas
    % we skip id generation here for better performance
    list_to_binary([
        "<message xmlns='jabber:client' type='chat' to='", Name, "@", Service, "'>",
            "<body>", Message, "</body>",
        "</message>"
    ]);
get_xml_message({muc_join, Room, Service, NickName}) ->
    list_to_binary([
        "<presence to='", Room,"@", Service,"/", NickName, "'></presence>"
    ]);
get_xml_message({muc_chat, Room, Service, Message}) ->
    list_to_binary([
        "<message type='groupchat' to ='", Room,"@", Service,"'>",
            "<body>", Message, "</body>",
        "</message>"
    ]);
get_xml_message({presence}) ->
    list_to_binary(["<presence><show/></presence>"]).

% wrappers to make them loggable
send(Socket, Data) ->
    % lager:info("[send] ~p", [Data]),
    gen_tcp:send(Socket, Data).

recv(Socket, Buffer, Timeout) ->
    case gen_tcp:recv(Socket, Buffer, Timeout) of
        {ok, Data} ->
            % lager:info("[recv] ~p", [Data]),
            Data;
        {error, closed} ->
            mzb_metrics:notify({"xmpp.connection-closed", counter}, 1),
            erlang:error({recv, socket_is_closed});
        {error, Reason} ->
            mzb_metrics:notify({"xmpp.recv-error", counter}, 1),
            erlang:error({recv, Reason})
    end.

-spec find_stanzas(Data :: binary(), PrevState :: binary()) -> {NewState :: binary(), Stanzas :: [binary()]}.
find_stanzas(Data, PrevState) ->
    find_stanzas(Data, PrevState, []).
find_stanzas(Data, PrevState, ListOfStanzas) ->
    Buffer = <<PrevState/binary, Data/binary>>,

    % Precompile regexp speeds stanza parsing by 30%
    % {ok, RegExp} = re:compile("</(message|presence|iq)>"),
    RegExp = {re_pattern,1,0,0,
                <<69,82,67,80,125,0,0,0,0,0,0,0,81,0,0,0,255,255,255,255,255,
                  255,255,255,60,0,62,0,0,0,1,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,
                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,125,0,57,29,60,29,47,
                  127,0,19,0,1,29,109,29,101,29,115,29,115,29,97,29,103,29,
                  101,113,0,7,29,105,29,113,113,0,19,29,112,29,114,29,101,29,
                  115,29,101,29,110,29,99,29,101,114,0,45,29,62,114,0,57,0>>},

    case re:run(Buffer, RegExp, [{capture, first}]) of
        {match, [{StartPos, Length}]} ->
            StanzaLength = StartPos + Length,
            <<Stanza:StanzaLength/binary, Rest/binary>> = Buffer,
            find_stanzas(Rest, <<>>, [Stanza | ListOfStanzas]);
        nomatch ->
            {Buffer, lists:reverse(ListOfStanzas)}
    end.

ts_to_longts(Ts) ->
    {A1, A2, A3} = Ts,
    (A1*1000000 + A2) * 1000000 + A3.

node_marker() -> integer_to_list(erlang:phash2(node())).

duplicate(N, Elem) ->
    duplicate(N, Elem, []).
duplicate(N, Elem, Acc) when N > 0 ->
    duplicate(N - 1, Elem, [Elem | Acc]);
duplicate(_N, _Elem, Acc) ->
    iolist_to_binary(Acc).
