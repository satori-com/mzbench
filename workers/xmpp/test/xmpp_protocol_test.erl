-module(xmpp_protocol_test).

-export([process_stanzas/5]).
-include_lib("eunit/include/eunit.hrl").


parse_stanzas_test() ->
    {Data0, []} = xmpp_protocol:find_stanzas(<<"<presen">>, <<>>),
    {Data1, []} = xmpp_protocol:find_stanzas(<<"ce> foo </pre">>, Data0),

    {Data2, Stanzas0} = xmpp_protocol:find_stanzas(<<"sence><message> bar </mes">>, Data1),
    ?assertEqual([<<"<presence> foo </presence>">>], Stanzas0),

    {<<>>, Stanzas1} = xmpp_protocol:find_stanzas(<<"sage><iq> zoo </iq>">>, Data2),
    ?assertEqual([<<"<message> bar </message>">>, <<"<iq> zoo </iq>">>], Stanzas1).


search_stanzas_randomized_test() ->
    random:seed(now()),
    N = 100000,
    {Time, NStanzas} = timer:tc(?MODULE,  process_stanzas, [N, <<>>, 0, <<>>, 0]),
    ?debugFmt("~p stanzas parsed for ~p microseconds", [NStanzas, Time]),
    ?assertEqual(N, NStanzas).

process_stanzas(0, _, 0, <<>>, ParsedStanzas) -> ParsedStanzas;
process_stanzas(N, MessageStream, Length, ParserState, ParsedStanzas) when N /= 0, Length < 200 ->
    Stanza = random_stanza(),
    process_stanzas(N-1, <<MessageStream/binary, Stanza/binary>>, Length + byte_size(Stanza), ParserState, ParsedStanzas);
process_stanzas(N, MessageStream, 0, ParserState, ParsedStanzas) ->
    ?debugFmt("something wrong ~p ~p ~p ~p", [N, MessageStream, ParserState, ParsedStanzas]);
process_stanzas(N, MessageStream, Length, ParserState, ParsedStanzas) ->
    PartLength = random:uniform(Length),
    Part = binary:part(MessageStream, 0, PartLength),
    Rest = binary: part(MessageStream, PartLength, Length - PartLength),
    {NewParserState, Stanzas}  = xmpp_protocol:find_stanzas(Part, ParserState),
    process_stanzas(N, Rest, Length - PartLength, NewParserState, ParsedStanzas + length(Stanzas)).

random_stanza() ->
    random_stanza(random:uniform(3)).
random_stanza(1) ->
    <<"<message from='room@domain/mzbench' xml:lang='' type='groupchat' id='muc-msg-187'><body>*********************************@@@91830193,1443351393819142@@@</body></message>">>;
random_stanza(2) ->
    <<"<presence type='available' to='user-1@domain/mzbench' from='room@domain/mzbench'><x xmlns='http://jabber.org/protocol/muc#user'><item jid='user-1@domain/mzbench' affiliation='owner' role='moderator'/></x></presence>">>;
random_stanza(3) ->
    <<"<iq type='result' from='domain' id='session-1'><session xmlns='urn:ietf:params:xml:ns:xmpp-session'/></iq>">>.
