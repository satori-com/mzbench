-module(simple_http_worker).

-export([initial_state/0, metrics/0,
         get/3, get/4, post/5, random_get/3]).

-type state() :: term().
-type meta() :: [{Key :: atom(), Value :: any()}].

-spec initial_state() -> state().
initial_state() -> [].

-spec metrics() -> list().
metrics() ->
    [
        {group, "Summary", [
            {graph, #{title => "HTTP Response",
                      units => "N",
                      metrics => [{"http_ok", counter}, {"http_fail", counter}, {"other_fail", counter}]}},
            {graph, #{title => "Latency",
                      units => "microseconds",
                      metrics => [{"latency", histogram}]}}
        ]}
    ].

-spec get(state(), meta(), string()) -> {nil, state()}.
get(State, _Meta, URL) ->
    get(State, _Meta, URL, []).

-spec get(state(), meta(), string(), [{atom(), term()}]) -> {nil, state()}.
get(State, _Meta, URL, Options) ->
    ShouldLog = proplists:get_value(log, Options, false),
    ExpectedCode = proplists:get_value(expected, Options, 200),

    ShouldLog andalso lager:info("GET ~s", [URL]),

    {HackneyOptions, Headers} = parse_options(Options),
    StartTime = os:timestamp(),
    Response = hackney:request(get, list_to_binary(URL), Headers, <<"">>, HackneyOptions),
    Latency = timer:now_diff(os:timestamp(), StartTime),

    mzb_metrics:notify({"latency", histogram}, Latency),

    case Response of
        {ok, ExpectedCode, _, Ref} ->
            hackney:skip_body(Ref),
            mzb_metrics:notify({"http_ok", counter}, 1);
        {ok, _, _, Ref} = Reply ->
            lager:error("GET failed~nURL: ~p~nHeaders: ~p~nReply:~p~n~p", [URL, Headers, Reply, hackney:body(Ref)]),
            mzb_metrics:notify({"http_fail", counter}, 1);
        E ->
            lager:error("hackney:request failed: ~p", [E]),
            lager:error("hackney:request failed~nURL: ~p~nHeaders: ~p~nReason: ~p", [URL, Headers, E]),
            mzb_metrics:notify({"other_fail", counter}, 1)
    end,
    {nil, State}.

post(State, _Meta, URL, Body, Options) ->
    ShouldLog = proplists:get_value(log, Options, false),
    ExpectedCode = proplists:get_value(expected, Options, 200),

    ShouldLog andalso lager:info("POST ~s", [URL]),

    {HackneyOptions, Headers} = parse_options(Options),
    StartTime = os:timestamp(),
    Response = hackney:request(post, list_to_binary(URL), Headers, list_to_binary(Body), HackneyOptions),
    Latency = timer:now_diff(os:timestamp(), StartTime),

    mzb_metrics:notify({"latency", histogram}, Latency),

    case Response of
        {ok, ExpectedCode, _, Ref} ->
            hackney:skip_body(Ref),
            mzb_metrics:notify({"http_ok", counter}, 1);
        {ok, _, _, Ref} = Reply ->
            lager:error("POST failed~nURL: ~p~nHeaders: ~p~nBody: ~p~nReply:~p~n~p", [URL, Headers, Body, Reply, hackney:body(Ref)]),
            mzb_metrics:notify({"http_fail", counter}, 1);
        E ->
            lager:error("hackney:request failed~nURL: ~p~nHeaders: ~p~nBody: ~p~nReason: ~p", [URL, Headers, Body, E]),
            mzb_metrics:notify({"other_fail", counter}, 1)
    end,

    {nil, State}.

random_get(State, Meta, List) ->
    {URL, Options} = weighted_choose(List),
    get(State, Meta, URL, Options).

weighted_choose(List) ->
    Sum = lists:sum([W || {_, W} <- List]),
    R = random:uniform(Sum),
    choose(R, List).

choose(R, [{E, W}|_]) when R =< W -> E;
choose(R, [{_, W}|T]) -> choose(R - W, T).

parse_options(Options) -> parse_options(Options, [], []).
parse_options([], OptionsAcc, HeadersAcc) -> {lists:reverse(OptionsAcc), lists:reverse(HeadersAcc)};
parse_options([{header, Header}|T], OAcc, HAcc) ->
    [N, V] = string:tokens(Header, ":"),
    parse_options(T, OAcc, [{list_to_binary(N), list_to_binary(V)}|HAcc]);
parse_options([{basic_auth, {Name, Pass}}|T], OAcc, HAcc) ->
    parse_options(T, [{basic_auth, {list_to_binary(Name), list_to_binary(Pass)}}|OAcc], HAcc);
parse_options([_|T], OAcc, HAcc) ->
    parse_options(T, OAcc, HAcc).

