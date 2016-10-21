-module(simple_http_worker).

-export([initial_state/0, metrics/0,
         get/3, get/4, random_get/3]).

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
    StartTime = os:timestamp(),
    ShouldLog = proplists:get_value(log, Options, false),

    ShouldLog andalso lager:info("GET ~s", [URL]),

    HackneyOptions = parse_options(Options),
    Response = hackney:request(get, list_to_binary(URL), [], <<"">>, HackneyOptions),

    case Response of
        {ok, _, _, BodyRef} -> hackney:skip_body(BodyRef);
        _ -> ok
    end,

    Latency = timer:now_diff(os:timestamp(), StartTime),
    mzb_metrics:notify({"latency", histogram}, Latency),

    case Response of
        {ok, 200, _, _} ->
            mzb_metrics:notify({"http_ok", counter}, 1);
        {ok, _, _, _} = Reply ->
            lager:error("GET failed: ~p", [Reply]),
            mzb_metrics:notify({"http_fail", counter}, 1);
        E ->
            lager:error("hackney:request failed: ~p", [E]),
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

parse_options(Options) -> parse_options(Options, []).
parse_options([], Acc) -> lists:reverse(Acc);
parse_options([{basic_auth, {Name, Pass}}|T], Acc) ->
    parse_options(T, [{basic_auth, {list_to_binary(Name), list_to_binary(Pass)}}|Acc]);
parse_options([_|T], Acc) ->
    parse_options(T, Acc).

