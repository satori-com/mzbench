-module(simple_http_worker).

-export([initial_state/0, metrics/0,
         get/3]).

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
    StartTime = os:timestamp(),
    Response = hackney:request(get, list_to_binary(URL), [], <<"">>, []),

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
