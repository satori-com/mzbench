-module(http_worker).

-export([initial_state/0, metrics/0]).

-export([set_host/3, set_port/3, set_options/3,
    get/3, post/4]).

-type meta() :: [{Key :: atom(), Value :: any()}].
-type http_host() :: string().
-type http_port() :: integer().
-type http_options() :: list().

-record(state,
    { host :: http_host()
    , port :: http_port()
    , options = [] :: http_options()
    }).

-type state() :: #state{}.

-define(TIMED(Name, Expr),
    (fun () ->
        StartTime = os:timestamp(),
        Result = Expr,
        Value = timer:now_diff(os:timestamp(), StartTime),
        mzb_metrics:notify({Name, histogram}, Value),
        Result
    end)()).

-spec initial_state() -> state().
initial_state() ->
    #state{}.

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

-spec set_host(state(), meta(), string()) -> {nil, state()}.
set_host(State, _Meta, NewHost) ->
    {nil, State#state{host = NewHost}}.

-spec set_port(state(), meta(), http_port()) -> {nil, state()}.
set_port(State, _Meta, NewPort) ->
    {nil, State#state{port = NewPort}}.

-spec set_options(state(), meta(), http_options()) -> {nil, state()}.
set_options(State, _Meta, NewOptions) ->
    {nil, State#state{options = NewOptions}}.

-spec get(state(), meta(), string()) -> {nil, state()}.
get(#state{host = Host, port = Port, options = Options} = State, _Meta, Endpoint) ->
    URL = lists:flatten(io_lib:format("http://~s:~p~s", [Host, Port, Endpoint])),
    Response = ?TIMED("latency", hackney:request(
        get, list_to_binary(URL), [], <<"">>, Options)),
    record_response(Response),
    {nil, State}.

-spec post(state(), meta(), string(), iodata()) -> {nil, state()}.
post(#state{host = Host, port = Port, options = Options} = State, _Meta, Endpoint, Payload) ->
    URL = lists:flatten(io_lib:format("http://~s:~p~s", [Host, Port, Endpoint])),
    Response = ?TIMED("latency", hackney:request(
        post, list_to_binary(URL), [], Payload, [{follow_redirect, true}] ++ Options)),
    record_response(Response),
    {nil, State}.

record_response(Response) ->
    case Response of
        {ok, 200, _, BodyRef} ->
            hackney:body(BodyRef),
            mzb_metrics:notify({"http_ok", counter}, 1);
        {ok, _, _, BodyRef} ->
            hackney:body(BodyRef),
            mzb_metrics:notify({"http_fail", counter}, 1);
        E ->
            lager:error("hackney:request failed: ~p", [E]),
            mzb_metrics:notify({"other_fail", counter}, 1)
    end.