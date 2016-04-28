-module(rest_worker).

-export([initial_state/0, metrics/0]).

-export([set_host/3,
         set_port/3,
         set_options/3,
         get/3,
         post/3,
         request/7
        ]).

-export([set_body/4
        ]).

-type meta() :: [{Key :: atom(), Value :: any()}].
-type http_host() :: string().
-type http_port() :: integer().
-type http_options() :: list().

-record(state,
    { host :: http_host()
    , port :: http_port()
    , options = [] :: http_options()
    , body :: binary()
    , headers :: dict:dict()
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

-spec set_body(state(), meta(), undefined | binary(), term()) -> {nil, state()}.
set_body(State, _Meta, undefined, Value) when is_binary(Value) ->
    {nil, State#state{body=Value}};
set_body(State=#state{headers=Headers}, _Meta, ContentType, Value) when is_binary(ContentType), is_binary(Value) ->
    NewHeaders = dict:store(<<"Content-Type">>, ContentType, Headers),
    {nil, State#state{headers=NewHeaders, body=Value}}.

-spec get(state(), meta(), string()) -> {nil, state()}.
get(#state{host = Host, port = Port, headers = Headers, body = Body, options = Options} = State, _Meta, Endpoint) ->
    request(get, Host, Port, Endpoint, Headers, Body, Options),
    {nil, State}.

-spec post(state(), meta(), string()) -> {nil, state()}.
post(#state{host = Host, port = Port, headers = Headers, body = Body, options = Options} = State, _Meta, Endpoint) ->
    request(post, Host, Port, Endpoint, Headers, Body, Options),
    {nil, State}.

request(Method, Host, Port, Endpoint, Headers, Body, Options) ->
    URL = lists:flatten(io_lib:format("http://~s:~p~s", [Host, Port, Endpoint])),
    Response = ?TIMED("latency", hackney:request(
        Method, list_to_binary(URL), dict:to_list(Headers), Body, [{follow_redirect, true}] ++ Options)),
    record_response(Response).

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
