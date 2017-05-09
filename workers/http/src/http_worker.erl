-module(http_worker).

-export([initial_state/0, metrics/0]).

-export([connect/4, connect/6, set_options/3, disconnect/2,
    get/3, post/4, set_prefix/3]).

-type meta() :: [{Key :: atom(), Value :: any()}].
-type http_options() :: list().

-record(state,
    { connection = undefined
    , prefix = "default"
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
    application:set_env(hackney, use_default_pool, false),
    #state{}.

-spec metrics() -> list().
metrics() -> metrics("default").

metrics(Prefix) ->
    [
        {group, "HTTP (" ++ Prefix ++ ")", [
            {graph, #{title => "HTTP Response",
                      units => "N",
                      metrics => [{Prefix ++ ".http_ok", counter}, {Prefix ++ ".http_fail", counter}, {Prefix ++ ".other_fail", counter}]}},
            {graph, #{title => "Latency",
                      units => "microseconds",
                      metrics => [{Prefix ++ ".latency", histogram}]}}
        ]}
    ].

-spec set_prefix(state(), meta(), string()) -> {nil, state()}.
set_prefix(State, _Meta, NewPrefix) ->
    mzb_metrics:declare_metrics(metrics(NewPrefix)),
    {nil, State#state{prefix = NewPrefix}}.

-spec disconnect(state(), meta()) -> {nil, state()}.
disconnect(#state{connection = Connection} = State, _Meta) ->
    hackney:close(Connection),
    {nil, State}.

-spec connect(state(), meta(), string() | binary(), integer()) -> {nil, state()}.
connect(State, Meta, Host, Port) when is_list(Host) ->
    connect(State, Meta, list_to_binary(Host), Port, http, []).

-spec connect(state(), meta(), string() | binary(), integer(), atom(), list()) -> {nil, state()}.
connect(State, Meta, Host, Port, Protocol, Options) when is_list(Host) ->
    connect(State, Meta, list_to_binary(Host), Port, Protocol, Options);
connect(State, _Meta, Host, Port, Protocol, Options) ->
    {ok, ConnRef} = hackney:connect(
        if Protocol == http -> hackney_tcp;
           Protocol == https -> hackney_ssl;
           true -> lager:error("Unsupported protocol ~p", [Protocol]) end, Host, Port, Options),
    {nil, State#state{connection = ConnRef}}.

-spec set_options(state(), meta(), http_options()) -> {nil, state()}.
set_options(State, _Meta, NewOptions) ->
    {nil, State#state{options = NewOptions}}.

-spec get(state(), meta(), string() | binary()) -> {nil, state()}.
get(State, Meta, Endpoint) when is_list(Endpoint) ->
    get(State, Meta, list_to_binary(Endpoint));
get(#state{connection = Connection, prefix = Prefix, options = Options} = State, _Meta, Endpoint) ->
    Response = ?TIMED(Prefix ++ ".latency", hackney:send_request(Connection,
        {get, Endpoint, Options, <<>>})),
    {nil, State#state{connection = record_response(Prefix, Response)}}.

-spec post(state(), meta(), string() | binary(), iodata()) -> {nil, state()}.
post(State, Meta, Endpoint, Payload) when is_list(Endpoint) ->
    post(State, Meta, list_to_binary(Endpoint), Payload);
post(#state{connection = Connection, prefix = Prefix, options = Options} = State, _Meta, Endpoint, Payload) ->
    Response = ?TIMED(Prefix ++ ".latency", hackney:send_request(Connection,
        {post, Endpoint, Options, Payload})),
    {nil, State#state{connection = record_response(Prefix, Response)}}.

record_response(Prefix, Response) ->
    case Response of
        {ok, 200, _, Connection} ->
            hackney:body(Connection),
            mzb_metrics:notify({Prefix ++ ".http_ok", counter}, 1),
            Connection;
        {ok, _, _, Connection} ->
            hackney:body(Connection),
            mzb_metrics:notify({Prefix ++ ".http_fail", counter}, 1),
            Connection;
        E ->
            lager:error("hackney:request failed: ~p", [E]),
            mzb_metrics:notify({Prefix ++ ".other_fail", counter}, 1)
    end.
