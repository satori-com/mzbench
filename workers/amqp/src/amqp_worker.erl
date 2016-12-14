-module(amqp_worker).
-export([initial_state/0, metrics/0]).
-export([connect/3, disconnect/2,
         declare_exchange/3, declare_queue/3, bind/5,
         publish/4, publish/5, get/3, subscribe/3]).
-export([consumer/2, consumer_loop/2]).

-include_lib("amqp_client/include/amqp_client.hrl").

-define(DEFAULT_X, <<>>).

-record(s, {
    prefix           = "default",
    connection       = undefined,
    channel          = undefined,
    consumer_pid     = undefined,
    subscription_tag = undefined
}).

-record(payload, {
    data      = <<>>         :: binary(),
    timestamp = erlang:now() :: erlang:timestamp()
}).

-define(TIMED(Name, Expr),
    (fun () ->
        StartTime = os:timestamp(),
        Result = Expr,
        Value = timer:now_diff(os:timestamp(), StartTime),
        mzb_metrics:notify({Name, histogram}, Value),
        Result
    end)()).

initial_state() -> #s{}.

metrics() -> metrics("default").

metrics(Prefix) ->
    [
        {group, "AMQP (" ++ Prefix ++ ")", [
            {graph, #{title => "Messages",
                      units => "N",
                      metrics => [{Prefix ++ ".publish", counter}, {Prefix ++ ".get", counter}, {Prefix ++ ".consume", counter}]}},
            {graph, #{title => "Latency",
                      units => "microseconds",
                      metrics => [{Prefix ++ ".latency", histogram}]}}
        ]}
    ].

connect(_State, _Meta, Param) ->
    UserName = proplists:get_value(username, Param, <<"guest">>),
    Password = proplists:get_value(password, Param, <<"guest">>),
    UserNameBin = if is_list(UserName) -> list_to_binary(UserName);
                    true -> UserName end,
    PasswordBin = if is_list(Password) -> list_to_binary(Password);
                    true -> Password end,
    Host = proplists:get_value(host, Param, "localhost"),
    Port = proplists:get_value(port, Param, 5672),
    {ok, Connection} = amqp_connection:start(#amqp_params_network{
        username = UserNameBin,
        password = PasswordBin,
        host = Host,
        port = Port
        }),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    {nil, #s{connection = Connection, channel = Channel}}.

disconnect(#s{connection = Connection, channel = Channel,
        consumer_pid = Consumer, subscription_tag = Tag}, _Meta) ->
    case Consumer of
        undefined -> ok;
        _         -> amqp_channel:call(Channel, #'basic.cancel'{consumer_tag = Tag})
    end,
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
    {nil, initial_state()}.

declare_queue(State, Meta, InQ) when is_list(InQ) ->
    declare_queue(State, Meta, list_to_binary(InQ));
declare_queue(#s{channel = Channel} = State, _Meta, InQ) ->
    Declare = #'queue.declare'{queue = InQ, auto_delete = true, arguments = [{<<"x-expires">>, long, 60000}]},
    #'queue.declare_ok'{} = amqp_channel:call(Channel, Declare),
    {nil, State}.

declare_exchange(State, Meta, X) when is_list(X) ->
    declare_exchange(State, Meta, list_to_binary(X));
declare_exchange(#s{channel = Channel} = State, _Meta, X) ->
    Declare = #'exchange.declare'{exchange = X, auto_delete = true},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, Declare),
    {nil, State}.

bind(State, Meta, X, RoutingKey, InQ) when is_list(InQ) ->
    bind(State, Meta, X, RoutingKey, list_to_binary(InQ));
bind(State, Meta, X, RoutingKey, InQ) when is_list(X) ->
    bind(State, Meta, list_to_binary(X), RoutingKey, InQ);
bind(State, Meta, X, RoutingKey, InQ) when is_list(RoutingKey) ->
    bind(State, Meta, X, list_to_binary(RoutingKey), InQ);
bind(#s{channel = Channel} = State, _Meta, X, RoutingKey, InQ) ->
    amqp_channel:call(Channel, #'queue.bind'{queue = InQ, exchange = X, routing_key = RoutingKey}),
    {nil, State}.

publish(State, Meta, RoutingKey, Payload) ->
    publish(State, Meta, ?DEFAULT_X, RoutingKey, Payload).

publish(State, Meta, X, RoutingKey, Payload) when is_list(X) ->
    publish(State, Meta, list_to_binary(X), RoutingKey, Payload);
publish(State, Meta, X, RoutingKey, Payload) when is_list(RoutingKey) ->
    publish(State, Meta, X, list_to_binary(RoutingKey), Payload);
publish(State, Meta, X, RoutingKey, Payload) when is_list(Payload) ->
    publish(State, Meta, X, RoutingKey, list_to_binary(Payload));
publish(#s{channel = Channel, prefix = Prefix} = State, _Meta, X, RoutingKey, Payload) ->
    Publish = #'basic.publish'{exchange = X, routing_key = RoutingKey},
    Binary = erlang:term_to_binary(#payload{data = Payload}),
    ok = amqp_channel:call(Channel, Publish, #amqp_msg{payload = Binary}),
    mzb_metrics:notify({Prefix ++ ".publish", counter}, 1),
    {nil, State}.

get(State, Meta, InQ) when is_list(InQ) ->
    get(State, Meta, list_to_binary(InQ));
get(#s{channel = Channel, prefix = Prefix} = State, _Meta, InQ) ->
    Get = #'basic.get'{queue = InQ, no_ack = true},
    Response = amqp_channel:call(Channel, Get),
    case Response of
        {#'basic.get_ok'{}, Content} ->
            #amqp_msg{payload = Payload} = Content,
            #payload{timestamp = Now1} = erlang:binary_to_term(Payload),
            mzb_metrics:notify({Prefix ++ ".get", counter}, 1),
            mzb_metrics:notify({Prefix ++ ".latency", histogram}, timer:now_diff(erlang:now(), Now1));
        #'basic.get_empty'{} ->
            nop
    end,
    {nil, State}.

subscribe(State, Meta, InQ) when is_list(InQ) ->
    subscribe(State, Meta, list_to_binary(InQ));
subscribe(#s{channel = Channel, prefix = Prefix} = State, _Meta, InQ) ->
    Consumer = spawn_link(?MODULE, consumer, [Channel, Prefix]),
    Sub = #'basic.consume'{queue = InQ},
    #'basic.consume_ok'{consumer_tag = Tag} = amqp_channel:subscribe(Channel, Sub, Consumer),
    {Consumer, State#s{consumer_pid = Consumer, subscription_tag = Tag}}.

consumer(Channel, Prefix) ->
    erlang:monitor(process, Channel),
    consumer_loop(Channel, Prefix).

%% Internal functions
consumer_loop(Channel, Prefix) ->
    receive
        #'basic.consume_ok'{} ->
            consumer_loop(Channel, Prefix);

        #'basic.cancel_ok'{} ->
            ok;

        {#'basic.deliver'{delivery_tag = Tag}, Content} ->
            #amqp_msg{payload = Payload} = Content,
            #payload{timestamp = Now1} = erlang:binary_to_term(Payload),
            mzb_metrics:notify({Prefix ++ ".consume", counter}, 1),
            mzb_metrics:notify({Prefix ++ ".latency", histogram}, timer:now_diff(erlang:now(), Now1)),
            amqp_channel:call(Channel, #'basic.ack'{delivery_tag = Tag}),
            consumer_loop(Channel, Prefix);

        {'DOWN', _, _, _, _} ->
            ok
    end.

set_prefix(State, _Meta, NewPrefix) ->
    mzb_metrics:declare_metrics(metrics(NewPrefix)),
    {nil, State#s{prefix = NewPrefix}}.
