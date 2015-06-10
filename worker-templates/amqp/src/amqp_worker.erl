-module(amqp_worker).
-export([initial_state/0, metrics/0]).
-export([connect/3, disconnect/2,
         declare_exchange/3, declare_queue/3, bind/5,
         publish/4, publish/5, get/3, subscribe/3]).
-export([consumer/2, consumer_loop/2]).

-include_lib("amqp_client/include/amqp_client.hrl").

-define(DEFAULT_X, <<>>).

-record(s, {
    connection       = undefined,
    channel          = undefined,
    consumer_pid     = undefined,
    subscription_tag = undefined,
    queue            = undefined
}).

-record(payload, {
    data      = <<>>         :: binary(),
    timestamp = erlang:now() :: erlang:timestamp()
}).

initial_state() -> #s{}.


metrics() ->
    [
        {"publish", counter},
        {"get", counter},
        {"get", histogram},
        {"consumer_loop", counter},
        {"consumer_loop", histogram}
    ].

connect(_, _Meta, Address) ->
    {ok, Target} = amqp_uri:parse(Address),
    {ok, Connection} = amqp_connection:start(Target),
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

declare_queue(State, Meta, InQ) ->
    Q = add_postfix(Meta, InQ),
    Channel = State#s.channel,
    Declare = #'queue.declare'{queue = Q, auto_delete = true, arguments = [{<<"x-expires">>, long, 60000}]},
    #'queue.declare_ok'{} = amqp_channel:call(Channel, Declare),
    {nil, State}.

declare_exchange(State, _Meta, X) ->
    Channel = State#s.channel,
    Declare = #'exchange.declare'{exchange = X, auto_delete = true},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, Declare),
    {nil, State}.

bind(State, Meta, X, RoutingKey, InQ) ->
    Q = add_postfix(Meta, InQ),
    Channel = State#s.channel,
    amqp_channel:call(Channel, #'queue.bind'{queue = Q, exchange = X, routing_key = add_postfix(Meta, RoutingKey)}),
    {nil, State}.

publish(State, Meta, RoutingKey, Payload) ->
    publish(State, Meta, ?DEFAULT_X, RoutingKey, Payload).

publish(State, Meta, X, RoutingKey, Payload) ->
    Channel = State#s.channel,
    Publish = #'basic.publish'{exchange = X, routing_key = add_postfix(Meta, RoutingKey)},
    Binary = erlang:term_to_binary(#payload{data = Payload}),
    ok = amqp_channel:call(Channel, Publish, #amqp_msg{payload = Binary}),
    mzb_metrics:notify({"publish", counter}, 1),
    {nil, State}.

get(State, Meta, InQ) ->
    Q = add_postfix(Meta, InQ),
    Channel = State#s.channel,
    Get = #'basic.get'{queue = Q, no_ack = true},
    Response = amqp_channel:call(Channel, Get),
    case Response of
        {#'basic.get_ok'{}, Content} ->
            #amqp_msg{payload = Payload} = Content,
            #payload{timestamp = Now1} = erlang:binary_to_term(Payload),
            mzb_metrics:notify({"get", counter}, 1),
            mzb_metrics:notify({"get", histogram}, timer:now_diff(erlang:now(), Now1));
        #'basic.get_empty'{} ->
            nop
    end,
    {nil, State}.

subscribe(State, Meta, InQ) ->
    Q = add_postfix(Meta, InQ),
    Channel = State#s.channel,
    Consumer = spawn_link(?MODULE, consumer, [Channel, Meta]),
    Sub = #'basic.consume'{queue = Q},
    #'basic.consume_ok'{consumer_tag = Tag} = amqp_channel:subscribe(Channel, Sub, Consumer),
    {Consumer, State#s{consumer_pid = Consumer, subscription_tag = Tag}}.

consumer(Channel, Meta) ->
    erlang:monitor(process, Channel),
    consumer_loop(Channel, Meta).

%% Internal functions
consumer_loop(Channel, Meta) ->
    receive
        #'basic.consume_ok'{} ->
            ?MODULE:consumer_loop(Channel, Meta);

        #'basic.cancel_ok'{} ->
            ok;

        {#'basic.deliver'{delivery_tag = Tag}, Content} ->
            #amqp_msg{payload = Payload} = Content,
            #payload{timestamp = Now1} = erlang:binary_to_term(Payload),
            mzb_metrics:notify({"consumer_loop", counter}, 1),
            mzb_metrics:notify({"consumer_loop", histogram}, timer:now_diff(erlang:now(), Now1)),
            amqp_channel:call(Channel, #'basic.ack'{delivery_tag = Tag}),
            ?MODULE:consumer_loop(Channel, Meta);

        {'DOWN', _, _, _, _} ->
            ok
    end.

add_postfix(Meta, S) when is_binary(S) ->
    RunId = list_to_binary(proplists:get_value(run_id, Meta, "default")),
    <<S/binary, "-", RunId/binary>>.

