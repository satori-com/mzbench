-module(empty_worker).

-export([initial_state/0, metrics/0,
         operation/3]).

-type state() :: string().
-type meta() :: [{Key :: atom(), Value :: any()}].

-type graph_group() :: {group, Name :: string(), [graph()]}
                     | graph().
-type graph()       :: {graph, Opts :: #{metrics => [metric()],
                                         units => string(),
                                         title => string()}}
                     | [metric()]
                     | metric().
-type metric()      :: {Name :: string(), Type :: metric_type() }
                     | {Name :: string(), Type :: metric_type(), Opts :: map() }.
-type metric_type() :: counter | gauge | histogram.


-spec initial_state() -> state().
initial_state() -> "".

-spec metrics() -> [graph_group()].
metrics() -> [].

-spec operation(state(), meta(), string()) -> {nil, state()}.
operation(State, _Meta, Param) ->
    lager:info("It works ~p~n", [Param]),
    {nil, State}.
