-module(empty_worker).

-export([initial_state/0, metrics/0,
         operation/3]).

-type state() :: string().
-type meta() :: [{Key :: atom(), Value :: any()}].

-spec initial_state() -> state().
initial_state() -> "".

-spec metrics() -> list().
metrics() -> [].

-spec operation(state(), meta(), string()) -> {nil, state()}.
operation(State, _Meta, Param) ->
    lager:info("It works ~p~n", [Param]),
    {nil, State}.
