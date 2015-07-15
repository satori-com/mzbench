-module(dummy_worker).

-export([initial_state/0, metrics/0,
         print/3, test_method/3]).

-type state() :: string().

-spec initial_state() -> state().
initial_state() -> "".

metrics() -> [[{"print", counter}, {"print_2", counter}], {"dummy", histogram}].

print(State, Meta, Text) ->
    _ = mzb_metrics:notify("print", 1),
    _ = mzb_metrics:notify("print_2", 2),
    N = random:uniform(1000000000),
    _ = mzb_metrics:notify({"dummy", histogram}, N/7),
    lager:info("Appending ~p, Meta: ~p~n", [Text, Meta]),
    {nil, State}.

test_method(State, _Meta, Text) ->
    {nil, Text ++ State}.
