-module(dummy_worker).

-export([initial_state/0, metrics/0,
         print/3, test_method/3]).

-type state() :: string().

-spec initial_state() -> state().
initial_state() -> "".

metrics() -> [[{"print", counter}, {"print_2", counter}], {"dummy", histogram}].

print(State, _Meta, Text) ->
    Start = os:timestamp(),
    _ = mzb_metrics:notify("print", 1),
    lager:info("Dummy print: ~p", [Text]),
    Finish = os:timestamp(),
    _ = mzb_metrics:notify({"dummy", histogram}, timer:now_diff(Finish, Start)),
    {nil, State}.

test_method(State, _Meta, Text) ->
    {nil, Text ++ State}.
