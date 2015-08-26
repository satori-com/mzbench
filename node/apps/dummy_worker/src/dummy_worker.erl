-module(dummy_worker).

-export([initial_state/0, metrics/0,
         print/3, test_method/3, doubled_print_counter/0]).

-type state() :: string().

-spec initial_state() -> state().
initial_state() -> "".

metrics() ->
    [{"print", counter},
     {"dummy", histogram},
     {"derived", derived, #{resolver => doubled_print_counter}}].

doubled_print_counter() ->
    2 * mzb_metrics:get_value("print").

print(State, _Meta, Text) ->
    Start = os:timestamp(),
    _ = mzb_metrics:notify("print", 1),
    lager:info("Dummy print: ~p", [Text]),
    Finish = os:timestamp(),
    _ = mzb_metrics:notify({"dummy", histogram}, timer:now_diff(Finish, Start)),
    {nil, State}.

test_method(State, _Meta, Text) ->
    {nil, Text ++ State}.
