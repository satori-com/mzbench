-module(counter_worker).

-export(
    [ initial_state/0
    , inc/2
    ]).

initial_state() -> 0.

inc(N, _) -> {nil, N + 1}.
