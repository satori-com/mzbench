-module(mz_counter_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_, _) ->
    mz_counter:start_link().

stop(_State) ->
    ok.
