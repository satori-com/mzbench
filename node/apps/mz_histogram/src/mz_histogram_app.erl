-module(mz_histogram_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_, _) ->
    mz_histogram:start_link().

stop(_State) ->
    ok.
