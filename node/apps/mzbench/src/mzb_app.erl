-module(mzb_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_, _) ->
    mzb_erl_worker:add_pathsz("*"),
    mzb_sup:start_link().

stop(_State) ->
    ok.
