-module(mzb_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_, _) ->
    case application:get_env(mzbench_api, load_all_workers_subdirs, false) of
        false -> ok;
        _ -> mzb_erl_worker:add_pathsz("*")
    end,
    mzb_sup:start_link().

stop(_State) ->
    ok.
