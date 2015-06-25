-module(mzb_pipeline_test).

-include_lib("eunit/include/eunit.hrl").

setup_pipeline() ->
    Self = self(),
    ok = meck:new(dummy_pipeline, [non_strict]),
    ok = meck:expect(dummy_pipeline, init, fun ([args]) -> {ok, #{args => args}} end),
    ok = meck:expect(dummy_pipeline, get_logger, fun (_State) -> fun(_S, _F, _A) -> ignore end end),
    ok = meck:expect(dummy_pipeline, handle_pipeline_status, fun (S, _State) -> Self ! S end),
    ok = meck:expect(dummy_pipeline, terminate, fun (_Reason, _State) -> ok end),
    dummy_pipeline.

wait_statuses(Statuses) ->
    lists:foreach(fun (Status) ->
                          receive
                              Status -> ok;
                              UnknownMessage ->
                                 ?debugFmt("Receive unknown message ~p", [UnknownMessage]),
                                 erlang:raise(unexpected_message)
                          after 5000 ->
                                 erlang:raise(stage_timeout)
                          end
                  end, Statuses).

wait_process(Pid) ->
    MonRef = monitor(process, Pid),
    receive
        {'DOWN', MonRef, process, _Pid, _Reason} -> ok;
    UnknownMessage ->
        ?debugFmt("Receive unknown message ~p", [UnknownMessage]),
        erlang:raise(unexpected_message)
    after 5000 ->
          erlang:raise(timeout_error)
    end.


successful_pipeline_test() ->
    Module = setup_pipeline(),
    try
        ok = meck:expect(Module, workflow_config, fun (_State) ->
                                                      [{pipeline, [p1, p2, p3]},
                                                       {finalize, [f1, f2, f3]}]
                                                  end),
        ok = meck:expect(Module, handle_stage, fun (_,_,_) -> ok end),

        {ok, Pid} = mzb_pipeline:start_link(Module, [args], []),

        wait_statuses([{pipeline, p1}, {pipeline, p2}, {pipeline, p3}, complete,
                       {finalize, f1}, {finalize, f2}, {finalize, f3}]),

        wait_process(Pid)
    after
        meck:unload(Module)
    end.

failed_pipeline_test() ->
    Module = setup_pipeline(),
    try
        ok = meck:expect(Module, workflow_config, fun (_State) ->
                                                      [{pipeline, [p1, p2, p3]},
                                                       {finalize, [f1, f2, f3]}]
                                                  end),

        ok = meck:expect(Module, handle_stage, fun
                                                   (pipeline,p2,_) -> erlang:raise(service_fail);
                                                   (_,_,_) -> ok 
                                               end),


        {ok, Pid} = mzb_pipeline:start_link(Module, [args], []),

        wait_statuses([{pipeline, p1}, {pipeline, p2}, failed,
                       {finalize, f1}, {finalize, f2}, {finalize, f3}]),

        wait_process(Pid)
    after
        meck:unload(Module)
    end.

stopped_pipeline_test() ->
    Module = setup_pipeline(),
    try
        ok = meck:expect(Module, workflow_config, fun (_State) ->
                                                      [{pipeline, [p1, p2, p3]},
                                                       {finalize, [f1, f2, f3]}]
                                                  end),

        ok = meck:expect(Module, handle_stage, fun
                                                   (pipeline,p2,_) ->
                                                       % create sum other process and check 
                                                       % that process are closed after stop
                                                       Pid = spawn_link(fun() -> timer:sleep(60000) end),
                                                       register(hung_process, Pid),
                                                       timer:sleep(60000);
                                                   (_,_,_) -> ok 
                                               end),


        {ok, Pid} = mzb_pipeline:start_link(Module, [args], []),

        wait_statuses([{pipeline, p1}, {pipeline, p2}]),
        mzb_pipeline:stop(Pid),
        wait_statuses([stopped, {finalize, f1}, {finalize, f2}, {finalize, f3}]),
        wait_process(hung_process),
        wait_process(Pid)
    after
        meck:unload(Module)
    end.

