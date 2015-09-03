-module(mzb_pipeline_tests).

-include_lib("eunit/include/eunit.hrl").

setup_pipeline() ->
    Self = self(),
    ok = meck:new(dummy_pipeline, [non_strict]),
    ok = meck:expect(dummy_pipeline, init, fun ([args]) -> {ok, #{args => args}} end),
    ok = meck:expect(dummy_pipeline, handle_pipeline_status, fun (S, _State) -> Self ! S end),
    ok = meck:expect(dummy_pipeline, terminate, fun (_Reason, _State) -> ok end),
    dummy_pipeline.

wait_statuses(Statuses) ->
    lists:foreach(fun (Status) ->
                          receive
                              {exception, Phase, Stage, Reason, _ST} ->
                                  ?assertEqual({exception, Phase, Stage, Reason}, Status);
                              Message -> ?assertEqual(Status, Message)
                          after 5000 ->
                                 erlang:raise(stage_timeout)
                          end
                  end, Statuses).

wait_process(Pid) ->
    MonRef = monitor(process, Pid),
    receive
        Message -> ?assertMatch({'DOWN', MonRef, process, _Pid, _Reason}, Message)
    after 5000 ->
          erlang:raise(timeout_error)
    end.


successful_pipeline_test() ->
    Module = setup_pipeline(),
    try
        ok = meck:expect(Module, workflow_config, fun (_State) ->
                                                      [{pipeline, [p1, p2]},
                                                       {finalize, [f1, f2]}]
                                                  end),
        ok = meck:expect(Module, handle_stage, fun (_,_,_) -> ok end),

        {ok, Pid} = mzb_pipeline:start_link(Module, [args], []),
        register(pipeline, Pid),

        wait_statuses([{start, pipeline, p1}, {complete, pipeline, p1},
                       {start, pipeline, p2}, {complete, pipeline, p2},
                       {final, complete},
                       {start, finalize, f1}, {complete, finalize, f1},
                       {start, finalize, f2}, {complete, finalize, f2} ])

    after
        wait_process(pipeline),
        meck:unload(Module)
    end.

failed_pipeline_test() ->
    Module = setup_pipeline(),
    try
        ok = meck:expect(Module, workflow_config, fun (_State) ->
                                                      [{pipeline, [p1, p2]},
                                                       {finalize, [f1, f2]}]
                                                  end),

        ok = meck:expect(Module, handle_stage, fun
                                                   (pipeline,p2,_) -> erlang:error(service_fail);
                                                   (_,_,_) -> ok 
                                               end),


        {ok, Pid} = mzb_pipeline:start_link(Module, [args], []),
        register(pipeline, Pid),

        wait_statuses([{start, pipeline, p1}, {complete, pipeline, p1},
                       {start, pipeline, p2},
                       {exception, pipeline, p2, service_fail},
                       {final, failed},
                       {start, finalize, f1}, {complete, finalize, f1},
                       {start, finalize, f2}, {complete, finalize, f2} ])
    after
        wait_process(pipeline),
        meck:unload(Module)
    end.

stopped_pipeline_test() ->
    Module = setup_pipeline(),
    try
        ok = meck:expect(Module, workflow_config, fun (_State) ->
                                                      [{pipeline, [p1, p2]},
                                                       {finalize, [f1, f2]}]
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
        register(pipeline, Pid),

        wait_statuses([{start, pipeline, p1}, {complete, pipeline, p1},
                       {start, pipeline, p2}]),
        mzb_pipeline:stop(Pid),
        wait_statuses([{final, stopped},
                       {start, finalize, f1}, {complete, finalize, f1},
                       {start, finalize, f2}, {complete, finalize, f2} ]),
        wait_process(hung_process)
    after
        wait_process(pipeline),
        meck:unload(Module)
    end.

