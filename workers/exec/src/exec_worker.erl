-module(exec_worker).

-export([
    initial_state/0,
    metrics/0,
    execute/3
]).

initial_state() -> "".

metrics() ->
    [
        {group, "Summary", [
            {graph, #{title => "Results",
                      units => "N",
                      metrics => [{"success", counter}, {"fail", counter}]}},
            {graph, #{title => "Latency",
                      units => "microseconds",
                      metrics => [{"latency", histogram}]}}
        ]}
    ].

execute(State, Meta, Command) ->
    WorkerId = proplists:get_value(worker_id, Meta, undefined),
    lager:info("Executing ~p... on ~p~n", [Command, WorkerId]),
    TimeStart = os:timestamp(),
    case run(Command, [], WorkerId) of
        0 -> mzb_metrics:notify("success", 1);
        ExitCode ->
            mzb_metrics:notify("fail", 1),
            lager:error("Execution on ~p failed. Exit Code: ~p", [WorkerId, ExitCode])
    end,
    TimeFinish = os:timestamp(),
    mzb_metrics:notify({"latency", histogram}, timer:now_diff(TimeFinish, TimeStart)),
    {nil, State}.

run(Command, _Opts, WorkerId) ->
    {ok, Pid, _OsPid} = exec:run(Command, [monitor, stdout, stderr,
        {env, [{"MZB_WORKER_ID", integer_to_list(WorkerId)}]}]),
    get_data(Pid, WorkerId).

get_data(Port, WorkerId) -> get_data(Port, WorkerId, []).

get_data(Port, WorkerId, _Buffer) ->
    receive
        {Stream, _Pid, Bytes} when (Stream == stdout) or (Stream == stderr) -> 
            lager:info("Worker ~p output: ~s", [WorkerId, Bytes]),
            get_data(Port, WorkerId, []);
        {'DOWN', _ , _, _, {exit_status, Code}} -> Code;
        {'DOWN', _ , _, _, normal} -> 0
    end.
