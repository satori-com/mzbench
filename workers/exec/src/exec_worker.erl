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

run(Command, Opts, WorkerId) ->
    Port = open_port({spawn, Command}, [
        {env, [{"MZB_WORKER_ID", integer_to_list(WorkerId)}]}, 
        {line, 255}, stream, eof, exit_status, stderr_to_stdout | Opts]),
    get_data(Port, WorkerId).

get_data(Port, WorkerId) -> get_data(Port, WorkerId, []).

get_data(Port, WorkerId, Buffer) ->
    receive
        {Port, {data, {eol, Bytes}}} ->
            lager:info("Worker ~p output: ~s", [WorkerId, Buffer ++ Bytes]),
            get_data(Port, WorkerId, []);
        {Port, {data, {noeol, Bytes}}} ->
            get_data(Port, WorkerId, Buffer ++ Bytes);
        {Port, eof} ->
            Port ! {self(), close},
            get_data(Port, WorkerId, Buffer);
        stop ->
            Port ! {self(), close},
            get_data(Port, WorkerId, Buffer);
        {Port, closed} ->
            receive
                {Port, {exit_status, Code}} -> Code
            end
    end.
