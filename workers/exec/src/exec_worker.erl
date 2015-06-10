-module(exec_worker).

-export([
    initial_state/0,
    metrics/0,
    execute/3
]).

initial_state() -> "".

metrics() ->
    [
     {"success", counter},
     {"fail", counter},
     {"latency_us", histogram}
    ].

execute(State, _Meta, Command) ->
    lager:info("Executing ~p...~n", [Command]),
    TimeStart = os:timestamp(),
    case run(Command, []) of
        {ok, _} -> 
            mzb_metrics:notify("success", 1);
        {error, {ExitCode, Output}} ->
            mzb_metrics:notify("fail", 1),
            lager:error("Execution failed~nCommand: ~p~nExit Code: ~p~nOutput: ~s", [Command, ExitCode, Output])
    end,
    TimeFinish = os:timestamp(),
    mzb_metrics:notify({"latency_us", histogram}, timer:now_diff(TimeFinish, TimeStart)),
    {nil, State}.

run(Command, Opts) ->
    Port = open_port({spawn, Command}, [stream, eof, exit_status | Opts]),
    case get_data(Port, "") of
        {0, Output} ->
            string:strip(Output, right, $\n),
            {ok, Output};
        {Code, Output} ->
            {error, {Code, Output}}
    end.

get_data(Port, Acc) ->
    receive
        {Port, {data, Bytes}} ->
            get_data(Port, [Acc | Bytes]);
        {Port, eof} ->
            Port ! {self(), close},
            get_data(Port, Acc);
        stop ->
            Port ! {self(), close},
            get_data(Port, Acc);
        {Port, closed} ->
            ExitCode =
                receive
                    {Port, {exit_status, Code}} -> Code
                end,
            {ExitCode, lists:flatten(Acc)}
    end.
