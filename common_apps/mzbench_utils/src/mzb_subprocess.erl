-module(mzb_subprocess).

-export([
    remote_cmd/5,
    remote_cmd/6,
    exec_format/4
]).


remote_cmd(UserName, Hosts, Executable, Args, Logger) ->
    remote_cmd(UserName, Hosts, Executable, Args, Logger, [stderr_to_stdout]).

remote_cmd(UserName, Hosts, Executable, Args, Logger, Opts) ->
    Args2 = lists:map(
        fun (A) when is_atom(A) -> erlang:atom_to_list(A);
            (A) -> A
        end, Args),
    mzb_lists:pmap(
        fun ("localhost") ->
                OrigPath = os:getenv("ORIG_PATH"),
                exec_format("bash -c \"export PATH='~s'; source /etc/profile;~s ~s\"",
                    [OrigPath, Executable, string:join(Args2, " ")], Opts, Logger);
            (Host) ->
                UserNameParam =
                    case UserName of
                        undefined -> "";
                        _ -> io_lib:format("~s@", [UserName])
                    end,
                exec_format("ssh -A -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no ~s~s \"source /etc/profile; ~s ~s\"",
                    [UserNameParam, Host, Executable, string:join(Args2, " ")], Opts, Logger)
        end, Hosts).

exec_format(Format, Args, Opts, Logger) ->
    Handler = fun (eof, Acc) -> lists:flatten(Acc);
                  (Data, Acc) -> [Acc|Data]
              end,
    exec_format(Format, Args, Opts, Handler, [], Logger).

exec_format(Format, Args, Opts, Handler, InitState, Logger) ->
    Command = io_lib:format(Format, Args),
    BeforeExec = os:timestamp(),
    Logger(info, "[ EXEC ] ~s (~p)", [Command, self()]),
    Port = open_port({spawn, lists:flatten(Command)}, [stream, eof, exit_status | Opts]),
    case get_data(Port, Handler, InitState) of
        {0, Output} ->
            Duration = timer:now_diff(os:timestamp(), BeforeExec),
            Logger(info,  "[ EXEC ] OK in ~p ms (~p)", [Duration / 1000, self()]),
            string:strip(Output, right, $\n);
        {Code, Output} ->
            Duration = timer:now_diff(os:timestamp(), BeforeExec),
            Logger(error, "[ EXEC ] Command execution failed in ~p ms~nCmd: ~s~nExit code: ~p~nOutput: ~s",
                [Duration / 1000, Command, Code, Output]),
            erlang:error({cmd_failed, lists:flatten(Command), Code, Output})
    end.

get_data(Port, Handler, State) ->
    receive
        {Port, {data, Bytes}} ->
            get_data(Port, Handler, Handler(Bytes, State));
        {Port, eof} ->
            Port ! {self(), close},
            get_data(Port, Handler, State);
        stop ->
            Port ! {self(), close},
            get_data(Port, Handler, State);
        {Port, closed} ->
            ExitCode =
                receive
                    {Port, {exit_status, Code}} -> Code
                end,
            {ExitCode, Handler(eof, State)}
    end.

