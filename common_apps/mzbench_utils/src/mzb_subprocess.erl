-module(mzb_subprocess).

-export([
    remote_cmd/5,
    remote_cmd/6,
    exec_format/4,
    check_output/4
]).


remote_cmd(UserName, Hosts, Executable, Args, Logger) ->
    remote_cmd(UserName, Hosts, Executable, Args, Logger, [stderr_to_stdout]).

remote_cmd(UserName, Hosts, Executable, Args, Logger, Opts) ->
    Args2 = lists:map(
        fun (A) when is_atom(A) -> erlang:atom_to_list(A);
            (A) -> A
        end, Args),

    CmdFormater =
        case Hosts of
            [Host] when Host == "localhost"; Host == "127.0.0.1" ->
                fun (_) ->
                    OrigPath = os:getenv("ORIG_PATH"),
                    mzb_string:format("bash -c -l \"export PATH='~s'; ~s ~s\"",
                        [OrigPath, Executable, string:join(Args2, " ")])
                end;
            _ ->
                fun (H) ->
                    UserNameParam =
                        case UserName of
                            undefined -> "";
                            _ -> io_lib:format("~s@", [UserName])
                        end,
                    mzb_string:format("ssh -A -o LogLevel=ERROR -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no ~s~s \"source /etc/profile; ~s ~s\"",
                            [UserNameParam, H, Executable, string:join(Args2, " ")])
                end
        end,

    Logger(info, "[ REMOTE EXEC ] ~s~n  at ~p", [CmdFormater("<HOST>"), Hosts]),

    try
        mzb_lists:pmap(
            fun (Host) ->
                % path might contain ~ so we need to replace it with ~~ to let io_lib:format work correctly
                CmdStr = re:replace(CmdFormater(Host), "~", "~~", [{return, list}, global]),
                exec_format(CmdStr, [], Opts, fun (_, _, _) -> ok end)
            end, Hosts)
    catch
        C:{cmd_failed, Cmd, Code, Output} = E ->
            ST = erlang:get_stacktrace(),
            Logger(error, "[ REMOTE EXEC ] Command execution failed:~nCmd: ~s~nExit code: ~p~nOutput: ~s", [Cmd, Code, Output]),
            erlang:raise(C, E, ST);
        C:E ->
            ST = erlang:get_stacktrace(),
            Logger(error, "[ REMOTE EXEC ] Command execution unnormally failed: ~p~nCmd: ~p~nArgs: ~p~nHosts: ~p", [E, Executable, Args, Hosts]),
            erlang:raise(C, E, ST)
    end.

exec_format(Format, Args, Opts, Logger) ->
    Handler = fun (eof, Acc) -> lists:flatten(Acc);
                  (Data, Acc) -> [Acc|Data]
              end,
    exec_format(Format, Args, Opts, Logger, Handler, []).

exec_format(Format, Args, Opts, Logger, Handler, InitState) ->
    Command = io_lib:format(Format, Args),
    BeforeExec = os:timestamp(),
    Logger(info, "[ EXEC ] ~s (~p)", [Command, self()]),
    Port = open_port({spawn, lists:flatten(Command)}, [stream, eof, exit_status | Opts]),
    case get_data(Port, Handler, InitState) of
        {0, Output} ->
            string:strip(Output, right, $\n);
        {Code, Output} ->
            Duration = timer:now_diff(os:timestamp(), BeforeExec),
            Logger(error, "[ EXEC ] Command execution failed in ~p ms~nCmd: ~s~nExit code: ~p~nOutput: ~s",
                [Duration / 1000, Command, Code, Output]),
            erlang:error({cmd_failed, lists:flatten(Command), Code, Output})
    end.

-type logger() :: any(). % FIXME
-spec check_output(string(), [any()], [any()], logger()) -> {integer(), string()}.
check_output(Format, Args, Opts, Logger) ->
    Handler = fun (eof, Acc) -> lists:flatten(Acc);
                  (Data, Acc) -> [Acc|Data]
              end,
    Command = io_lib:format(Format, Args),
    BeforeExec = os:timestamp(),
    Logger(info, "[ EXEC ] ~s (~p)", [Command, self()]),
    Port = open_port({spawn, lists:flatten(Command)}, [stream, eof, exit_status | Opts]),
    {Code, _Output} = Res = get_data(Port, Handler, []),
    Duration = timer:now_diff(os:timestamp(), BeforeExec),
    Logger(info, "[ EXEC ] Command executed in ~p ms~nCmd: ~s~nExit code: ~p~n",
        [Duration / 1000, Command, Code]),
    Res.


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

