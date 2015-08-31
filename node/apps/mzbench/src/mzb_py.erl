-module(mzb_py).

-export([
    start_interpreter/1,
    stop_interpreter/1,
    eval/2,
    apply/4
]).

% for tests:
-export([encode_for_python/1,encode_str_for_python/1]).

-record(python_interpreter, {
    metrics_pipe                        :: port(),
    metrics_pipe_name                   :: string(),
    python_port                         :: port()
}).

-type python_interpreter() :: #python_interpreter{}.

-spec start_interpreter(string()) -> python_interpreter().
start_interpreter(Module) ->
    {MetricsPipeName, MetricsPipe} = create_fifo(Module),
    PythonExec = os:find_executable("python"),
    PythonPort = open_port({spawn, io_lib:format("~s -i", [PythonExec])}, [
        {line, 255}, 
        {env, [
            {"MZ_PYTHON_WORKER_FIFO_NAME", MetricsPipeName}
        ]},
        exit_status, 
        eof,
        stderr_to_stdout
    ]),
    Interpreter = #python_interpreter{
        metrics_pipe        = MetricsPipe,
        metrics_pipe_name   = MetricsPipeName,
        python_port         = PythonPort
    },

    % skip version, copyright, etc
    skip_port_messages(PythonPort, 3),

    port_command(PythonPort, "import sys; sys.ps1 = ''; sys.ps2 = ''\n"),   % Get rid of standard Python prompt
    port_command(PythonPort, "import os\n"),

    {ok, WorkerDirs} = application:get_env(mzbench, workers_dirs),
    WorkersDirsStr = string:join(["'./src'" | [io_lib:format("os.path.expanduser('~s')", [filename:join(W, Module)]) || W <- WorkerDirs]], ", "),
    port_command(PythonPort, io_lib:format("sys.path = sys.path + [~s]\n", [WorkersDirsStr])),

    port_command(PythonPort, "import traceback\n"),
    port_command(PythonPort, "import mzbench\n"),
    port_command(PythonPort, mzb_string:format("import ~s\n", [Module])),
    Interpreter.


-spec stop_interpreter(python_interpreter()) -> ok.
stop_interpreter(#python_interpreter{
                            metrics_pipe        = MetricsPipe, 
                            metrics_pipe_name   = MetricsPipeName, 
                            python_port         = PythonPort
                        }) ->
    _ = (catch port_command(PythonPort, "quit()\n")),
    _ = (catch wait_python_stop(PythonPort)),
    close_fifo(MetricsPipeName, MetricsPipe).


-spec apply(python_interpreter(), string() | atom(), string() | atom(), [term()]) -> term().
apply(Interpreter, Module, Func, Args) ->
    ParamStr = string:join(lists:map(fun encode_for_python/1, Args), ", "),
    send_command(Interpreter, "~s.~s(~s)", [Module, Func, ParamStr]).

-spec eval(python_interpreter(), string()) -> term().
eval(Interpreter, Str) ->
    send_command(Interpreter, Str, []).


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec send_command(python_interpreter(), string(), [term()]) -> term().
send_command(#python_interpreter{python_port = PythonPort} = Interpreter, CmdTemplate, Args) ->
    Command = mzb_string:format(CmdTemplate, Args),
    port_command(PythonPort, io_lib:format("try:\n"
        "    mzbench._instruction_end(~s)\n"
        "except:\n"
        "    traceback.print_exc()\n"
        "    mzbench._instruction_failed(sys.exc_info())\n"
        "\n", 
        [Command])),
    read_python_output(Interpreter).

-spec create_fifo(string()) -> {string(), port()}.
create_fifo(Name) ->
    FifoName = gen_fifo_name(Name),
    MkFifoExec = os:find_executable("mkfifo"),
    _ = mzb_subprocess:exec_format("~s ~s", [MkFifoExec, FifoName], [], fun(_, _, _) -> ok end),
    FifoPort = open_port(FifoName, [{line, 255}, eof]),
    {FifoName, FifoPort}.

-spec close_fifo(string(), port()) -> ok.
close_fifo(FifoName, FifoPort) ->
    _ = (catch port_close(FifoPort)),
    _ = file:delete(FifoName),
    ok.

-spec gen_fifo_name(string()) -> string().
gen_fifo_name(Module) ->
    {N1, N2, N3} = erlang:now(),
    filename:join(["/", "tmp", io_lib:format("worker_~s_~b_~b_~b.pipe", [Module, N1, N2, N3])]).

skip_port_messages(_Port, 0) -> ok;
skip_port_messages(Port, N) ->
    receive {Port, {data, {eol, _}}} -> ok
    after 10000 -> ok
    end,
    skip_port_messages(Port, N - 1).

-spec wait_python_stop(port()) -> ok.
wait_python_stop(PythonPort) ->
    wait_python_stop(PythonPort, "").

-spec wait_python_stop(port(), string()) -> ok.
wait_python_stop(PythonPort, Acc) ->
    ok = receive
        {PythonPort, eof} ->
            case length(Acc) of
                0 -> ok;
                _ -> lager:info(Acc, [])
            end,
            receive
                {PythonPort, {exit_status, _Status}} ->
                    _ = (catch port_close(PythonPort)),
                    ok
            end;
        {PythonPort, {data, {eol, Line}}} ->
            lager:info(Acc ++ Line, []),
            wait_python_stop(PythonPort, "");
        {PythonPort, {data, {noeol, Line}}} ->
            wait_python_stop(PythonPort, Acc ++ Line)
    after 5000 ->
        _ = (catch port_close(PythonPort)),
        ok
    end.

-spec read_python_output(python_interpreter()) -> term().
read_python_output(Interpreter) ->
    read_python_output(Interpreter, "", "").

-spec read_python_output(python_interpreter(), string(), string()) -> term().
read_python_output(#python_interpreter{python_port = PythonPort, metrics_pipe = MetricsPipe} = Interpreter,
                        PythonAcc, MetricsAcc) ->
    receive
        {PythonPort, {data, {eol, Line}}} ->
            lager:info(PythonAcc ++ Line, []),
            read_python_output(Interpreter, "", MetricsAcc);
        {PythonPort, {data, {noeol, Line}}} ->
            read_python_output(Interpreter, PythonAcc ++ Line, MetricsAcc);
        {PythonPort, {exit_status, _Status}} ->
            case length(PythonAcc) of
                0 -> ok;
                _ -> lager:info(PythonAcc),
                    ok
            end,
            lager:error("Python interpreter finished unexpectedly!"),
            erlang:error(python_interpreter_died);
        {MetricsPipe, {data, {eol, Line}}} ->
            case interpret_metrics_pipe(MetricsAcc ++ Line) of
                {call, {M, F, A}} ->
                    {Res, Content} = try
                        {"OK", erlang:apply(M, F, A)}
                    catch
                        _:Error ->
                            {"ERROR", mzb_string:format("~p", [Error])}
                    end,
                    Str = encode_for_python(Content),
                    LineNum = length(string:tokens(Str, "\n")),
                    port_command(PythonPort, io_lib:format("~s~n~b~n~s~n", [Res, LineNum, Str])),
                    read_python_output(Interpreter, PythonAcc, "");
                {execution_finished, Res} -> Res;
                {execution_failed, Reason} -> erlang:error({python_statement_failed, Reason});
                continue -> read_python_output(Interpreter, PythonAcc, "")
            end;
        {MetricsPipe, {data, {noeol, Line}}} ->
            read_python_output(Interpreter, PythonAcc, MetricsAcc ++ Line);
        {MetricsPipe, closed} ->
            case length(MetricsAcc) of
                0 -> ok;
                _ -> lager:info(MetricsAcc),
                    ok
            end,
            lager:error("Metrics communication pipe closed unexpectedly!"),
            erlang:error(metrics_pipe_closed)
    end.

% Metrics pipe message format is as follow:
%   <Marker> [<Data>]\n
%
% The "Marker" is a character identifying the message and the "Data" is an optional erlang term.
%
% Currently existing messages:
%   * C - call erlang function
%   * T - function execution finished normally;
%   * E - function execution finished with an error;
%   * M - metric notify ("Data" = {Metric, Value});
-spec interpret_metrics_pipe(string()) -> execution_finished | execution_failed | {metrics_list, term()} | {funcs_list, term()} | continue.
interpret_metrics_pipe(String) ->
    case String of
        "C " ++ MFA ->
            {call, eval_erlang_term(MFA)};
        "T " ++ Result ->
            {execution_finished, eval_erlang_term(Result)};
        "E " ++ Reason ->
            {execution_failed, Reason};
        "M " ++ MetricUpdateText ->
            {Metric, Value} = eval_erlang_term(MetricUpdateText),
            _ = mzb_metrics:notify(Metric, Value),
            continue;
        _ -> erlang:error({invalid_script_feedback, String})
    end.

-spec eval_erlang_term(string()) -> term().
eval_erlang_term(String) ->
    try
        {ok, Tokens, _EndLine} = erl_scan:string(String),
        {ok, AbsForm} = erl_parse:parse_exprs(Tokens),
        {value, Value, _Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
        Value
    catch
        _:_ -> erlang:error({bad_erlang_term, String})
    end.

-spec encode_for_python(term()) -> string().
encode_for_python(Term) when is_list(Term) -> "'" ++ encode_str_for_python(Term) ++ "'";
encode_for_python(Term) when is_atom(Term) -> "'" ++ encode_str_for_python(erlang:atom_to_list(Term)) ++ "'";
encode_for_python(Term) when is_integer(Term) -> erlang:integer_to_list(Term);
encode_for_python(Term) when is_float(Term) -> erlang:float_to_list(Term).

-spec encode_str_for_python(string()) -> string().
encode_str_for_python(Str) ->
    lists:reverse(lists:foldl(
        fun($', Acc) -> lists:reverse("\\'") ++ Acc;
            ($\\, Acc) -> "\\\\" ++ Acc;
            (Char, Acc) -> [Char | Acc] end,
        "", Str)).

