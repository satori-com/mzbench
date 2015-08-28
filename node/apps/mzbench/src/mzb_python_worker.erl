-module(mzb_python_worker).

-export([
    load/1,
    init/1,
    apply/3,
    apply/4,
    metrics/1,
    terminate/2,
    validate/1,
    validate_function/3,
    encode_for_python/1,
    encode_str_for_python/1
]).

-include_lib("mzbench_language/include/mzbl_types.hrl").

-record(python_interpreter, {
    metrics_pipe                        :: port(),
    metrics_pipe_name                   :: string(),
    python_port                         :: port()
}).
-type python_interpreter() :: #python_interpreter{}.

-record(state, {
    worker_name                         :: string(),
    python_interpreter                  :: python_interpreter()
}).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================
-spec load(worker_name()) -> ok.
load(_Worker) -> ok.

-spec init(worker_name()) -> state().
init(Worker) ->
    PythonInterpreter = start_python_interpreter(Worker),
    _ = send_command(PythonInterpreter, "~s.initial_state()", [Worker]),
    #state{
        worker_name         =   Worker,
        python_interpreter  =   PythonInterpreter
    }.

-spec apply(Func :: atom(), Args :: [term()], Worker :: worker_name()) -> term().
apply(Func, Args, Worker) ->
    PythonInterpreter = start_python_interpreter(Worker),
    try
        execute_python_command(PythonInterpreter, Worker, Func, Args)
    catch
        _:Reason ->
            stop_python_interpreter(PythonInterpreter),
            erlang:error({python_apply_failed, {Func, Args, Worker}, Reason})
    after
        stop_python_interpreter(PythonInterpreter)
    end.


-spec apply(atom(), [term()], state(), term()) -> {term(), state()}.
apply(Func, Args, #state{
                            worker_name         = Worker, 
                            python_interpreter  = PythonInterpreter
                        } = State, _Meta) ->
    try
        {execute_python_command(PythonInterpreter, Worker, Func, Args), State}
    catch
        _:Reason ->
            stop_python_interpreter(PythonInterpreter),
            erlang:error({python_apply_failed, {Func, Args, Worker}, Reason})
    end.

-spec metrics(worker_name()) -> [term()].
metrics(Worker) ->
    PythonInterpreter = start_python_interpreter(Worker),
    try
        mzb_script_metrics:normalize(send_command(PythonInterpreter, "~s.metrics()", [Worker]))
    after
        stop_python_interpreter(PythonInterpreter)
    end.

-spec terminate(term(), state()) -> ok.
terminate(_Result, #state{python_interpreter = PythonInterpreter}) ->
    stop_python_interpreter(PythonInterpreter).

-spec validate(worker_name()) -> [].
validate(_WorkerName) -> [].

-spec validate_function(worker_name(), atom(), integer()) -> ok | not_found | bad_arity.
validate_function(Worker, Func, _Arity) ->
    PythonInterpreter = start_python_interpreter(Worker),
    try
        ModuleFuncs = send_command(PythonInterpreter,"mzbench._module_funcs(~s)", [Worker]),
        case lists:member(atom_to_list(Func), ModuleFuncs) of
            true -> ok;
            false -> not_found
        end
    after
        stop_python_interpreter(PythonInterpreter)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec create_fifo(worker_name()) -> {string(), port()}.
create_fifo(Worker) ->
    FifoName = gen_fifo_name(Worker),
    MkFifoExec = os:find_executable("mkfifo"),
    _ = mzb_subprocess:exec_format("~s ~s", [MkFifoExec, FifoName], [], fun(_, _, _) -> ok end),
    FifoPort = open_port(FifoName, [{line, 255}, eof]),
    {FifoName, FifoPort}.

-spec close_fifo(string(), port()) -> ok.
close_fifo(FifoName, FifoPort) ->
    _ = (catch port_close(FifoPort)),
    _ = file:delete(FifoName),
    ok.

-spec gen_fifo_name(worker_name()) -> string().
gen_fifo_name(Worker) ->
    {N1, N2, N3} = erlang:now(),
    filename:join(["/", "tmp", io_lib:format("worker_~s_~b_~b_~b.pipe", [Worker, N1, N2, N3])]).

-spec start_python_interpreter(string()) -> python_interpreter().
start_python_interpreter(Worker) ->
    {MetricsPipeName, MetricsPipe} = create_fifo(Worker),
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
    PythonInterpreter = #python_interpreter{
        metrics_pipe        = MetricsPipe,
        metrics_pipe_name   = MetricsPipeName,
        python_port         = PythonPort
    },

    % skip version, copyright, etc
    skip_port_messages(PythonPort, 3),

    port_command(PythonPort, "import sys; sys.ps1 = ''; sys.ps2 = ''\n"),   % Get rid of standard Python prompt
    port_command(PythonPort, "import os\n"),

    {ok, WorkerDirs} = application:get_env(mzbench, workers_dirs),
    WorkersDirsStr = string:join(["'./src'" | [io_lib:format("os.path.expanduser('~s')", [filename:join(W, Worker)]) || W <- WorkerDirs]], ", "),
    port_command(PythonPort, io_lib:format("sys.path = sys.path + [~s]\n", [WorkersDirsStr])),

    port_command(PythonPort, "import traceback\n"),
    port_command(PythonPort, "import mzbench\n"),
    port_command(PythonPort, mzb_string:format("import ~s\n", [Worker])),
    PythonInterpreter.

skip_port_messages(_Port, 0) -> ok;
skip_port_messages(Port, N) ->
    receive {Port, {data, {eol, _}}} -> ok
    after 10000 -> ok
    end,
    skip_port_messages(Port, N - 1).

-spec stop_python_interpreter(python_interpreter()) -> ok.
stop_python_interpreter(#python_interpreter{
                            metrics_pipe        = MetricsPipe, 
                            metrics_pipe_name   = MetricsPipeName, 
                            python_port         = PythonPort
                        }) ->
    _ = (catch port_command(PythonPort, "quit()\n")),
    _ = (catch wait_python_stop(PythonPort)),
    close_fifo(MetricsPipeName, MetricsPipe).

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

-spec execute_python_command(python_interpreter(), string(), atom(), [term()]) -> ok | error.
execute_python_command(PythonInterpreter, Worker, Func, Args) ->
    ParamStr = string:join(lists:map(fun encode_for_python/1, Args), ", "),
    send_command(PythonInterpreter, "~s.~s(~s)", [Worker, Func, ParamStr]).

-spec send_command(python_interpreter(), string(), [term()]) -> term().
send_command(#python_interpreter{python_port = PythonPort} = PythonInterpreter, CmdTemplate, Args) ->
    Command = mzb_string:format(CmdTemplate, Args),
    port_command(PythonPort, io_lib:format("try:\n"
        "    mzbench._instruction_end(~s)\n"
        "except:\n"
        "    traceback.print_exc()\n"
        "    mzbench._instruction_failed(sys.exc_info())\n"
        "\n", 
        [Command])),
    read_python_output(PythonInterpreter).

-spec read_python_output(python_interpreter()) -> term().
read_python_output(PythonInterpreter) ->
    read_python_output(PythonInterpreter, "", "").

-spec read_python_output(python_interpreter(), string(), string()) -> term().
read_python_output(#python_interpreter{python_port = PythonPort, metrics_pipe = MetricsPipe} = PythonInterpreter, 
                        PythonAcc, MetricsAcc) ->
    receive
        {PythonPort, {data, {eol, Line}}} ->
            lager:info(PythonAcc ++ Line, []),
            read_python_output(PythonInterpreter, "", MetricsAcc);
        {PythonPort, {data, {noeol, Line}}} ->
            read_python_output(PythonInterpreter, PythonAcc ++ Line, MetricsAcc);
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
                {execution_finished, Res} -> Res;
                {execution_failed, Reason} -> erlang:error({python_statement_failed, Reason});
                continue -> read_python_output(PythonInterpreter, PythonAcc, "")
            end;
        {MetricsPipe, {data, {noeol, Line}}} ->
            read_python_output(PythonInterpreter, PythonAcc, MetricsAcc ++ Line);
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
%   * T - function execution finished normally;
%   * E - function execution finished with an error;
%   * M - metric notify ("Data" = {Metric, Value});
-spec interpret_metrics_pipe(string()) -> execution_finished | execution_failed | {metrics_list, term()} | {funcs_list, term()} | continue.
interpret_metrics_pipe(String) ->
    case String of
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
