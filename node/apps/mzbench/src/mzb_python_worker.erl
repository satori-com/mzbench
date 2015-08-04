-module(mzb_python_worker).

-export([
    load/1,
    init/1,
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
    ok = send_command(PythonInterpreter, "~s.initial_state()", [Worker]),
    #state{
        worker_name         =   Worker,
        python_interpreter  =   PythonInterpreter
    }.

-spec apply(atom(), [term()], state(), term()) -> {term(), state()}.
apply(Func, Args, #state{
                            worker_name         = Worker, 
                            python_interpreter  = PythonInterpreter
                        } = State, _Meta) ->
    case execute_python_command(PythonInterpreter, Worker, Func, Args) of
        ok -> {ok, State};
        error ->
            stop_python_interpreter(PythonInterpreter),
            erlang:error(python_statement_failed)
    end.

-spec metrics(worker_name()) -> [term()].
metrics(Worker) ->
    PythonInterpreter = start_python_interpreter(Worker),
    Metrics = send_command(PythonInterpreter, "~s.metrics()", [Worker]),
    stop_python_interpreter(PythonInterpreter),
    Metrics.

-spec terminate(term(), state()) -> ok.
terminate(_Result, #state{python_interpreter = PythonInterpreter}) ->
    stop_python_interpreter(PythonInterpreter).

-spec validate(worker_name()) -> [].
validate(_WorkerName) -> [].

-spec validate_function(worker_name(), atom(), integer()) -> ok | false | bad_arity.
validate_function(Worker, Func, _Arity) ->
    PythonInterpreter = start_python_interpreter(Worker),
    ModuleFuncs = send_command(PythonInterpreter, "mzbench._module_funcs(~s)", [Worker]),
    stop_python_interpreter(PythonInterpreter),
    
    case lists:any(fun(FuncName) -> string:equal(FuncName, atom_to_list(Func)) end, ModuleFuncs) of
        true -> ok;
        false -> false
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
    port_close(FifoPort),
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
    
    port_command(PythonPort, "import sys; sys.ps1 = ''; sys.ps2 = ''\n"),   % Get rid of standard Python prompt
    port_command(PythonPort, "import os\n"),
    
    {ok, WorkerDirs} = application:get_env(mzbench, workers_dirs),
    WorkersDirsStr = string:join(["'./src'" | [io_lib:format("os.path.expanduser('~s')", [filename:join(W, Worker)]) || W <- WorkerDirs]], ", "),
    port_command(PythonPort, io_lib:format("sys.path = sys.path + [~s]\n", [WorkersDirsStr])),
    
    port_command(PythonPort, "import traceback\n"),
    port_command(PythonPort, "import mzbench\n"),
    ok = send_command(PythonInterpreter, "import ~s", [Worker]),
    PythonInterpreter.

-spec stop_python_interpreter(python_interpreter()) -> ok.
stop_python_interpreter(#python_interpreter{
                            metrics_pipe        = MetricsPipe, 
                            metrics_pipe_name   = MetricsPipeName, 
                            python_port         = PythonPort
                        }) ->
    port_command(PythonPort, "quit()\n"),
    wait_python_stop(PythonPort),
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
                    port_close(PythonPort),
                    ok
            end;
        {PythonPort, {data, {eol, Line}}} ->
            lager:info(Acc ++ Line, []),
            wait_python_stop(PythonPort, "");
        {PythonPort, {data, {noeol, Line}}} ->
            wait_python_stop(PythonPort, Acc ++ Line)
    after 5000 ->
        port_close(PythonPort),
        ok
    end.

-spec execute_python_command(python_interpreter(), string(), atom(), [term()]) -> ok | error.
execute_python_command(PythonInterpreter, Worker, Func, Args) ->
    FuncName = atom_to_list(Func),
    ParamStr = string:join(lists:map(fun encode_for_python/1, Args), ", "),
    
    try
        ok = send_command(PythonInterpreter, "~s.~s(~s)", [Worker, FuncName, ParamStr])
    catch
        _ -> error
    end.

-spec send_command(python_interpreter(), string(), [term()]) -> term().
send_command(#python_interpreter{python_port = PythonPort} = PythonInterpreter, CmdTemplate, Args) ->
    port_command(PythonPort, io_lib:format("try:\n"
        "    ~s\n"
        "    mzbench._instruction_end()\n"
        "except:\n"
        "    traceback.print_exc()\n"
        "    mzbench._instruction_failed()\n"
        "\n", 
        [io_lib:format(CmdTemplate, Args)])),
    read_python_output(PythonInterpreter).

-spec read_python_output(python_interpreter()) -> term().
read_python_output(PythonInterpreter) ->
    read_python_output(PythonInterpreter, "", "", ok).

-spec read_python_output(python_interpreter(), string(), string(), term()) -> term().
read_python_output(#python_interpreter{python_port = PythonPort, metrics_pipe = MetricsPipe} = PythonInterpreter, 
                        PythonAcc, MetricsAcc, ReturnTerm) ->
    receive
        {PythonPort, {data, {eol, Line}}} ->
            lager:info(PythonAcc ++ Line, []),
            read_python_output(PythonInterpreter, "", MetricsAcc, ReturnTerm);
        {PythonPort, {data, {noeol, Line}}} ->
            read_python_output(PythonInterpreter, PythonAcc ++ Line, MetricsAcc, ReturnTerm);
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
                execution_finished -> ReturnTerm;
                execution_failed -> erlang:error(python_statement_failed);
                {metrics_list, MetricsList} -> read_python_output(PythonInterpreter, PythonAcc, "", MetricsList);
                {funcs_list, FuncsList} -> read_python_output(PythonInterpreter, PythonAcc, "", FuncsList);
                continue -> read_python_output(PythonInterpreter, PythonAcc, "", ReturnTerm)
            end;
        {MetricsPipe, {data, {noeol, Line}}} ->
            read_python_output(PythonInterpreter, PythonAcc, MetricsAcc ++ Line, ReturnTerm);
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
%   * D - the "Data" carry a list of possible metrics;
%   * M - metric notify ("Data" = {Metric, Value});
%   * F - the "Data" carry a list of functions.
-spec interpret_metrics_pipe(string()) -> execution_finished | execution_failed | {metrics_list, term()} | {funcs_list, term()} | continue.
interpret_metrics_pipe(String) ->
    case hd(String) of
        $T -> execution_finished;
        $E -> execution_failed;
        $D ->
            MetricsText = string:substr(String, 2),
            {metrics_list, eval_erlang_term(MetricsText)};
        $M ->
            MetricUpdateText = string:substr(String, 2),
            {Metric, Value} = eval_erlang_term(MetricUpdateText),
            _ = mzb_metrics:notify(Metric, Value),
            continue;
        $F ->
            FuncsListText = string:substr(String, 2),
            FuncsList = eval_erlang_term(FuncsListText),
            {funcs_list, FuncsList};
        _ -> erlang:error(invalid_script_feedback)
    end.

-spec eval_erlang_term(string()) -> term().
eval_erlang_term(String) ->
    {ok, Tokens, _EndLine} = erl_scan:string(String),
    {ok, AbsForm} = erl_parse:parse_exprs(Tokens),
    {value, Value, _Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
    Value.

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
