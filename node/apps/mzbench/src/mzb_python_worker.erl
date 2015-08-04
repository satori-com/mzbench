-module(mzb_python_worker).

-export([
    load/1,
    init/1,
    apply/4,
    metrics/1,
    terminate/2,
    validate/1,
    validate_function/3
]).

-include_lib("mzbench_language/include/mzbl_types.hrl").

-record(state, {
    worker_name                         :: string(),
    metrics_pipe                        :: port(),
    metrics_pipe_name                   :: string(),
    python_port                         :: port()
}).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================
-spec load(worker_name()) -> ok.
load(_Worker) -> ok.

-spec init(worker_name()) -> state().
init(Worker) ->
    {MetricsPipeName, MetricsPipe} = create_fifo(Worker),
    PythonPort = start_python_interpreter(Worker, MetricsPipeName),
    port_command(PythonPort, io_lib:format("~s.initial_state()~n", [Worker])),
    #state{
        worker_name         =   Worker,
        metrics_pipe        =   MetricsPipe,
        metrics_pipe_name   =   MetricsPipeName,
        python_port         =   PythonPort
    }.

-spec apply(atom(), [term()], state(), term()) -> {term(), state()}.
apply(Func, Args, #state{worker_name = Worker, python_port = PythonPort, metrics_pipe = MetricsPipe} = State, _Meta) ->
    _ = execute_python_command(PythonPort, MetricsPipe, Worker, Func, Args),
    {ok, State}.

-spec metrics(worker_name()) -> [term()].
metrics(Worker) ->
    {MetricsPipeName, MetricsPipe} = create_fifo(Worker),
    PythonPort = start_python_interpreter(Worker, MetricsPipeName),
    
    port_command(PythonPort, io_lib:format("~s.metrics()\n", [Worker])),
    Metrics = read_python_output(PythonPort, MetricsPipe),
    
    stop_python_interpreter(PythonPort),
    close_fifo(MetricsPipe),
    _ = file:delete(MetricsPipeName),
    
    Metrics.

-spec terminate(term(), state()) -> ok.
terminate(_Result, #state{metrics_pipe = MetricsPipe, metrics_pipe_name = MetricsPipeName, python_port = PythonPort}) ->
    stop_python_interpreter(PythonPort),
    close_fifo(MetricsPipe),
    _ = file:delete(MetricsPipeName).

-spec validate(worker_name()) -> [].
validate(_WorkerName) -> [].

-spec validate_function(worker_name(), atom(), integer()) -> ok | false | bad_arity.
validate_function(Worker, Func, _Arity) ->
    {MetricsPipeName, MetricsPipe} = create_fifo(Worker),
    PythonPort = start_python_interpreter(Worker, MetricsPipeName),
    
    port_command(PythonPort, io_lib:format("mzbench._module_funcs(~s)\n", [Worker])),
    ModuleFuncs = read_python_output(PythonPort, MetricsPipe),
    
    stop_python_interpreter(PythonPort),
    close_fifo(MetricsPipe),
    _ = file:delete(MetricsPipeName),
    
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
    _ = run(io_lib:format("/usr/bin/mkfifo ~s", [FifoName]), []),
    FifoPort = open_port(FifoName, [{line, 255}, eof]),
    {FifoName, FifoPort}.

-spec close_fifo(port()) -> ok.
close_fifo(FifoPort) ->
    FifoPort ! {self(), close},
    ok.

-spec gen_fifo_name(worker_name()) -> string().
gen_fifo_name(Worker) ->
    {N1, N2, N3} = erlang:now(),
    filename:join(["/", "tmp", io_lib:format("worker_~s_~b_~b_~b.pipe", [Worker, N1, N2, N3])]).

-spec start_python_interpreter(string(), string()) -> port().
start_python_interpreter(Worker, MetricsPipeName) ->
    PythonPort = open_port({spawn, "python -i"}, [
        {line, 255}, 
        {env, [
            {"MZ_PYTHON_WORKER_FIFO_NAME", MetricsPipeName}
        ]},
        exit_status,
        stderr_to_stdout
    ]),
    port_command(PythonPort, "import sys; sys.ps1 = ''; sys.ps2 = ''\n"),
    port_command(PythonPort, "import os\n"),
    
    {ok, WorkerDirs} = application:get_env(mzbench, workers_dirs),
    WorkersDirsStr = string:join(["'./src'" | [io_lib:format("os.path.expanduser('~s')", [filename:join(W, Worker)]) || W <- WorkerDirs]], ", "),
    port_command(PythonPort, io_lib:format("sys.path = sys.path + [~s]\n", [WorkersDirsStr])),
    
    port_command(PythonPort, "import mzbench\n"),
    port_command(PythonPort, io_lib:format("import ~s~n", [Worker])),
    PythonPort.

-spec stop_python_interpreter(port()) -> ok.
stop_python_interpreter(PythonPort) ->
    port_command(PythonPort, "quit()\n"),
    wait_python_stop(PythonPort).

-spec wait_python_stop(port()) -> ok.
wait_python_stop(PythonPort) ->
    wait_python_stop(PythonPort, "").

-spec wait_python_stop(port(), string()) -> ok.
wait_python_stop(PythonPort, Acc) ->
    receive
        {PythonPort, {exit_status, _Status}} -> ok;
        {PythonPort, {data, {eol, Line}}} ->
            lager:info(Acc ++ Line, []),
            wait_python_stop(PythonPort, "");
        {PythonPort, {data, {noeol, Line}}} ->
            wait_python_stop(PythonPort, Acc ++ Line)
    after 5000 ->
        port_close(PythonPort),
        ok
    end.

-spec execute_python_command(port(), port(), string(), atom(), [term()]) -> ok | term().
execute_python_command(PythonPort, MetricsPipe, Worker, Func, Args) ->
    FuncName = atom_to_list(Func),
    ParamStr = string:join(lists:map(
        fun(Term) when is_list(Term) -> "'" ++ Term ++ "'";
            (Term) when is_integer(Term) -> erlang:integer_to_list(Term);
            (Term) when is_float(Term) -> erlang:float_to_list(Term)
        end, Args), ", "),
    port_command(PythonPort, io_lib:format("~s.~s(~s)\nmzbench._instruction_end()\n", [Worker, FuncName, ParamStr])),
    read_python_output(PythonPort, MetricsPipe).

-spec read_python_output(port(), port()) -> ok | term().
read_python_output(PythonPort, MetricsPipe) ->
    read_python_output(PythonPort, MetricsPipe, {"", ""}).

-spec read_python_output(port(), port(), {string(), string()}) -> ok | term().
read_python_output(PythonPort, MetricsPipe, {PythonAcc, MetricsAcc}) ->
    receive
        {PythonPort, {data, {eol, Line}}} ->
            lager:info(PythonAcc ++ Line, []),
            read_python_output(PythonPort, MetricsPipe, {"", MetricsAcc});
        {PythonPort, {data, {noeol, Line}}} ->
            read_python_output(PythonPort, MetricsPipe, {PythonAcc ++ Line, MetricsAcc});
        {MetricsPipe, {data, {eol, Line}}} ->
            case interpret_metrics_pipe(MetricsAcc ++ Line) of
                execution_finished -> ok;
                {metrics_list, Metrics} -> Metrics;
                {funcs_list, FuncsList} -> FuncsList;
                continue -> read_python_output(PythonPort, MetricsPipe, {PythonAcc, ""})
            end;
        {MetricsPipe, {data, {noeol, Line}}} ->
            read_python_output(PythonPort, MetricsPipe, {PythonAcc, MetricsAcc ++ Line})
    end.

-spec interpret_metrics_pipe(string()) -> execution_finished | {metrics_list, term()} | {funcs_list, term()} | continue.
interpret_metrics_pipe(String) ->
    case hd(String) of
        $T -> execution_finished;
        $D ->
            MetricsText = string:substr(String, 2),
            {metrics_list, eval_erlang_term(MetricsText)};
        $M ->
            MetricUpdateText = string:substr(String, 2),
            {Metric, Value} = eval_erlang_term(MetricUpdateText),
            mzb_metrics:notify(Metric, Value),
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

-spec run(string(), [atom()]) -> {ok, string()} | {error, {integer(), string()}}.
run(Command, Opts) ->
    Port = open_port({spawn, Command}, [stream, eof, exit_status | Opts]),
    case get_data(Port, "") of
        {0, Output} ->
            _ = string:strip(Output, right, $\n),
            {ok, Output};
        {Code, Output} ->
            {error, {Code, Output}}
    end.

-spec get_data(port(), string()) -> {integer(), string()}.
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
