-module(mzb_python_worker).

-export([
    load/1,
    init/1,
    apply/3,
    apply/4,
    metrics/1,
    terminate/2,
    validate/1,
    validate_function/3
]).

-include_lib("mzbench_language/include/mzbl_types.hrl").

-record(state, {
    worker_name                         :: string(),
    python_interpreter                  :: term()
}).

-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================
-spec load(worker_name()) -> ok.
load(_Worker) -> ok.

-spec init(worker_name()) -> state().
init(Worker) ->
    Python = mzb_py:start_interpreter(Worker),
    _ = mzb_py:apply(Python, Worker, initial_state, []),
    #state{
        worker_name         =   Worker,
        python_interpreter  =   Python
    }.

-spec apply(Func :: atom(), Args :: [term()], Worker :: worker_name()) -> term().
apply(Func, Args, Worker) ->
    Python = mzb_py:start_interpreter(Worker),
    try
        mzb_py:apply(Python, Worker, Func, Args)
    catch
        _:Reason ->
            erlang:error({python_apply_failed, {Func, Args, Worker}, Reason})
    after
        mzb_py:stop_interpreter(Python)
    end.


-spec apply(atom(), [term()], state(), term()) -> {term(), state()}.
apply(Func, Args, #state{
                            worker_name         = Worker,
                            python_interpreter  = Python
                        } = State, _Meta) ->
    try
        {mzb_py:apply(Python, Worker, Func, Args), State}
    catch
        _:Reason ->
            mzb_py:stop_interpreter(Python),
            erlang:error({python_apply_failed, {Func, Args, Worker}, Reason})
    end.

-spec metrics(worker_name()) -> [term()].
metrics(Worker) ->
    Python = mzb_py:start_interpreter(Worker),
    try
        mzb_script_metrics:normalize(mzb_py:apply(Python, Worker, metrics, []))
    after
        mzb_py:stop_interpreter(Python)
    end.

-spec terminate(term(), state()) -> ok.
terminate(_Result, #state{python_interpreter = Python}) ->
    mzb_py:stop_interpreter(Python).

-spec validate(worker_name()) -> [].
validate(_WorkerName) -> [].

-spec validate_function(worker_name(), atom(), integer()) -> ok | not_found | bad_arity.
validate_function(Worker, Func, _Arity) ->
    Python = mzb_py:start_interpreter(Worker),
    try
        ModuleFuncs = mzb_py:eval(Python, mzb_string:format("mzbench._module_funcs(~s)", [Worker])),
        case lists:member(atom_to_list(Func), ModuleFuncs) of
            true -> ok;
            false -> not_found
        end
    after
        mzb_py:stop_interpreter(Python)
    end.

