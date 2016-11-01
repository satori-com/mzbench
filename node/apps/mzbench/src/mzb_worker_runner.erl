-module(mzb_worker_runner).

-export([run_worker_script/5]).

-include_lib("mzbench_language/include/mzbl_types.hrl").

-spec run_worker_script([script_expr()], worker_env() , module(), Pool :: pid(), PoolName ::string())
    -> ok.
run_worker_script(Script, Env, {WorkerProvider, Worker}, PoolPid, PoolName) ->
    %NodeName = mzb_utility:hostname_str(node()),
    Res = try
        _ = random:seed(now()),
        ok = mzb_metrics:notify(mzb_string:format("workers.~s.started", [PoolName]), 1),
        InitialState = WorkerProvider:init(Worker),
        {WorkerResult, WorkerResultState} = mzbl_interpreter:eval(Script, InitialState, Env, WorkerProvider),
        WorkerProvider:terminate(WorkerResult, { Worker, WorkerResultState}),
        {ok, WorkerResult}
    catch
        error:{mzbl_interpreter_runtime_error, {{Error, Reason}, ErrorState }} ->
            ST = erlang:get_stacktrace(),
            ok = mzb_metrics:notify(mzb_string:format("workers.~s.failed", [PoolName]), 1),
            try
                WorkerProvider:terminate({ Error, Reason, ST }, { Worker, ErrorState}),
                {exception, node(),  { Error, Reason, ST, ErrorState }}
            catch
                E0:R0 ->
                    {exception, node(), { E0, R0, erlang:get_stacktrace() }}
            end;
        C:E ->
            ST = erlang:get_stacktrace(),
            ok = mzb_metrics:notify(mzb_string:format("workers.~s.failed", [PoolName]), 1),
            try
                WorkerProvider:terminate({C, E, ST}, { Worker, undefined}),
                {exception, node(), {C, E, ST}}
            catch
                E1:R1 ->
                    {exception, node(), { E1, R1, erlang:get_stacktrace() }}
            end
    after
            mzb_metrics:notify(mzb_string:format("workers.~s.ended", [PoolName]), 1)
    end,
    PoolPid ! {worker_result, self(), Res},
    ok.

