-module(mzb_worker_runner).

-export([run_worker_script/5, eval_expr/4]).

-include_lib("mzbench_language/include/mzbl_types.hrl").
-include("mzb_types.hrl").

-spec run_worker_script([script_expr()], worker_env() , module(), Pool :: pid(), PoolName ::string())
    -> ok.
run_worker_script(Script, Env, {WorkerProvider, Worker}, PoolPid, PoolName) ->
    NodeName = mzb_utility:hostname_str(node()),
    Res =
        try
            _ = random:seed(now()),
            ok = mzb_metrics:notify(mzb_string:format("workers.~s.started", [PoolName]), 1),
            ok = mzb_metrics:notify(mzb_string:format("workers.~s.~s.started", [PoolName, NodeName]), 1),

            InitialState = WorkerProvider:init(Worker),
            {WorkerResult, WorkerResultState} = eval_expr(Script, InitialState, Env, WorkerProvider),
            ok = mzb_metrics:notify(mzb_string:format("workers.~s.ended", [PoolName]), 1),
            ok = mzb_metrics:notify(mzb_string:format("workers.~s.~s.ended", [PoolName, NodeName]), 1),
            _ = (catch  WorkerProvider:terminate(WorkerResult, WorkerResultState)),
            {ok, WorkerResult}
        catch
            C:E ->
                ok = mzb_metrics:notify(mzb_string:format("workers.~s.ended", [PoolName]), 1),
                ok = mzb_metrics:notify(mzb_string:format("workers.~s.~s.ended", [PoolName, NodeName]), 1),
                ok = mzb_metrics:notify(mzb_string:format("workers.~s.failed", [PoolName]), 1),
                ok = mzb_metrics:notify(mzb_string:format("workers.~s.~s.failed", [PoolName, NodeName]), 1),
                _ = (catch WorkerProvider:terminate({C, E, erlang:get_stacktrace()}, undefined)),
                {exception, node(), {C, E, erlang:get_stacktrace()}}
        end,

    PoolPid ! {worker_result, self(), Res},
    ok.

-spec eval_expr(script_expr(), worker_state(), worker_env(), module())
    -> {script_value(), worker_state()}.
eval_expr(#operation{is_std = false, name = Name, args = Args, meta = Meta}, State, Env, WorkerProvider) ->
    eval_function(Name, Args, Meta, State, Env, WorkerProvider);
eval_expr(#operation{is_std = true, name = Name, args = Args, meta = Meta}, State, Env, WorkerProvider) ->
    eval_std_function(Name, Args, Meta, State, Env, WorkerProvider);
eval_expr(ExprList, State, Env, WorkerProvider) when is_list(ExprList) ->
    lists:foldl(fun(E, {EvaluatedParams, CurrentState}) ->
                    {Result, S} = eval_expr(E, CurrentState, Env, WorkerProvider),
                    {EvaluatedParams ++ [Result], S}
              end,
              {[], State},
              ExprList);
eval_expr(#constant{value=V} = C, State, Env,  WorkerProvider) -> 
    {Value, NewState} = eval_expr(V, State, Env, WorkerProvider),
    {C#constant{value=Value}, NewState};
eval_expr(Value, State, _Env,  _) -> {Value, State}.

eval_function(Name, [], Meta, State, _, WorkerProvider) ->
    WorkerProvider:apply(Name, [], State, Meta);
eval_function(Name, Args, Meta, State, Env, WorkerProvider) ->
    %% Eager left-to-right evaluation of parameters.
    {Params, NextState} = eval_expr(Args, State, Env, WorkerProvider),
    WorkerProvider:apply(Name, Params, NextState, Meta).

eval_std_function(parallel, [Body], _, State, Env, WorkerProvider) ->
    [R | _] = mzb_lists:pmap(fun (E) -> eval_expr(E, State, Env, WorkerProvider) end, Body), R;

eval_std_function(loop, [LoopSpec, Body], _, State, Env, WorkerProvider) ->
    mzb_loop:eval(LoopSpec, Body, State, Env, WorkerProvider);
eval_std_function(t, Args, _, State, Env, WorkerProvider) ->
    {Params, NextState} = eval_expr(Args, State, Env, WorkerProvider),
    {list_to_tuple(Params), NextState};
eval_std_function(Profile, Args, _, State, Env, WorkerProvider)
    when Profile == ramp; Profile == comb; Profile == think_time ->
    {Params, NextState} = eval_expr(Args, State, Env, WorkerProvider),
    {#operation{name = Profile, args = Params}, NextState};
eval_std_function('compiled-var', [Name], _, State, _, _) ->
    {mzb_compiled_vars:Name(), State};
eval_std_function(ignore_failure, Args, _, State, Env, WorkerProvider) ->
    try eval_expr(Args, State, Env, WorkerProvider)
    catch
        _:E -> {E, State}
    end;
eval_std_function(Name, [], Meta, State, Env, _) ->
    apply(mzb_stdlib, Name, [State, Env, Meta]);
eval_std_function(Name, Args, Meta, State, Env, WorkerProvider) ->
    %% Eager left-to-right evaluation of parameters.
    {Params, NextState} = eval_expr(Args, State, Env, WorkerProvider),
    apply(mzb_stdlib, Name, [NextState, Env, Meta | Params]).


