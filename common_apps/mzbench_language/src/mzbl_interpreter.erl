-module(mzbl_interpreter).

-export([eval/4, eval_std/2]).

-include("mzbl_types.hrl").

-spec eval_std(Expr :: [script_expr()], Env :: worker_env()) -> term().
eval_std(Expr, Env) ->
    {Res, _} = eval(Expr, undefined, Env, undefined),
    Res.

-spec eval(script_expr(), worker_state(), worker_env(), module())
    -> {script_value(), worker_state()}.
%% Generates exception: error:{mzbl_interpreter_runtime_error, {{Error, _Reason}, State }}
eval(Expr, State, Env, WorkerProvider) ->
    try
        eval_(Expr, State, Env, WorkerProvider)
    catch
        error:{mzbl_interpreter_runtime_error, _} = E ->
            erlang:raise(error, E, erlang:get_stacktrace());
        E:R ->
            erlang:raise(error, {mzbl_interpreter_runtime_error, {{E, R}, State}}, erlang:get_stacktrace())
    end.

-spec eval_(script_expr(), worker_state(), worker_env(), module())
    -> {script_value(), worker_state()}.
eval_(#operation{is_std = false, name = Name, args = Args, meta = Meta}, State, Env, WorkerProvider) ->
    eval_function(Name, Args, Meta, State, Env, WorkerProvider);
eval_(#operation{is_std = true, name = Name, args = Args, meta = Meta}, State, Env, WorkerProvider) ->
    eval_std_function(Name, Args, Meta, State, Env, WorkerProvider);
eval_(ExprList, State, Env, WorkerProvider) when is_list(ExprList) ->
    lists:foldl(fun(E, {EvaluatedParams, CurrentState}) ->
                    {Result, S} = eval(E, CurrentState, Env, WorkerProvider),
                    {EvaluatedParams ++ [Result], S}
              end,
              {[], State},
              ExprList);
eval_(#constant{value=V} = C, State, Env,  WorkerProvider) ->
    {Value, NewState} = eval(V, State, Env, WorkerProvider),
    {C#constant{value=Value}, NewState};
eval_(Value, State, _Env,  _) -> {Value, State}.

eval_function(Name, [], Meta, State, _, WorkerProvider) ->
    WorkerProvider:apply(Name, [], State, Meta);
eval_function(Name, Args, Meta, State, Env, WorkerProvider) ->
    %% Eager left-to-right evaluation of parameters.
    {Params, NextState} = eval(Args, State, Env, WorkerProvider),
    WorkerProvider:apply(Name, Params, NextState, Meta).

eval_std_function(parallel, [Body], _, State, Env, WorkerProvider) ->
    [R | _] = mzb_lists:pmap(fun (E) -> eval(E, State, Env, WorkerProvider) end, Body), R;

eval_std_function(loop, [LoopSpec, Body], _, State, Env, WorkerProvider) ->
    mzbl_loop:eval(LoopSpec, Body, State, Env, WorkerProvider);
eval_std_function(t, Args, _, State, Env, WorkerProvider) ->
    {Params, NextState} = eval(Args, State, Env, WorkerProvider),
    {list_to_tuple(Params), NextState};
eval_std_function(Profile, Args, _, State, Env, WorkerProvider)
    when Profile == ramp; Profile == comb; Profile == think_time ->
    {Params, NextState} = eval(Args, State, Env, WorkerProvider),
    {#operation{name = Profile, args = Params}, NextState};
eval_std_function('compiled-var', [Name], _, State, _, _) ->
    {mzb_compiled_vars:Name(), State};
eval_std_function(ignore_failure, Args, _, State, Env, WorkerProvider) ->
    try eval(Args, State, Env, WorkerProvider)
    catch
        _:E -> {E, State}
    end;
eval_std_function(Name, [], Meta, State, Env, _) ->
    apply(mzbl_stdlib, Name, [State, Env, Meta]);
eval_std_function(Name, Args, Meta, State, Env, WorkerProvider) ->
    %% Eager left-to-right evaluation of parameters.
    {Params, NextState} = eval(Args, State, Env, WorkerProvider),
    apply(mzbl_stdlib, Name, [NextState, Env, Meta | Params]).

