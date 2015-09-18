-module(mzb_worker_runner).

-export([run_worker_script/5]).

% For Common and EUnit tests
-export([eval_expr/4, time_of_next_iteration_in_ramp/4, mknow/0]).

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
            _ = WorkerProvider:terminate(WorkerResult, WorkerResultState),
            ok = mzb_metrics:notify(mzb_string:format("workers.~s.ended", [PoolName]), 1),
            ok = mzb_metrics:notify(mzb_string:format("workers.~s.~s.ended", [PoolName, NodeName]), 1),
            {ok, WorkerResult}
        catch
            C:E ->
                ok = mzb_metrics:notify(mzb_string:format("workers.~s.ended", [PoolName]), 1),
                ok = mzb_metrics:notify(mzb_string:format("workers.~s.~s.ended", [PoolName, NodeName]), 1),
                ok = mzb_metrics:notify(mzb_string:format("workers.~s.failed", [PoolName]), 1),
                ok = mzb_metrics:notify(mzb_string:format("workers.~s.~s.failed", [PoolName, NodeName]), 1),
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
    {LoopSpec2, NextState} =
    lists:foldl(
        fun (#operation{args = A} = O, {EvaluatedParams, CurrentState}) ->
              {Result, S} = eval_expr(A, CurrentState, Env, WorkerProvider),
              {EvaluatedParams ++ [O#operation{args = Result}], S}
        end,
        {[], State},
        LoopSpec),
    eval_loop(LoopSpec2, Body, NextState, Env, WorkerProvider);
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

-spec eval_loop([proplists:property()], [script_expr()], worker_state(), worker_env(), module())
    -> {script_value(), worker_state()}.
eval_loop(LoopSpec, Body, State, Env, WorkerProvider) ->
    [#constant{value = Time, units = ms}] =
        mzbl_literals:convert(mzbl_ast:find_operation_and_extract_args(time, LoopSpec, [#constant{value = undefined, units = ms}])),
    [Iterator] = mzbl_ast:find_operation_and_extract_args(iterator, LoopSpec, [undefined]),
    [ProcNum] = mzbl_ast:find_operation_and_extract_args(parallel, LoopSpec, [1]),
    [Spawn] = mzbl_ast:find_operation_and_extract_args(spawn, LoopSpec, [false]),

    case mzbl_literals:convert(mzbl_ast:find_operation_and_extract_args(rate, LoopSpec, 
                                        [#constant{value = undefined, units = rps}])) of
        [#constant{value = 0, units = rps}] -> {nil, State};
        [#constant{value = Rps, units = rps}] ->
            looprun(ProcNum, Time, Iterator, Spawn, {constant, Rps}, Body, WorkerProvider, State, Env);
        [#operation{name = think_time, args = [#constant{units = rps} = Rate,
                #constant{units = ms} = ThinkTime]}] ->
            superloop(ProcNum, Time, Iterator, Spawn,
                [Rate, #constant{value = 1000, units = ms}, #constant{value = 0, units = rps}, ThinkTime], Body, WorkerProvider, State, Env);
        [#operation{name = comb, args = RatesANDPeriods}] ->
            superloop(ProcNum, Time, Iterator, Spawn, RatesANDPeriods, Body, WorkerProvider, State, Env);
        [#operation{name = ramp, args = [linear, #constant{value = From, units = rps},
               #constant{value = To, units = rps}]}] ->
            if
                From == To -> looprun(ProcNum, Time, Iterator, Spawn, {constant, From}, Body, WorkerProvider, State, Env);
                true -> looprun(ProcNum, Time, Iterator, Spawn, {linear, From, To}, Body, WorkerProvider, State, Env)
            end
    end.

-spec mknow() -> integer().
mknow() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    MegaSecs * 1000000000000 + Secs * 1000000 + MicroSecs.

-spec time_of_next_iteration_in_ramp(number(), number(), number(), integer())
    -> number().
time_of_next_iteration_in_ramp(StartRPS, FinishRPS, RampDuration, IterationNumber) ->
    % This function solves the following equation for Elapsed:
    %
    % Use linear interpolation for y0 = StartRPS, y1 = FinishRPS, x0 = 0, x1 = RampDuration
    % f(x) = StartRPS + (FinishRPS - StartRPS) * x / Duration, where x - time from start, y - RPS at moment x
    %
    % IterationNumber = Elapsed * (StartRPS + f(Elapsed)) / 2
    %
    % Expanding linear interpolation:
    %
    % IterationNumber = (FinishRPS - StartRPS) * Elapsed ^ 2 / (2 * RampDuration) + StartRPS * Elapsed
    %
    % Solve quadratic equation and choose positive solution
    %
    % Elapsed = (sqrt((StartRPS * Time) ^ 2 + 2 * (FinishRPS - StartRPS) * IterationNumber * RampDuration) - StartRPS * RampDuration) / (FinishRPS - StartRPS)
    %
    % Please note that we could calculate time for the next iteration which could be out of boundary if FinishRPS < StartRPS.
    % To avoid this we use discriminant = 0. Retured value will be higher then RampDuration in this case

    DRPS = FinishRPS - StartRPS,
    Time = RampDuration / 1000000,
    Discriminant = max(0, StartRPS * StartRPS * Time * Time + 2 * IterationNumber * DRPS * Time),
    1000000 * (math:sqrt(Discriminant) - StartRPS * Time)
        / DRPS.

superloop(_, Time, _, _, _, _, _, State, _) when Time =< 0 -> {nil, State};
superloop(_, Time, _, _, [], _, _, State, _) -> timer:sleep(Time), {nil, State};
superloop(N, Time, Iterator, Spawn, [#constant{value = Rate, units = rps} = R,
    #constant{value = PTime, units = ms} = T | Tail], Body, WorkerProvider, State, Env) ->
    LocalStart = mknow(),
    looprun(N, PTime, Iterator, Spawn, {constant, Rate}, Body, WorkerProvider, State, Env),
    superloop(N, Time - ((mknow() - LocalStart) div 1000), Iterator, Spawn, Tail ++ [R, T],
        Body, WorkerProvider, State, Env).

looprun(1, Time, Iterator, Spawn, Rate, Body, WorkerProvider, State, Env)  ->
    timerun(mknow(), 1, Time * 1000, Iterator, Spawn, Rate, Body, WorkerProvider, Env, 1, State, 0);
looprun(N, Time, Iterator, Spawn, Rate, Body, WorkerProvider, State, Env) ->
    _ = mzb_lists:pmap(fun (I) ->
        timerun(mknow(), N, Time * 1000, Iterator, Spawn, Rate, Body, WorkerProvider, Env, 1, State, I)
    end, lists:seq(0, N - 1)),
    {nil, State}.

timerun(Start, Step, Time, Iterator, Spawn, Rate, Body, WorkerProvider, Env, Batch, State, Done) ->
    ShouldBe = case Rate of
        {constant, undefined} -> 0;
        {constant, 0} -> Time * 2; % ShouldBe should be more than loop length "2" does not stand for anything important
        {constant, RPS} -> (Done * 1000000) / RPS;
        {linear, From, To} ->
            time_of_next_iteration_in_ramp(From, To, Time, Done)
    end,
    LocalStart = mknow(),

    Remain = Start + trunc(ShouldBe) - LocalStart,
    GotTime = Start + trunc(Time) - LocalStart,

    Sleep = max(0, min(Remain, GotTime)),
    if
        Sleep > 1000 -> timer:sleep(Sleep div 1000);
        true -> ok
    end,
    %io:format("Time: ~p, Start: ~p, LocalStart: ~p, Sleep: ~p~n", [Time, Start, LocalStart, Sleep]),
    case Time + Start =< LocalStart + Sleep of
        true -> {nil, State};
        false ->
            BatchStart = mknow(),
            NextState =
                case Spawn of
                    false ->
                        case Iterator of
                            undefined -> k_times(Body, WorkerProvider, Env, Step, State, Done, Batch);
                            _ -> k_times_iter(Body, WorkerProvider, Iterator, Env, Step, State, Done, Batch)
                        end;
                    true ->
                        k_times_spawn(Body, WorkerProvider, Iterator, Env, Step, State, Done, Batch)
                end,
            BatchEnd = mknow(),
            TimePerIter = max(0, (BatchEnd - BatchStart) div Batch),
            NewBatch =
                if (BatchEnd - BatchStart) * 4 > GotTime -> Batch div 2 + 1;
                   (Sleep < 1000) and (Batch < 1000000) -> Batch + Batch div 2 + 1;
                   Sleep > 2*TimePerIter -> max(Batch - Batch div 16 - 1, 1);
                   true -> Batch
                end,

            timerun(Start, Step, Time, Iterator, Spawn, Rate, Body, WorkerProvider, Env, NewBatch, NextState, Done + Step*Batch)
    end.

k_times(_, _, _, _, S, _, 0) -> S;
k_times(Expr, Provider, Env, Step, S, Done, N) ->
    {_, NewS} = eval_expr(Expr, S, Env, Provider),
    k_times(Expr, Provider, Env, Step, NewS, Done + Step, N-1).

k_times_iter(_, _, _, _, _, S, _, 0) -> S;
k_times_iter(Expr, Provider, I, Env, Step, S, Done, N) ->
    {_, NewS} = eval_expr(Expr, S, [{I, Done}|Env], Provider),
    k_times_iter(Expr, Provider, I, Env, Step, NewS, Done + Step, N-1).

k_times_spawn(_, _, _, _, _, S, _, 0) -> S;
k_times_spawn(Expr, Provider, I, Env, Step, S, Done, N) ->
    spawn_link(fun() -> eval_expr(Expr, S, [{I, Done}|Env], Provider) end),
    k_times_iter(Expr, Provider, I, Env, Step, S, Done + Step, N-1).

