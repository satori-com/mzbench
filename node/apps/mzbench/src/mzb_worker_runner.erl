-module(mzb_worker_runner).

-export([run_worker_script/5]).

% For Common and EUnit tests
-export([eval_expr/4, time_of_next_iteration_in_ramp/4, msnow/0]).

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
    eval_loop(LoopSpec, Body, State, Env, WorkerProvider);
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
    Evaluator =
        fun (Expr) ->
            fun (S) ->
                {Value, NewS} = eval_expr(Expr, S, Env, WorkerProvider),
                case mzbl_literals:convert(Value) of
                    #constant{value = R} -> {R, NewS};
                    R -> {R, NewS}
                end
            end
        end,
    ArgEvaluator =
        fun (Name, Spec, Default) ->
            case lists:keyfind(Name, #operation.name, Spec) of
                false -> fun (S) -> {Default, S} end;
                #operation{args = [Expr]} -> Evaluator(Expr)
            end
        end,

    TimeFun = ArgEvaluator(time, LoopSpec, undefined),
    {Iterator, State1} = (ArgEvaluator(iterator, LoopSpec, undefined))(State),
    {ProcNum, State2} = (ArgEvaluator(parallel, LoopSpec, 1))(State1),
    {Spawn, State3} = (ArgEvaluator(spawn, LoopSpec, false))(State2),
    case lists:keyfind(rate, #operation.name, LoopSpec) of
        #operation{args = [#constant{value = 0, units = rps}]} ->
            {nil, State3};
        false ->
            RPSFun = fun (S) -> {undefined, S} end,
            looprun(ProcNum, TimeFun, Iterator, Spawn, {constant, RPSFun}, Body, WorkerProvider, State3, Env);
        #operation{args = [#constant{value = _, units = _} = RPS]} ->
            RPSFun = Evaluator(RPS),
            looprun(ProcNum, TimeFun, Iterator, Spawn, {constant, RPSFun}, Body, WorkerProvider, State3, Env);
        #operation{args = [#operation{name = think_time,
                                      args = [#constant{units = _} = Rate,
                                              #constant{units = _} = ThinkTime]}]} ->
            RPSFun1 = Evaluator(Rate),
            PeriodFun1 = fun (S) -> {1000, S} end,
            RPSFun2 = fun (S) -> {0, S} end,
            PeriodFun2 = Evaluator(ThinkTime),
            superloop(ProcNum, TimeFun, Iterator, Spawn,
                [RPSFun1, PeriodFun1, RPSFun2, PeriodFun2], Body, WorkerProvider, State3, Env);
        #operation{args = [#operation{name = comb, args = RatesAndPeriods}]} ->
            RatesAndPeriodsFuns = [Evaluator(E) || E <- RatesAndPeriods],
            superloop(ProcNum, TimeFun, Iterator, Spawn, RatesAndPeriodsFuns, Body, WorkerProvider, State3, Env);
        #operation{args = [#operation{name = ramp, args = [
                                        linear,
                                        #constant{value = _, units = _} = From,
                                        #constant{value = _, units = _} = To]}]} ->
            if
                From == To -> looprun(ProcNum, TimeFun, Iterator, Spawn, {constant, Evaluator(From)}, Body, WorkerProvider, State3, Env);
                true -> looprun(ProcNum, TimeFun, Iterator, Spawn, {linear, Evaluator(From), Evaluator(To)}, Body, WorkerProvider, State3, Env)
            end
    end.

-spec msnow() -> integer().
msnow() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    MegaSecs * 1000000000 + Secs * 1000 + MicroSecs div 1000.

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
    Time = RampDuration / 1000,
    Discriminant = max(0, StartRPS * StartRPS * Time * Time + 2 * IterationNumber * DRPS * Time),
    1000 * (math:sqrt(Discriminant) - StartRPS * Time)
        / DRPS.

superloop(N, TimeFun, Iterator, Spawn, Rates, Body, WorkerProvider, State, Env) ->
    case TimeFun(State) of
        {Time, NewState} when Time =< 0 -> {nil, NewState};
        {Time, NewState} when Rates == [] ->
            timer:sleep(Time),
            {nil, NewState};
        {_, NewState} ->
            [PRateFun, PTimeFun | Tail] = Rates,
            LocalStart = msnow(),
            looprun(N, PTimeFun, Iterator, Spawn, {constant, PRateFun}, Body, WorkerProvider, NewState, Env),
            LoopTime = msnow() - LocalStart,
            NewTimeFun =
                fun (S) ->
                    {T, NewS} = TimeFun(S),
                    {T - LoopTime, NewS}
                end,
            superloop(N, NewTimeFun, Iterator, Spawn, Tail ++ [PRateFun, PTimeFun],
                      Body, WorkerProvider, NewState, Env)
    end.

looprun(1, TimeFun, Iterator, Spawn, Rate, Body, WorkerProvider, State, Env)  ->
    timerun(msnow(), 1, TimeFun, Iterator, Spawn, Rate, undefined, Body, WorkerProvider, Env, 1, State, 0);
looprun(N, TimeFun, Iterator, Spawn, Rate, Body, WorkerProvider, State, Env) ->
    _ = mzb_lists:pmap(fun (I) ->
        timerun(msnow(), N, TimeFun, Iterator, Spawn, Rate, undefined, Body, WorkerProvider, Env, 1, State, I)
    end, lists:seq(0, N - 1)),
    {nil, State}.

timerun(Start, Step, TimeFun, Iterator, Spawn, Rate, PrevRate, Body, WorkerProvider, Env, Batch, State, OldDone) ->
    LocalStart = msnow(),
    {Time, NewState} = TimeFun(State),

    {RateVal, Done, NewState2} = case Rate of
        {constant, RateFun} ->
            {R, S} = RateFun(NewState),
            case PrevRate of
                {constant, R} -> {{constant, R}, OldDone, S};
                undefined -> {{constant, R}, OldDone, S};
                {constant, OldR} -> {{constant, R}, OldDone * R / OldR, S}
            end;
        {linear, FromFun, ToFun} ->
            %{linear, OldF, OldT} = PrevRate,
            {F, S1} = FromFun(NewState),
            {T, S2} = ToFun(S1),
            case PrevRate of
                {linear, F, T} -> {{linear, F, T}, OldDone, S2};
                undefined ->  {{linear, F, T}, OldDone, S2};
                {linear, OldF, OldT} ->
                    Tm = (LocalStart - Start),
                    A = F + (T - F) * Tm / (2 * Time),
                    B = OldF + (OldT - OldF) * Tm / (2 * Time),
                    {{linear, F, T}, OldDone *  A / B, S2}
            end
    end,

    ShouldBe = case RateVal of
        {constant, undefined} -> 0;
        {constant, 0} -> Time * 2; % ShouldBe should be more than loop length "2" does not stand for anything important
        {constant, RPS} -> (Done * 1000) / RPS;
        {linear, From, To} ->
            time_of_next_iteration_in_ramp(From, To, Time, Done)
    end,

    Remain = Start + trunc(ShouldBe) - LocalStart,
    GotTime = Start + trunc(Time) - LocalStart,

    Sleep = max(0, min(Remain, GotTime)),
    if
        Sleep > 1 -> timer:sleep(Sleep);
        true -> ok
    end,
    %io:format("Time: ~p, Start: ~p, LocalStart: ~p, Sleep: ~p~n", [Time, Start, LocalStart, Sleep]),
    case Time + Start =< LocalStart + Sleep of
        true -> {nil, State};
        false ->
            BatchStart = msnow(),
            NextState =
                case Spawn of
                    false ->
                        case Iterator of
                            undefined -> k_times(Body, WorkerProvider, Env, Step, NewState2, Done, Batch);
                            _ -> k_times_iter(Body, WorkerProvider, Iterator, Env, Step, NewState2, Done, Batch)
                        end;
                    true ->
                        k_times_spawn(Body, WorkerProvider, Iterator, Env, Step, NewState2, Done, Batch)
                end,
            BatchEnd = msnow(),
            TimePerIter = max(0, (BatchEnd - BatchStart) div Batch),
            NewBatch =
                if (BatchEnd - BatchStart) * 4 > GotTime -> Batch div 2 + 1;
                   (Sleep == 0) and (Batch < 1000000) -> Batch + Batch div 2 + 1;
                   Sleep > 2*TimePerIter -> max(Batch - Batch div 16 - 1, 1);
                   true -> Batch
                end,

            timerun(Start, Step, TimeFun, Iterator, Spawn, Rate, RateVal, Body, WorkerProvider, Env, NewBatch, NextState, Done + Step*Batch)
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

