-module(mzbl_loop).

-compile({inline, [msnow/0, eval_rates/7, time_of_next_iteration/3, batch_size/4]}).

-export([eval/5]).

% For Common and EUnit tests
-export([time_of_next_iteration/3, msnow/0]).

-define(MAXSLEEP, 1000).
-define(DEFAULT_MAX_BATCH, 1000000).
-define(MSEC_in_SEC, 1000).

-include("mzbl_types.hrl").

-record(const_rate, {
    rate_fun = undefined :: fun((State :: term()) -> {Rate :: undefined | number(), NewState :: term()}),
    value    = undefined :: undefined | number()
}).
-record(linear_rate, {
    from_fun = undefined :: fun((State :: term()) -> {Rate :: undefined | number(), NewState :: term()}),
    to_fun   = undefined :: fun((State :: term()) -> {Rate :: undefined | number(), NewState :: term()}),
    from     = undefined :: undefined | number(),
    to       = undefined :: undefined | number()
}).

-record(opts, {
    spawn = false        :: true | false,
    iterator = undefined :: undefined | string(),
    parallel = 1         :: pos_integer(),
    poisson = false      :: true | false,
    while = []           :: list()
}).

-spec eval([proplists:property()], [script_expr()], worker_state(), worker_env(), module())
    -> {script_value(), worker_state()}.
eval(LoopSpec, Body, State, Env, WorkerProvider) ->
    % Evaluator returns fun for evaluation of specific expression
    % For example:
    %     F = Evaluator(Expr),
    %     {Res1, State2} = F(State1), % evaluates expression Expr
    %     {Res2, State3} = F(State2)  % evaluates expression Expr again

    Evaluator =
        fun (Expr) ->
            fun (S) ->
                {Value, NewS} = mzbl_interpreter:eval(Expr, S, Env, WorkerProvider),
                case mzbl_literals:convert(Value) of
                    #constant{value = R} -> {R, NewS};
                    R -> {R, NewS}
                end
            end
        end,

    % ArgEvaluator returns fun for finding specific operation in list and evaluation
    % of it's args
    % For example:
    %    Specs = [#operation{name = rate, args = RateArgs}, #operation{name = time, args = TimeArgs}],
    %    F = ArgEvaluator(time, Specs, undefined),
    %    {ArgsRes, State2} = F(State1) % Evaluates TimeArgs and returns results as ArgsRes
    %
    % We use these funs in order to reevaluate some key loop parameters (like
    % rate) while executing loop because vars values could be changed at any
    % moment. So, for instance, instead of passing Rate to a loop function, we
    % pass function that evaluates Rate as soon as we need it.

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
    {Poisson, State4} = (ArgEvaluator(poisson, LoopSpec, false))(State3),
    Asserts = mzbl_ast:find_operation_and_extract_args(while, LoopSpec, []),
    Opts = #opts{
            spawn = Spawn,
            iterator = Iterator,
            parallel = ProcNum,
            poisson = Poisson,
            while = Asserts
        },
    case mzbl_ast:find_operation_and_extract_args(rate, LoopSpec, [#constant{value = undefined, units = rps}]) of
        [#constant{value = _, units = _} = RPS] ->
            RPSFun = Evaluator(RPS),
            looprun(TimeFun, #const_rate{rate_fun = RPSFun}, Body, WorkerProvider, State4, Env, Opts);
        [#operation{name = think_time,
                    args = [#constant{units = _} = ThinkTime,
                            #constant{units = _} = Rate]}] ->
            RPSFun1 = Evaluator(Rate),
            PeriodFun1 = fun (S) -> {1000, S} end,
            RPSFun2 = fun (S) -> {0, S} end,
            PeriodFun2 = Evaluator(ThinkTime),
            superloop(TimeFun, [RPSFun1, PeriodFun1, RPSFun2, PeriodFun2], Body,
                      WorkerProvider, State4, Env, Opts);
        [#operation{name = comb, args = RatesAndPeriods}] ->
            RatesAndPeriodsFuns = [Evaluator(E) || E <- RatesAndPeriods],
            superloop(TimeFun, RatesAndPeriodsFuns, Body, WorkerProvider, State4, Env, Opts);
        [#operation{name = ramp, args = [
                            linear,
                            #constant{value = _, units = _} = From,
                            #constant{value = _, units = _} = To]}] ->
            looprun(TimeFun, #linear_rate{from_fun = Evaluator(From), to_fun = Evaluator(To)}, Body, WorkerProvider, State4, Env, Opts)
    end.

-spec msnow() -> integer().
msnow() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    MegaSecs * 1000000000 + Secs * 1000 + MicroSecs div 1000.

-spec time_of_next_iteration(#const_rate{} | #linear_rate{}, number(), float())
    -> number().
time_of_next_iteration(#const_rate{value = undefined}, _, _) -> 0;
time_of_next_iteration(#const_rate{value = 0}, Duration, _) -> Duration * 2; % should be more than loop length "2" does not stand for anything important
time_of_next_iteration(#const_rate{value = 0.0}, Duration, _) -> Duration * 2;
time_of_next_iteration(#const_rate{value = Rate}, _Duration, IterationNumber) ->
    (IterationNumber * ?MSEC_in_SEC) / Rate;
time_of_next_iteration(#linear_rate{from = F, to = T}, Duration, _) when F == 0, T == 0 -> Duration * 2; % we want to match 0 and 0.0 hence the guard usage
time_of_next_iteration(#linear_rate{from = Rate1, to = Rate2}, _, IterationNumber) when Rate1 == Rate2 ->
    (IterationNumber * ?MSEC_in_SEC) / Rate1;
time_of_next_iteration(#linear_rate{from = StartRPS, to = FinishRPS}, RampDuration, IterationNumber) ->
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

superloop(TimeFun, Rates, Body, WorkerProvider, State, Env, Opts) ->
    case TimeFun(State) of
        {Time, NewState} when Time =< 0 -> {nil, NewState};
        {Time, NewState} when Rates == [] ->
            timer:sleep(Time),
            {nil, NewState};
        {_, NewState} ->
            [PRateFun, PTimeFun | Tail] = Rates,
            LocalStart = msnow(),
            looprun(PTimeFun, #const_rate{rate_fun = PRateFun}, Body, WorkerProvider, NewState, Env, Opts),
            LoopTime = msnow() - LocalStart,
            NewTimeFun =
                fun (S) ->
                    {T, NewS} = TimeFun(S),
                    {T - LoopTime, NewS}
                end,
            superloop(NewTimeFun, Tail ++ [PRateFun, PTimeFun],
                      Body, WorkerProvider, NewState, Env, Opts)
    end.

looprun(TimeFun, Rate, Body, WorkerProvider, State, Env, Opts = #opts{parallel = 1})  ->
    timerun(msnow(), random:uniform(), TimeFun, Rate, Body, WorkerProvider, Env, true, Opts, 1, State, 0, 0, {0, 0});
looprun(TimeFun, Rate, Body, WorkerProvider, State, Env, Opts = #opts{parallel = N}) ->
    StartTime = msnow(),
    _ = mzb_lists:pmap(fun (I) ->
        _ = random:seed(now()),
        timerun(StartTime, I + random:uniform(), TimeFun, Rate, Body, WorkerProvider, Env, true, Opts, 1, State, 0, 0, {0, I})
    end, lists:seq(0, N - 1)),
    {nil, State}.

timerun(Start, Shift, TimeFun, Rate, Body, WorkerProvider, Env, IsFirst, Opts, Batch, State, OldDone, OldIter, OldRun) ->
    case mzbl_asserts:check_loop_expr(Opts#opts.while, Env) of
        false -> {nil, State};
        _ ->

    State = receive
        {run_command, AST} ->
                {_, NewState} = mzbl_interpreter:eval(AST, OldState, Env, WorkerProvider),
                NewState
        after 0 -> OldState
    end,
    LocalTime = msnow() - Start,
    {Time, State1} = TimeFun(State),
    {NewRate, Done, State2, NewRun} = eval_rates(Rate, OldDone, LocalTime, Time, State1, Opts#opts.parallel, OldRun),
    ShouldBe = time_of_next_iteration(NewRate, Time, Done + Shift),
    Remain = round(ShouldBe) - LocalTime,
    GotTime = round(Time) - LocalTime,

    NeedToSleep = max(0, min(Remain, GotTime)),
    Sleep =
        case Opts#opts.poisson of
            true -> max(0, min(round(-Remain * math:log(random:uniform())), GotTime));
            false -> NeedToSleep
        end,
    case Sleep > ?MAXSLEEP of
        true ->
            timer:sleep(?MAXSLEEP),
            timerun(Start, Shift, TimeFun, NewRate, Body, WorkerProvider, Env, IsFirst, Opts, Batch, State2, Done, OldIter, NewRun);
        false ->
            Sleep > 0 andalso timer:sleep(Sleep),
            case Time =< LocalTime + Sleep of
                true -> {nil, State2};
                false ->
                    BatchStart = msnow(),
                    Iterator = Opts#opts.iterator,
                    Step = Opts#opts.parallel,
                    NextState =
                        case Opts#opts.spawn of
                            false ->
                                case Iterator of
                                    undefined -> k_times(Body, WorkerProvider, Env, State2, Batch);
                                    _ -> k_times_iter(Body, WorkerProvider, Iterator, Env, Step, State2, OldIter + erlang:trunc(Shift), Batch)
                                end;
                            true ->
                                k_times_spawn(Body, WorkerProvider, Iterator, Env, Step, State2, OldIter + erlang:trunc(Shift), Batch)
                        end,
                    BatchEnd = msnow(),
                    NewBatch = case IsFirst of
                        true -> Batch;
                        false -> batch_size(BatchEnd - BatchStart, GotTime, NeedToSleep, Batch)
                    end,

                    timerun(Start, Shift, TimeFun, NewRate, Body, WorkerProvider, Env, false, Opts, NewBatch, NextState, Done + Step*Batch, OldIter + Step*Batch, NewRun)
    end
    end
end.

eval_rates(#const_rate{rate_fun = F, value = Prev} = RateState, Done, CurTime, _Time, State, Step, {OldRun, DoneLastUpdate}) ->
    {Rate, State1} = F(State),
    % If rate changes we have to change number of "done" iterations accordingly
    % Also we need to reset done counter every minute or so
    % to make sure we don't generate more load than we were asked for
    % It happens after periods of time when we were unable to maintain needed rate (for some external reasons)
    NewRun = CurTime div (60*?MSEC_in_SEC),

    NewDone =
        case {Rate, Prev} of
            {Prev, _} when (NewRun =< OldRun) orelse (Done < DoneLastUpdate + 10 * Step) -> Done;
            {Prev, _} ->
                ND = Prev * CurTime / ?MSEC_in_SEC,
                case ND > Done + Step of
                    true -> ND;
                    false -> Done
                end;
            {undefined, _} -> Done;
            {_New, undefined} -> Done;
            {New, _} -> (New * CurTime / ?MSEC_in_SEC) % It's important not to round done counter here
        end,

    NewDoneLastUpdate =
        case NewDone == Done of
            true -> DoneLastUpdate;
            false -> NewDone
        end,
    {RateState#const_rate{value = Rate}, NewDone, State1, {NewRun, NewDoneLastUpdate}};
eval_rates(#linear_rate{from_fun = FFun, to_fun = ToFun, from = OldF, to = OldT} = RateState, Done, CurTime, Time, State, Step, {OldRun, DoneLastUpdate}) ->
    {F, State1} = FFun(State),
    {T, State2} = ToFun(State1),
    % If rate changes we have to change number of "done" iterations accordingly
    % Also we need to reset done counter every minute or so
    % to make sure we don't generate more load than we were asked for
    % It happens after periods of time when we were unable to maintain needed rate (for some external reasons)
    NewRun = CurTime div (60*?MSEC_in_SEC),

    NewDone =
        case {F, T} of
            {OldF, OldT} when (NewRun =< OldRun) orelse (Done < DoneLastUpdate + 10 * Step) -> Done;
            {OldF, OldT} ->
                ND = F * CurTime/?MSEC_in_SEC  + (T - F) * CurTime * CurTime / (2 * ?MSEC_in_SEC * Time),
                case ND > Done + Step of
                    true -> ND;
                    false -> Done
                end;
            {_, _} when OldF == undefined -> Done;
            {_, _} ->
                % Calculating area under new ramp graph (which is trapezium)
                % Kinematic equations also could be used (S = v0*t + a*t^2/2)
                %
                % It's important not to round done counter here
                F * CurTime/?MSEC_in_SEC  + (T - F) * CurTime * CurTime / (2 * ?MSEC_in_SEC * Time)
        end,

    NewDoneLastUpdate =
        case NewDone == Done of
            true -> DoneLastUpdate;
            false -> NewDone
        end,
    {RateState#linear_rate{from = F, to = T}, NewDone, State2, {NewRun, NewDoneLastUpdate}}.

batch_size(BatchTime, TimeLeft, Sleep, Batch) ->
    MaxBatch =
        case BatchTime of
            0 -> ?DEFAULT_MAX_BATCH;
            _ -> max(Batch*?MSEC_in_SEC div BatchTime, 1) % Batch execution shouldn't take more than 1 sec
        end,
    TimePerIter = max(0, BatchTime div Batch),
    NewBatch =
        if BatchTime * 4 > TimeLeft -> Batch div 2 + 1;
            (Sleep == 0) and (Batch < MaxBatch) -> Batch + Batch div 2 + 1;
            Sleep > 2*TimePerIter -> max(Batch - Batch div 2 - 1, 1);
            true -> Batch
        end,
    min(NewBatch, MaxBatch).

k_times(_, _, _, S, 0) -> S;
k_times(Expr, Provider, Env, S, N) ->
    {_, NewS} = mzbl_interpreter:eval(Expr, S, Env, Provider),
    k_times(Expr, Provider, Env, NewS, N-1).

k_times_iter(_, _, _, _, _, S, _, 0) -> S;
k_times_iter(Expr, Provider, I, Env, Step, S, Iter, N) ->
    {_, NewS} = mzbl_interpreter:eval(Expr, S, [{I, Iter}|Env], Provider),
    k_times_iter(Expr, Provider, I, Env, Step, NewS, Iter + Step, N-1).

k_times_spawn(_, _, _, _, _, S, _, 0) -> S;
k_times_spawn(Expr, Provider, I, Env, Step, S, Iter, N) ->
    spawn_link(fun() -> mzbl_interpreter:eval(Expr, S, [{I, Iter}|Env], Provider) end),
    k_times_iter(Expr, Provider, I, Env, Step, S, Iter + Step, N-1).
