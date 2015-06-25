-module(mzb_pipeline).

-behaviour(gen_server).

-export([
    start_link/3,
    call/2,
    stop/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {stage       = undefined,
                module      = undefined,
                ref         = undefined,
                user_state  = undefined,
                logger      = undefined,
                active      = true}).

-callback init(Args :: term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
-callback workflow_config(State :: term()) ->
    [{pipeline | finalize, [Stage :: atom()]}].
-callback get_logger(State :: term()) -> 
    fun ((Severity :: atom(), Format :: list(), Args :: list()) -> ok).
-callback handle_stage(Pipeline :: atom(), Stage :: atom(), State :: term()) ->
    fun ((State :: term()) ->  NewState :: term()) | ok.
-callback handle_pipeline_status(Status :: term(), State :: term()) ->
    NewState :: term().
-callback handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                      State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {reply, Reply :: term(), NewState :: term(), timeout() | hibernate} |
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_info(Info :: timeout() | term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.
-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                    State :: term()) ->
    term().

start_link(Module, Args, Options) ->
    gen_server:start_link(?MODULE, [Module, Args], Options).

call(ServerRef, Request) ->
    gen_server:call(ServerRef, Request).

stop(ServerRef) ->
    gen_server:call(ServerRef, {workflow, stop}).

init([Module, UserArgs]) ->
    Ref = make_ref(),
    case Module:init(UserArgs) of
        {ok, UserState} ->
            ok = gen_server:cast(self(), {workflow, start_phase, pipeline, Ref}),
            {ok, #state{module = Module, user_state = UserState, ref = Ref, logger = Module:get_logger(UserState)}};
        {ok, UserState, Timeout} ->
            ok = gen_server:cast(self(), {workflow, start_phase, pipeline, Ref}),
            {ok, #state{module = Module, user_state = UserState, ref = Ref, logger = Module:get_logger(UserState)}, Timeout};
        {stop, Reason} -> {stop, Reason};
        ignore -> ignore
    end.

handle_call({workflow, stop}, _From, State = #state{active = false, stage = Stage}) ->
    log(info, "Force stop ignored, bench marked as not active ~p", [Stage], State),
    {reply, ok, State};

handle_call({workflow, stop}, _From, State = #state{stage = Stage}) ->
    log(info, "Pipeline force stopped. Current stage ~p", [Stage], State),
    case Stage of
        {Pid, pipeline, _} ->
            try
                unlink(Pid),
                exit(Pid, kill)
            catch _C:E ->
                ST = erlang:get_stacktrace(),
                log(info, "Killing pipeline stage is failed with reason ~p~nStacktrace: ~p", [E, ST], State)
            end;
        _ -> nothing
    end,
    NewRef = make_ref(),
    gen_server:cast(self(), {workflow, start_phase, finalize, NewRef}),
    NewState = change_pipeline_status(stopped, State),
    {reply, ok, NewState#state{ref = NewRef}};

handle_call(Msg, From, #state{module = Module, user_state = UserState} = State) ->
    apply_user_state(Module:handle_call(Msg, From, UserState), State).

handle_cast({workflow, exception, Phase = finalize, Stage, Ref, {_C, E, ST} }, State = #state{ref = Ref}) ->
    log(error, "Stage '~s - ~s': failed~n~s", [Phase, Stage, format_error(Stage, {E, ST})], State),
    gen_server:cast(self(), {workflow, next, finalize, Stage, Ref}),
    {noreply, State#state{stage = undefined}};

handle_cast({workflow, exception, Phase = pipeline, Stage, Ref, {_C, E, ST} }, State = #state{ref = Ref}) ->
    log(error, "Stage '~s - ~s': failed~n~s", [Phase, Stage, format_error(Stage, {E, ST})], State),
    gen_server:cast(self(), {workflow, start_phase, finalize, Ref}),
    NewState = change_pipeline_status(failed, State),
    {noreply, NewState#state{stage = undefine}};

handle_cast({workflow, completed, Phase, Stage, Ref, Fn}, State = #state{ref = Ref}) ->
    log(info, "Stage '~s - ~s': finished", [Phase, Stage], State),
    gen_server:cast(self(), {workflow, next, Phase, Stage, Ref}),

    NewState = case is_last_stage(Phase, Stage, State) of
        true when Phase == pipeline ->
            change_pipeline_status(complete, State);
        _ -> State
    end,
    NewState1 = migrate_state(Fn, NewState),
    NewState2 = NewState1#state{stage = undefined},

    {noreply, NewState2};

handle_cast({workflow, start_phase, Phase, Ref}, State = #state{ref = Ref}) ->
    handle_cast({workflow, next, Phase, undefined, Ref}, State);

handle_cast({workflow, next, PrevPhase, PrevStage, Ref}, State = #state{ref = Ref}) ->
    case next_stage(PrevPhase, PrevStage, State) of
        none ->
            {stop, normal, State};
        {ok, NextPhase, NextStage} ->
            gen_server:cast(self(), {workflow, start, NextPhase, NextStage, Ref}),
            {noreply, State}
    end;

handle_cast({workflow, start, pipeline, Stage, Ref}, State = #state{ref = Ref, active = false}) ->
    log(info, "Stage '~s - ~s': ignored, bench marked as finished", [pipeline, Stage], State),
    {noreply, State};

handle_cast({workflow, start, Phase, Stage, Ref}, State = #state{ref = Ref, module = Module, user_state = UserState}) ->
    log(info, "Stage '~s - ~s': started", [Phase, Stage], State),
    Self = self(),
    Pid = spawn_link(
        fun () ->
            try
                StageResult = Module:handle_stage(Phase, Stage, UserState),
                gen_server:cast(Self, {workflow, completed, Phase, Stage, Ref, StageResult})
            catch
                C:E ->
                    ST = erlang:get_stacktrace(),
                    gen_server:cast(Self, {workflow, exception, Phase, Stage, Ref, {C, E, ST}})
            end
        end),
    NewState = change_pipeline_status({Phase, Stage}, State),
    {noreply, NewState#state{stage = {Pid, Phase, Stage}}};

handle_cast(Msg, #state{module = Module, user_state = UserState} = State) ->
    apply_user_state(Module:handle_cast(Msg, UserState), State).

handle_info(Msg, #state{module = Module, user_state = UserState} = State) ->
    apply_user_state(Module:handle_info(Msg, UserState), State).

terminate(Reason, #state{module = Module, user_state = UserState}) ->
    Module:terminate(Reason, UserState).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

change_pipeline_status(Status, #state{module = Module, user_state = UserState} = State) ->
    NewState = case Status of
                   {pipeline, _} -> State;
                   _ -> State#state{active = false}
               end,
    NewUserState = Module:handle_pipeline_status(Status, UserState),
    NewState#state{user_state = NewUserState}.

apply_user_state({reply, Reply, UserState}, State) -> {reply, Reply, State#state{user_state = UserState}};
apply_user_state({reply, Reply, UserState, Timeout}, State) -> {reply, Reply, State#state{user_state = UserState}, Timeout};
apply_user_state({noreply, UserState}, State) -> {noreply, State#state{user_state = UserState}};
apply_user_state({noreply, UserState, Timeout}, State) -> {noreply, State#state{user_state = UserState}, Timeout};
apply_user_state({stop, Reason, Reply, UserState}, State) -> {stop, Reason, Reply, State#state{user_state = UserState}};
apply_user_state({stop, Reason, UserState}, State) -> {stop, Reason, State#state{user_state = UserState}}.

next_stage(Phase, undefined, #state{module = Module, user_state = UserState} = State) ->
    Stages = proplists:get_value(Phase, Module:workflow_config(UserState), []),
    pick_stage(Phase, Stages, State);
next_stage(Phase, CurrentStage, #state{module = Module, user_state = UserState} = State) when CurrentStage /= undefined ->
    Stages = proplists:get_value(Phase, Module:workflow_config(UserState), []),
    [CurrentStage | RemainStages] = lists:dropwhile(fun(S) -> (S /= CurrentStage) end, Stages),
    pick_stage(Phase, RemainStages, State).

is_last_stage(Phase, Stage, #state{module = Module, user_state = UserState}) ->
    Stages = proplists:get_value(Phase, Module:workflow_config(UserState), []),
    case lists:reverse(Stages) of
        [Stage | _Rest] -> true;
        _ -> false
    end.

pick_stage(finalize, [], _) -> none;
pick_stage(pipeline, [], State) -> next_stage(finalize, undefined, State);
pick_stage(Phase, [NextStage | _Rest], _) -> {ok, Phase, NextStage}.

migrate_state(Fn, #state{user_state = UserState} = State) when is_function(Fn) ->
    State#state{user_state = Fn(UserState)};
migrate_state(_, State) -> State.

format_error(_, {{cmd_failed, Cmd, Code, Output}, _}) ->
    io_lib:format("Command returned ~b:~n ~s~nCommand output: ~s", [Code, Cmd, Output]);
format_error(Op, {E, Stack}) ->
    io_lib:format("Benchmark has failed on ~p with reason:~n~p~n~nStacktrace: ~p", [Op, E, Stack]).

log(Severity, Format, Args, #state{logger = Logger}) ->
    Logger(Severity, Format, Args).
