-module(mzb_pipeline).

-behaviour(gen_server).

-export([
    start_link/3,
    call/2,
    call/3,
    cast/2,
    stop/1,
    reply/2,
    error/2,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {stage       = undefined :: undefined | {pid(), atom(), atom()},
                module      = undefined :: undefined | module(),
                ref         = undefined :: undefined | reference(),
                prefinal    = false :: boolean(),
                user_state  = undefined :: term(),
                active      = true :: boolean()}).

-callback init(Args :: term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
-callback workflow_config(State :: term()) ->
    [{pipeline | finalize, [Stage :: atom()]}].
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
    gen_server:call(ServerRef, {user_call, Request}).

call(ServerRef, Request, Timeout) ->
    gen_server:call(ServerRef, {user_call, Request}, Timeout).

cast(ServerRef, Msg) ->
    gen_server:cast(ServerRef, {user_cast, Msg}).

stop(ServerRef) ->
    gen_server:call(ServerRef, {workflow, stop}).

reply(Client, Reply) ->
    gen_server:reply(Client, Reply).

init([Module, UserArgs]) ->
    Ref = make_ref(),
    case Module:init(UserArgs) of
        {ok, UserState} ->
            ok = gen_server:cast(self(), {workflow, start_phase, pipeline, Ref}),
            {ok, #state{module = Module, user_state = UserState, ref = Ref}};
        {ok, UserState, Timeout} ->
            ok = gen_server:cast(self(), {workflow, start_phase, pipeline, Ref}),
            {ok, #state{module = Module, user_state = UserState, ref = Ref}, Timeout};
        {stop, Reason} -> {stop, Reason};
        ignore -> ignore
    end.

-spec error(Reason :: term(), Fun :: undefined | fun()) -> no_return().
error(Reason, Fun) ->
    erlang:error({exception, Reason, Fun}).

handle_call({workflow, stop}, _From, State = #state{active = false}) ->
    {reply, ok, State};

handle_call({workflow, stop}, _From, State = #state{stage = Stage, module = Module,
            user_state = UserState, ref = OldRef}) ->
    PreFinal = case Stage of
        {Pid, pipeline, S} ->
            case lists:member(S, proplists:get_value(unstoppable, Module:workflow_config(UserState), [])) of
                false -> unlink(Pid),
                         exit(Pid, kill),
                         false;
                true -> true
            end;
        _ -> false
    end,
    NewRef = case PreFinal of
        false -> TRef = make_ref(),
                 gen_server:cast(self(), {workflow, start_phase, finalize, TRef}),
                 TRef;
        true -> OldRef
    end,
    NewState = change_pipeline_status({final, stopped}, State),
    {reply, ok, NewState#state{ref = NewRef, prefinal = PreFinal}};

handle_call({user_call, Msg}, From, #state{module = Module, user_state = UserState} = State) ->
    apply_user_state(Module:handle_call(Msg, From, UserState), State);

handle_call(_Unhandled, _From, State) ->
    {noreply, State}.

handle_cast({workflow, exception, Phase = finalize, Stage, Ref, {_C, E, ST, Fn} }, State = #state{ref = Ref}) ->
    NewState0 = migrate_state(Fn, State),
    gen_server:cast(self(), {workflow, next, finalize, Stage, Ref}),
    NewState1 = change_pipeline_status({exception, Phase, Stage, E, ST}, NewState0),
    {noreply, NewState1#state{stage = undefined}};

handle_cast({workflow, exception, Phase = pipeline, Stage, Ref, {_C, E, ST, Fn} }, State = #state{ref = Ref}) ->
    NewState0 = migrate_state(Fn, State),
    gen_server:cast(self(), {workflow, start_phase, finalize, Ref}),
    NewState1 = change_pipeline_status({exception, Phase, Stage, E, ST}, NewState0),
    NewState2 = change_pipeline_status({final, failed}, NewState1),
    {noreply, NewState2#state{stage = undefined}};

handle_cast({workflow, complete, _Phase, _Stage, Ref, Fn},
            State = #state{prefinal = true, ref = Ref}) ->
    NewState = migrate_state(Fn, State),
    gen_server:cast(self(), {workflow, start_phase, finalize, Ref}),
    {noreply, NewState#state{prefinal = false}};

handle_cast({workflow, complete, Phase, Stage, Ref, Fn}, State = #state{ref = Ref}) ->
    NewState0 = migrate_state(Fn, State),
    NewState1 = change_pipeline_status({complete, Phase, Stage}, NewState0),

    gen_server:cast(self(), {workflow, next, Phase, Stage, Ref}),
    NewState2 = case is_last_stage(Phase, Stage, NewState1) of
        true when Phase == pipeline ->
            change_pipeline_status({final, complete}, NewState1);
        _ -> NewState1
    end,
    NewState3 = NewState2#state{stage = undefined},

    {noreply, NewState3};

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

handle_cast({workflow, start, Phase, Stage, Ref}, State = #state{ref = Ref, module = Module, user_state = UserState}) ->
    Self = self(),
    Pid = spawn_link(
        fun () ->
            try
                StageResult = Module:handle_stage(Phase, Stage, UserState),
                gen_server:cast(Self, {workflow, complete, Phase, Stage, Ref, StageResult})
            catch
                C:{exception, E, Fn} ->
                    ST = erlang:get_stacktrace(),
                    gen_server:cast(Self, {workflow, exception, Phase, Stage, Ref, {C, E, ST, Fn}});
                C:E ->
                    ST = erlang:get_stacktrace(),
                    gen_server:cast(Self, {workflow, exception, Phase, Stage, Ref, {C, E, ST, undefined}})
            end
        end),
    NewState = change_pipeline_status({start, Phase, Stage}, State),
    {noreply, NewState#state{stage = {Pid, Phase, Stage}}};

handle_cast({user_cast, Msg}, #state{module = Module, user_state = UserState} = State) ->
    apply_user_state(Module:handle_cast(Msg, UserState), State);

handle_cast(_Unhandled, State) ->
    {noreply, State}.

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
                   {_, pipeline, _} -> State;
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
