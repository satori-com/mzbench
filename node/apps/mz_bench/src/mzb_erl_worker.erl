-module(mzb_erl_worker).

-export([
    load/1,
    init/1,
    apply/4,
    metrics/1,
    terminate/2,
    validate/1,
    validate_function/3]).

validate(Module) ->
    try Module:module_info() of
        _InfoList ->
            true
    catch
        _:_ ->
            false
    end.

validate_function(Module, Fn, Arity) ->
    Fns = Module:module_info(exports),
    case lists:member({Fn, Arity + 2}, Fns) of
        true -> ok;
        _ ->
            case lists:member(Fn, [N || {N, _} <- Fns]) of
                true -> bad_arity;
                false -> not_found
            end
    end.

load(Worker) ->
    ok = load_config(Worker, application:get_env(mz_bench, workers_dirs, [])),
    {ok, _} = ensure_all_started(Worker),
    ok.

init(Module) ->
    {Module, Module:initial_state()}.

apply(F, Args, {Module, State}, Meta) ->
    {Result, NewState} = apply(Module, F, [State, Meta | Args]),
    {Result, {Module, NewState}}.

terminate(Result, {Module, State}) ->
    apply_if_exists(Module, terminate_state, [Result, State]).

apply_if_exists(M, F, A) ->
    case lists:member({F, erlang:length(A)}, M:module_info(exports)) of
        true  -> {ok, erlang:apply(M,F,A)};
        false -> {error, not_exists}
    end.

metrics(Module) -> Module:metrics().

%% backported from R17

ensure_all_started(Application) ->
    ensure_all_started(Application, temporary).

ensure_all_started(Application, Type) ->
    case ensure_all_started(Application, Type, []) of
        {ok, Started} ->
            {ok, lists:reverse(Started)};
        {error, Reason, Started} ->
            _ = [application:stop(App) || App <- Started],
            {error, Reason}
    end.

ensure_all_started(Application, Type, Started) ->
    case application:start(Application, Type) of
        ok ->
            {ok, [Application | Started]};
        {error, {already_started, Application}} ->
            {ok, Started};
        {error, {not_started, Dependency}} ->
            case ensure_all_started(Dependency, Type, Started) of
                {ok, NewStarted} ->
                    ensure_all_started(Application, Type, NewStarted);
                Error ->
                    Error
            end;
        {error, Reason} ->
            {error, {Application, Reason}, Started}
    end.


load_config(_, []) -> ok;
load_config(Worker, [Dir|T]) ->
    WorkerDir = atom_to_list(Worker),
    File = filename:join([Dir, WorkerDir, "sys.config"]),
    case file:consult(File) of
        {ok, [Config]} ->
            lager:info("Reading configuration from ~s", [File]),
            lists:foreach(fun ({App, Env}) ->
                [application:set_env(App, Key, Val) || {Key, Val} <- Env]
            end, Config);
        {error, enoent} ->
            lager:info("Worker ~p config file not found: ~p", [Worker, File]),
            load_config(Worker, T);
        {error, Reason} ->
            lager:error("Could not open file ~p, reason ~p", [File, Reason])
    end.
