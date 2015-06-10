-module(mzb_lua_worker).

-export([
    load/1,
    init/1,
    apply/4,
    metrics/1,
    terminate/2,
    metric_names/4,
    validate/1,
    validate_function/3
    ]).

validate(Name) ->
    Files = [filename:absname(filename:join(P, worker_filename(Name))) || P <- search_paths(Name)],
    lists:any(fun filelib:is_regular/1, Files).

% TODO: Validator for lua functions
validate_function(_Name, _Fn, _Arity) -> ok.

load(_Name) -> ok.

init(Name) ->
    State0 = luerl:init(),
    {ok, Chunk, State1} = luerl:loadfile(search_worker_file(Name), State0),
    {_Ret, State2} = luerl:do(Chunk, State1),
    State2.

apply(F, Args, State, _Meta) ->
    luerl:call_function([F], Args, State).

terminate(Res, State) ->
    catch luerl:call_function([terminate], [Res], State).

% TODO: Metrics support
metric_names(_Module, _Name, _Meta, _Args) ->
    [].
metrics(_Module) -> {ok, []}.

search_paths(Name) ->
    [filename:join(P, Name) || P <- get_env(mz_bench, workers_dirs, [])].

worker_filename(Name) -> io_lib:format("~s_worker.lua", [Name]).

search_worker_file(Name) -> search_worker_file(Name, search_paths(Name)).
search_worker_file(_Name, []) -> not_found;
search_worker_file(Name, [Path|T]) ->
    FullPath = filename:join(Path, worker_filename(Name)),
    case filelib:is_regular(FullPath) of
        true -> FullPath;
        false -> search_worker_file(Name, T)
    end.

% Theree is no gen_env/3 function in R15
get_env(App, Env, Default) ->
    case application:get_env(App, Env) of
        undefined -> Default;
        {ok, Val} -> Val
    end.
