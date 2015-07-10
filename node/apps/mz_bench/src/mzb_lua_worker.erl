-module(mzb_lua_worker).

-export([
    init/1,
    load/1,
    apply/4,
    metrics/1,
    terminate/2,
    validate/1,
    validate_function/3
    ]).

-include_lib("mz_bench_language/include/mzbl_types.hrl").
-include_lib("luerl/src/luerl.hrl").

-spec load(worker_name()) -> ok.
load(_) -> ok.

-spec validate(worker_name()) -> [].
validate(Name) ->
    SearchPaths = search_paths(Name),
    lager:info(SearchPaths),
    Files = [filename:absname(filename:join(P, worker_filename(Name))) || P <- SearchPaths],
    lists:any(fun filelib:is_regular/1, Files),
    [].

-spec validate_function(worker_name(), atom(), integer()) -> ok | false | bad_arity.
validate_function(_, main, 0) -> ok;
validate_function(_, main, _) -> bad_arity;
validate_function(_, _, _) -> false.

-spec notify(binary(), binary(), integer() | float()) -> ok.
notify(<<"counter">>, Name, MaybeFloatValue) ->
    IntValue = case is_float(MaybeFloatValue) of
        true -> round(MaybeFloatValue);
        false -> MaybeFloatValue
    end,
    mzb_metrics:notify({binary_to_list(Name), counter}, IntValue);
notify(Type, Name, Value) ->
    mzb_metrics:notify({binary_to_list(Name), binary_to_atom(Type, latin1)}, Value).

-spec stdlib() -> [{string(), fun()}].
stdlib() ->
    [{"notify", fun notify/3}].

-spec inject({string(), fun()}, #luerl{}) -> #luerl{}.
inject({FunName, Fun}, LuaState) ->
    FunPath = [<<"mzbench">>, list_to_binary(FunName)],
    lager:info("FunPath: ~p", [FunPath]),
    luerl:set_table(
        FunPath,
        fun(Args, State) ->
            case apply(Fun, Args) of
                ok -> {[], State};
                Result -> {[Result], State}
            end
        end,
        LuaState).

-spec init(worker_name()) -> #luerl{}.
init(Name) ->
    SearchPaths = search_paths(Name),
    case search_worker_file(Name, SearchPaths) of
        {error, not_found, Filename} ->
            lager:error("worker file ~p not found in ~p", [Filename, SearchPaths]),
            {error, worker_file_not_found};
        {ok, Filename} ->
            {T0, L1} = luerl_emul:alloc_table(luerl:init()),
            L2 = luerl_emul:set_global_key(<<"mzbench">>, T0, L1),
            LuaWithStdlib = lists:foldl(fun inject/2, L2, stdlib()),
            {[], LuaWithUserCode} = luerl:dofile(Filename, LuaWithStdlib),
            LuaWithUserCode
    end.

-spec apply(atom(), [term()], #luerl{}, term()) -> term().
apply(F, Args, LuaWithUserCode, _Meta) ->
    luerl:call_function([F], Args, LuaWithUserCode).

-spec terminate(term(), #luerl{}) -> ok.
terminate(Res, State) ->
    catch luerl:call_function([terminate], [Res], State).

-spec metrics(worker_name()) -> [{string(), gauge | histogram | counter}].
metrics(WorkerName) ->
    lager:info("trying to get metrics from module ~p", [WorkerName]),
    SearchPaths = search_paths(WorkerName),
    {ok, Filename} = search_worker_file(WorkerName, SearchPaths),
    {[], LuaWithUserCode} = luerl:dofile(Filename),
    {[Metrics], _} = luerl:call_function([metrics], [], LuaWithUserCode),
    lists:map(
        fun({BinName, BinType}) ->
            {binary_to_list(BinName),
                case BinType of
                    <<"counter">> -> counter;
                    <<"gauge">> -> gauge;
                    <<"histogram">> -> histogram;
                    T -> erlang:error({unknown_metric_type, T})
                end}
        end,
        Metrics).

-spec search_paths(worker_name()) -> [string()].
search_paths(Name) ->
    [mzbl_utility:expand_filename(filename:join(P, Name))
    || P <- application:get_env(mz_bench, workers_dirs, [])].

-spec worker_filename(worker_name()) -> string().
worker_filename(Name) -> lists:flatten(io_lib:format("~s_worker.lua", [Name])).

-spec search_worker_file(worker_name(), [string()]) -> {ok, string()} | {error, not_found, worker_name()}.
search_worker_file(Name, []) -> {error, not_found, Name};
search_worker_file(Name, [Path|T]) ->
    FullPath = filename:join(Path, worker_filename(Name)),
    case filelib:is_regular(FullPath) of
        true -> {ok, FullPath};
        false -> search_worker_file(Name, T)
    end.