-module(mzb_script).

-export([metrics/2,
         parse/1,
         validate/1,
         get_real_script_name/1,
         read_and_validate/2,
         get_benchname/1,
         meta_to_location_string/1,
         substitute/2,
         extract_pools_and_env/2,
         script_metrics/2,
         extract_worker/1]).

-include("mzb_types.hrl").
-include("mzb_ast.hrl").

get_real_script_name(Env) ->
    case lists:keyfind("mzb_script_name", 1, Env) of
        {_K, V} -> V;
        false -> erlang:error(no_real_script_name)
    end.

read_and_validate(ScriptFileName, Env) ->
    try
        Nodes = [node()|nodes()],
        AutoEnv = [{"nodes_num", length(Nodes)},
                   {"bench_hosts", [hostname(N) || N <- Nodes]},
                   {"bench_script_dir", filename:dirname(ScriptFileName)},
                   {"bench_workers_dir", "/mz/mz_bench_workers"}],
        Body = read(ScriptFileName, AutoEnv ++ Env),
        ok = validate(Body),
        {ok, Body, AutoEnv ++ Env}
    catch
        C:{read_file_error, File, E} = Error ->
            Message = io_lib:format(
                "Failed to read file ~s: ~s",
                [File, file:format_error(E)]),
            {error, C, Error, erlang:get_stacktrace(), [lists:flatten(Message)]};
        C:{error, {LineNumber, erl_parse, E}} = Error ->
            Message = io_lib:format(
                "Failed to parse script ~s:~nAt line ~p: ~s~n",
                [ScriptFileName, LineNumber, [E]]),
            {error, C, Error, erlang:get_stacktrace(), [lists:flatten(Message)]};
        C:{error, {validation, VM}} = Error when is_list(VM) ->
            Messages = [lists:flatten(io_lib:format("Script ~s is invalid:~n", [ScriptFileName])) 
                        | VM],
            {error, C, Error, erlang:get_stacktrace(), Messages};
        C:Error ->
            ST = erlang:get_stacktrace(),
            Message = io_lib:format(
                "Script ~s is invalid:~nError: ~p~n~nStacktrace for the curious: ~p",
                [ScriptFileName, Error, ST]),
            {error, C, Error, ST, [lists:flatten(Message)]}
    end.

-spec meta_to_location_string(meta()) -> string().
meta_to_location_string(Meta) ->
    case proplists:get_value(line, Meta) of
        undefined -> "";
        LineNumber -> "line " ++ integer_to_list(LineNumber) ++ ": "
    end.

substitute(Tree, Env) -> substitute(Tree, Env, []).

substitute(#operation{name = loop, args = [Spec, Body]} = Op, Env, Iterators) ->
    NewSpec = substitute(Spec, Env, Iterators),
    NewIterators =
        case mzb_mproplists:get_value(iterator, NewSpec, [undefined]) of
            [undefined] -> Iterators;
            [IterName] ->
                case lists:member(IterName, Iterators) of
                    false -> [IterName | Iterators];
                    true ->
                        erlang:error({substitution_error,
                            iterator_is_shadowed_by_another_iterator, IterName})
                end
        end,
    NewBody = substitute(Body, Env, NewIterators),
    Op#operation{args = [NewSpec, NewBody]};
substitute(#operation{name = OpName, args = Args, meta = Meta} = Op, Env, Iterators)
        when OpName =:= var orelse OpName =:= numvar ->
    [VarName|Rest] = substitute(Args, Env, Iterators),
    case {proplists:get_value(VarName, Env), lists:member(VarName, Iterators), Rest} of
        {undefined, false, [DefaultValue]} -> DefaultValue;
        {undefined, false, _} ->
            erlang:error({substitution_error,
                variable_name_is_unbound, VarName,
                at_location, meta_to_location_string(Meta)});
        {Value, false, []} ->
            case OpName of
                numvar -> mzb_utility:any_to_num(Value);
                var -> Value
            end;
        {Value, false, [DefaultValue]} ->
            case OpName of
                numvar -> mzb_utility:any_to_num(Value);
                var -> cast_to_type(Value, DefaultValue)
            end;
        {undefined, true, []} -> Op#operation{args = [VarName]};
        {undefined, true, _} ->
            erlang:error({substitution_error,
                default_value_supplied_for_iterator, VarName,
                at_location, meta_to_location_string(Meta)});
        {_, true, _} ->
            erlang:error({substitution_error,
                env_variable_is_shadowed_by_an_iterator, VarName,
                at_location, meta_to_location_string(Meta)})
    end;
substitute(#operation{args = Args} = Op, Env, Iterators) ->
    Op#operation{args = substitute(Args, Env, Iterators)};
substitute(#constant{value = V} = C, Env, Iterators) ->
    C#constant{value = substitute(V, Env, Iterators)};
substitute(#ramp{from = F, to = T} = R, Env, Iterators) ->
    R#ramp{from = substitute(F, Env, Iterators), to = substitute(T, Env, Iterators)};
substitute(L, Env, Iterators) when is_list(L) ->
    lists:map(fun(X) -> substitute(X, Env, Iterators) end, L);
substitute(S, _, _) -> S.

% BC code for fast_hist:
convert_metrics({Name, fast_histogram}) -> {Name, histogram};
convert_metrics({Name, Type}) -> {Name, Type};
convert_metrics(List) when is_list(List) -> [convert_metrics(M) || M <- List].
% end of BC

script_metrics(Pools, Nodes) ->
    Metrics = lists:usort(lists:flatmap(fun pool_metrics/1, Pools)),
    % BC code for fast_hist:
    Metrics2 = lists:map(fun convert_metrics/1, Metrics),
    % end of BC
    SystemLoadMetrics = mzb_system_load_monitor:metric_names(Nodes),
    Metrics2 ++ SystemLoadMetrics ++ [{"metric_merging_time", gauge}].

pool_metrics(Pool) ->
    #operation{name = pool, args = [PoolOpts, _Script]} = Pool,
    {Provider, Worker} = extract_worker(PoolOpts),
    Provider:metrics(Worker).

metrics(Path, EnvFromClient) ->
    Script = read(Path, EnvFromClient),
    BenchName = get_benchname(get_real_script_name(EnvFromClient)),
    Nodes = [node() | nodes()],
    {Pools, Env} = extract_pools_and_env(Script, EnvFromClient),

    case mzb_metrics:get_graphite_url(Env) of
        undefined -> undefined_graphite;
        GraphiteUrl ->
            {GraphiteUrl, mzb_metrics:build_graphite_groups(BenchName ++ ".mzb", script_metrics(Pools, Nodes))}
    end.

read(Path, Env) ->
    try
        mzb_literals:convert(substitute(parse(read_file(Path)), Env))
    catch
        C:{parse_error, {_, Module, ErrorInfo}} = E ->
            ST = erlang:get_stacktrace(),
            lager:error("Parsing script file failed: ~s", [Module:format_error(ErrorInfo)]),
            erlang:raise(C,E,ST);
        C:E ->
            ST = erlang:get_stacktrace(),
            lager:error("Failed to read script: ~p 'cause of ~p~nStacktrace: ~p", [Path, E, ST]),
            erlang:raise(C,E,ST)
    end.

-spec parse(string()) -> [script_expr()].
parse(Body) ->
    case erl_scan:string(Body) of
        {ok, [], _} -> [];
        {ok, Ts, _} ->
            case erl_parse:parse_exprs(Ts) of
                {ok, [AST]} ->
                    mzb_ast:transform(AST);
                {error, Error} ->
                    erlang:error({parse_error, Error})
            end;
        {error, Error, _} ->
            erlang:error({parse_error, Error})
    end.

-spec extract_pools_and_env([script_expr()], [{Key::term(), Value::term()}]) ->
    {[#operation{}], [proplists:property()]}.
extract_pools_and_env(Script, Env) ->
    Env2 = lists:foldl(
            fun (#operation{name = include_resource, args = [Name, Path]}, Acc) ->
                    [{{resource, Name}, import_resource(Env, Path, erlang)} | Acc];
                (#operation{name = include_resource, args = [Name, Path, Type]}, Acc) ->
                    [{{resource, Name}, import_resource(Env, Path, Type)} | Acc];
                (#operation{name = assert, args = [Time, Expr]}, Acc) ->
                    {value, {_, Asserts}, Acc2} = lists:keytake(asserts, 1, Acc),
                    [{asserts, [{Time, normalize_assert(Expr)}|Asserts]}|Acc2];
                (_, Acc) -> Acc
            end, [{asserts, []}|Env], Script),

    Script2 = lists:filter(fun (#operation{name = pool}) -> true; (_) -> false end, enumerate_pools(Script)),
    Script3 = mzb_ast:map_meta(fun (Meta, Op) -> [{function, Op}|Meta] end, Script2),
    {Script3, Env2}.

normalize_assert(#operation{name = Op, args = [Op1, Op2]} = A) when is_list(Op2) ->
    A#operation{name = opposite_op(Op), args = [Op2, Op1]};
normalize_assert(#operation{args = [_, _]} = A) ->
    A.

opposite_op(gt) -> lt;
opposite_op(lt) -> gt;
opposite_op(gte) -> lte;
opposite_op(lte) -> gte.

import_resource(Env, File, Type) ->
    Root = proplists:get_value("bench_script_dir", Env),
    WorkerDir = proplists:get_value("bench_workers_dir", Env),
    try
        import_resource(filename:join(Root, File), Type)
    catch
        error:{read_file_error, _, enoent} = E ->
            case filelib:wildcard(filename:join([WorkerDir, "*", "resources", File])) of
                [] -> erlang:error(E);
                [Path|_] -> import_resource(Path, Type)
            end
    end.

import_resource(Path, erlang) ->
    case file:consult(Path) of
        {ok, [Content]} -> Content;
        {error, E} -> erlang:error({read_file_error, Path, E})
    end;
import_resource(Path, binary) ->
    case file:read_file(Path) of
        {ok, Binary} -> Binary;
        {error, E} -> erlang:error({read_file_error, Path, E})
    end;
import_resource(Path, text) ->
    erlang:binary_to_list(import_resource(Path, binary));
import_resource(Path, tsv) ->
    case file:open(Path, [read]) of
        {ok, H} ->
            try
                import_tsv_(Path, file:read_line(H), H, [])
            after
                catch file:close(H)
            end;
        {error, E} -> erlang:error({read_file_error, Path, E})
    end;
import_resource(_Path, Type) ->
    lager:error("Unknown resource file type: ~p", [Type]),
    erlang:error({unknown_resource_type, Type}).

import_tsv_(Path, {ok, Data}, H, Res) ->
    List = string:tokens(string:strip(Data, right, $\n), "\t"),
    import_tsv_(Path, file:read_line(H), H, [List|Res]);
import_tsv_(_Path, eof, _H, Res) ->
    lists:reverse(Res);
import_tsv_(Path, {error, E}, _H, _Res) ->
    erlang:error({read_file_error, Path, E}).

validate(Script) ->
    Script2 = enumerate_pools(Script),
    Errors = lists:foldl(
        fun (#operation{name = include_resource, args = [_Name, Path]}, Acc) ->
                validate_resource_filename(Path) ++ Acc;
            (#operation{name = include_resource, args = [_Name, Path, _Type]}, Acc) ->
                validate_resource_filename(Path) ++ Acc;
            (#operation{name = assert} = Op, Acc) ->
                mzb_asserts:validate(Op) ++ Acc;
            (#operation{name = require_package}, Acc) -> Acc;
            (#operation{name = make_install}, Acc) -> Acc;
            (#operation{name = use_graphite, args = _}, Acc) -> 
                ["use_graphite is deprecated and shouldn't be used anymore."
                    ++ " Use \"graphite\" environment variable instead."] 
                ++ Acc;
            (#operation{name = pool} = Pool, Acc) -> validate_pool(Pool) ++ Acc;
            (#operation{name = F, args = A, meta = M}, Acc) ->
                [lists:flatten(io_lib:format("~sUnknown function: ~p/~p",
                    [meta_to_location_string(M), F, erlang:length(A)]))|Acc];
            (_, Acc) -> Acc
        end, [], Script2),

    case Errors of
        [] -> ok;
        _ -> erlang:error({error, {validation, lists:reverse(Errors)}})
    end,

    mzb_signal_validation:validate(Script2),
    ok.

validate_resource_filename(Filename) ->
    case filename:split(Filename) of
        [_] -> [];
        [".", _] -> [];
        _ -> [lists:flatten(io_lib:format("Invalid resource filename: ~s", [Filename]))]
    end.

-spec enumerate_pools([script_expr()]) -> [script_expr()].
enumerate_pools(Pools) ->
    {Pools2, _} = lists:mapfoldl(
        fun (#operation{name = pool} = Op, Number) ->
                {mzb_ast:add_meta(Op, [{pool_name, "pool" ++ integer_to_list(Number)}]), Number + 1};
            (Op, Number) ->
                {Op, Number}
        end, 1, Pools),
    Pools2.

-spec validate_pool(#operation{}) -> [string()].
validate_pool(#operation{name = pool, args = [Opts, Script]} = Op) ->
    Name = proplists:get_value(pool_name, Op#operation.meta),
    {Provider, Worker} = extract_worker(Opts),
    [Size] = mzb_mproplists:get_value(size, Opts),
    [WorkerStartType] = mzb_mproplists:get_value(worker_start, Opts, [undefined]),
    lists:map(
      fun(Msg) -> Name ++ ": " ++ Msg end,
      case Provider:validate(Worker) of
          true ->
              case Size of
                #operation{name = N, args = A} ->
                    lists:flatten(io_lib:format(
                    "can't use operation ~p with args ~p as pool size.", [N, A]));
                _ -> [lists:flatten(io_lib:format(
                          "size option: expected something integer-like but got ~p.", [Size]))
                          || mzb_utility:to_integer_with_default(Size, fail) == fail] ++
                      ["zero size is not allowed." || mzb_utility:to_integer_with_default(Size, fail) == 0]
              end ++
              case mzb_worker_script_validator:validate_worker_script(Script, {Provider, Worker}) of
                  ok -> [];
                  {invalid_script, Errors} -> Errors
              end ++
              validate_worker_start_type(WorkerStartType);
          false -> [lists:flatten(io_lib:format("unknown worker type: ~p", [Worker]))]
      end).

validate_worker_start_type(undefined) -> [];
validate_worker_start_type(#operation{name = poisson, args = [#constant{value = N, units = rps}]}) when is_number(N), N > 0 -> [];
validate_worker_start_type(#operation{name = poisson, args = [#constant{value = N, units = rps}]}) ->
    [lists:flatten(io_lib:format("Invalid poisson parameter lambda (should be positive number): ~p", [N]))];
validate_worker_start_type(#operation{name = poisson, args = [N]}) ->
    [lists:flatten(io_lib:format("Invalid poisson parameter lambda (should be {<N>, rps}): ~p", [N]))];
validate_worker_start_type(#operation{name = poisson, args = Args}) ->
    [lists:flatten(io_lib:format("Invalid poisson arguments (only one arg is allowed, ~p were given)", [erlang:length(Args)]))];
validate_worker_start_type(#operation{name = linear, args = [#constant{value = N, units = rps}]}) when is_integer(N); N > 0 -> [];
validate_worker_start_type(#operation{name = linear, args = [#constant{value = N, units = rps}]}) ->
    [lists:flatten(io_lib:format("Invalid worker start rate (should be positive integer): ~b", [N]))];
validate_worker_start_type(#operation{name = linear, args = [Arg]}) ->
    [lists:flatten(io_lib:format("Invalid worker start rate: (should be like {<N>, rps}, but '~p' was given)", [Arg]))];
validate_worker_start_type(#operation{name = linear, args = Args}) ->
    [lists:flatten(io_lib:format("Invalid worker start rate arguments: only one arg is allowed, ~p were given", [erlang:length(Args)]))];
validate_worker_start_type(Unknown) ->
    [lists:flatten(io_lib:format("Unknown worker start type: ~p", [Unknown]))].

read_file(File) ->
    case file:read_file(File) of
        {ok, Content} -> erlang:binary_to_list(Content);
        {error, E} -> erlang:error({read_file_error, File, E})
    end.

get_benchname(ScriptName) ->
    Name = filename:basename(ScriptName, ".erl"),
    re:replace(Name, "[^a-zA-Z0-9]", "_", [{return, list}, global]).
    % lists:flatten(io_lib:format("~s-~s", [Name, iso_8601_fmt(erlang:localtime())])).

extract_worker(Opts) ->
    case mzb_mproplists:get_value(worker_type, Opts, [undefined]) of
        [WorkerName] -> {mzb_erl_worker, WorkerName};
        [WorkerName, erlang] -> {mzb_erl_worker, WorkerName};
        [WorkerName, lua] -> {mzb_lua_worker, WorkerName}
    end.

hostname(Node) ->
    [_, H] = string:tokens(erlang:atom_to_list(Node), "@"),
    H.

cast_to_type(Value, TypedValue) when is_binary(Value) and not is_binary(TypedValue) ->
    cast_to_type(binary_to_list(Value), TypedValue);
cast_to_type(Value, TypedValue) when is_list(Value) and is_integer(TypedValue) ->
    list_to_integer(Value);
cast_to_type(Value, TypedValue) when is_integer(Value) and is_float(TypedValue) ->
    float(Value);
cast_to_type(Value, TypedValue) when is_list(Value) and is_float(TypedValue) ->
    try
        list_to_float(Value)
    catch error:badarg ->
        float(list_to_integer(Value))
    end;
cast_to_type(Value, TypedValue) when is_list(Value) and is_binary(TypedValue) ->
    list_to_binary(Value);
cast_to_type(Value, _) -> Value.

