-module(mzbl_script).

-export([parse/1,
         hostname/1,
         get_real_script_name/1,
         read/2,
         read_from_string/2,
         get_benchname/1,
         meta_to_location_string/1,
         normalize_env/1,
         substitute/2,
         extract_pools_and_env/2,
         extract_install_specs/1,
         enumerate_pools/1,
         extract_worker/1,
         resolve_worker_provider/1,
         make_git_install_spec/3,
         make_rsync_install_spec/3]).

-include("mzbl_types.hrl").

-spec get_real_script_name([proplists:property()]) -> string().
get_real_script_name(Env) ->
    case lists:keyfind("mzb_script_name", 1, Env) of
        {_K, V} -> V;
        false -> erlang:error(no_real_script_name)
    end.

-spec meta_to_location_string(meta()) -> string().
meta_to_location_string(Meta) ->
    case proplists:get_value(line, Meta) of
        undefined -> "";
        LineNumber -> "line " ++ integer_to_list(LineNumber) ++ ": "
    end.

-spec substitute(abstract_expr(), [proplists:property()]) -> abstract_expr().
substitute(Tree, Env) -> substitute(Tree, Env, []).

-spec substitute(abstract_expr(), [proplists:property()], [string()]) -> abstract_expr().
substitute(#operation{name = loop, args = [Spec, Body]} = Op, Env, Iterators) ->
    NewSpec = substitute(Spec, Env, Iterators),
    NewIterators =
        case mzbl_ast:find_operation_and_extract_args(iterator, NewSpec, [undefined]) of
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
substitute(L, Env, Iterators) when is_list(L) ->
    lists:map(fun(X) -> substitute(X, Env, Iterators) end, L);
substitute(S, _, _) -> S.

-spec read_from_string(string(), [proplists:property()]) -> abstract_expr().
read_from_string(String, Env) ->
    try
        mzbl_literals:convert(substitute(parse(String), Env))
    catch
        C:{parse_error, {_, Module, ErrorInfo}} = E ->
            ST = erlang:get_stacktrace(),
            lager:error("Parsing script file failed: ~s", [Module:format_error(ErrorInfo)]),
            erlang:raise(C,E,ST);
        C:E ->
            ST = erlang:get_stacktrace(),
            lager:error(
				"Failed to read script '~p' 'cause of ~p~nStacktrace: ~s",
                [String, E, pretty_errors:stacktrace(ST)]),
            erlang:raise(C,E,ST)
    end.

-spec read(string(), [proplists:property()]) -> abstract_expr().
read(Path, Env) ->
    try
        read_from_string(read_file(Path), Env)
    catch
        C:E ->
            ST = erlang:get_stacktrace(),
            lager:error(
                "Failed to read script: ~p 'cause of ~p~nStacktrace: ~s",
                [Path, E, pretty_errors:stacktrace(ST)]),
            erlang:raise(C,E,ST)
    end.

-spec parse(string()) -> [script_expr()].
parse(Body) ->
    case erl_scan:string(Body) of
        {ok, [], _} -> [];
        {ok, Ts, _} ->
            case erl_parse:parse_exprs(Ts) of
                {ok, [AST]} ->
                    mzbl_ast:transform(AST);
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
    Script3 = mzbl_ast:map_meta(fun (Meta, Op) -> [{function, Op}|Meta] end, Script2),
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
    {ok, Content} = case 1 == string:str(File, "http://") orelse 1 == string:str(File, "https://") of
        true ->
            {ok, Result} = httpc:request(File),
            {_, _, Body} = Result,
            {ok, Body};
        false ->
            Root = proplists:get_value("bench_script_dir", Env),
            WorkerDirs = proplists:get_value("bench_workers_dir", Env),
            try
                file:read_file(filename:join(Root, File))
            catch
                error:{read_file_error, _, enoent} = E ->
                    Masks = [filename:join([D, "*", "resources", File]) || D <- WorkerDirs],
                    case lists:append([mzb_file:wildcard(M) || M <- Masks])  of
                        [] -> erlang:error(E);
                        [Path|_] -> file:read_file(Path)
                    end
            end
    end,
    convert(Content, Type).

-spec convert(string() | binary(), erlang) -> term();
             (string() | binary(), binary) -> binary();
             (string() | binary(), text) -> string();
             (string() | binary(), json) -> list() | map();
             (string() | binary(), tsv) -> [string()].
convert(X, binary) when is_binary(X) -> X;
convert(X, binary) -> list_to_binary(X);
convert(X, text) when is_binary(X) -> binary_to_list(X);
convert(X, text) -> X;
convert(X, erlang) ->
    S = case is_binary(X) of
        true -> binary_to_list(X);
        false -> X
    end,
    {ok, Tokens, _} = erl_scan:string(S),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term;
convert(X, json) -> jiffy:decode(X, [return_maps]);
convert(X, tsv) -> lists:map(fun(L) -> string:tokens(L, "\t") end, string:tokens(X, "\n"));
convert(X, T) -> erlang:error({invalid_conversion, T, X}).

-spec enumerate_pools([script_expr()]) -> [script_expr()].
enumerate_pools(Pools) ->
    {Pools2, _} = lists:mapfoldl(
        fun (#operation{name = pool} = Op, Number) ->
                {mzbl_ast:add_meta(Op, [{pool_name, "pool" ++ integer_to_list(Number)}]), Number + 1};
            (Op, Number) ->
                {Op, Number}
        end, 1, Pools),
    Pools2.

read_file(File) ->
    case file:read_file(File) of
        {ok, Content} -> erlang:binary_to_list(Content);
        {error, E} -> erlang:error({read_file_error, File, E})
    end.

-spec get_benchname(string()) -> string().
get_benchname(ScriptName) ->
    Name = filename:basename(ScriptName, ".erl"),
    re:replace(Name, "[^a-zA-Z0-9]", "_", [{return, list}, global]).

-spec extract_worker([operation()]) -> {worker_provider(), worker_name()}.
extract_worker(PoolOpts) ->
    WorkerType = mzbl_ast:find_operation_and_extract_args(worker_type, PoolOpts, [undefined]),
    resolve_worker_provider(WorkerType).

-spec resolve_worker_provider([atom()]) -> {worker_provider(), worker_name()}.
resolve_worker_provider(Worker) ->
    case Worker of
        [WorkerName] -> {mzb_erl_worker, WorkerName};
        [WorkerName, erlang] -> {mzb_erl_worker, WorkerName};
        [WorkerName, lua] -> {mzb_lua_worker, WorkerName};
        [WorkerName, python] -> {mzb_python_worker, WorkerName}
    end.

-spec hostname(atom()) -> string().
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

-spec extract_install_specs(abstract_expr()) -> [install_spec()].
extract_install_specs(AST) ->
    Convert =
        fun(#operation{args = [Args]}) ->
            case mzbl_ast:find_operation_and_extract_args(git, Args, undefined) of
                undefined ->
                    case mzbl_ast:find_operation_and_extract_args(rsync, Args, undefined) of
                        undefined -> erlang:error({install_spec_error, missed_mandatory_option, git});
                        [Remote] ->
                            [Excludes] = mzbl_ast:find_operation_and_extract_args(excludes, Args, [[]]),
                            [Subdir] = mzbl_ast:find_operation_and_extract_args(dir, Args, [""]),
                            make_rsync_install_spec(Remote, Subdir, Excludes)
                    end;
                [Repo] ->
                    [Branch] = mzbl_ast:find_operation_and_extract_args(branch, Args, [""]),
                    [Subdir] = mzbl_ast:find_operation_and_extract_args(dir, Args, ["."]),
                    make_git_install_spec(Repo, Branch, Subdir)
            end
        end,
    [Convert(InstallOperation) || (#operation{name = make_install} = InstallOperation) <- AST].

-spec make_git_install_spec(string(), string(), string()) -> git_install_spec().
make_git_install_spec(Repo, Branch, Dir) ->
    #git_install_spec{
        repo = to_string(Repo),
        branch = to_string(Branch),
        dir = to_string(Dir)}.

-spec make_rsync_install_spec(binary() | string(), binary() | string(), [binary() | string()]) -> rsync_install_spec().
make_rsync_install_spec(Remote, Subdir, Excludes) ->
    #rsync_install_spec{
        remote = to_string(Remote),
        dir = to_string(Subdir),
        excludes = [to_string(E) || E <- Excludes]}.

-spec to_string(string() | binary()) -> string().
to_string(X) when is_binary(X) -> binary_to_list(X);
to_string(X) when is_list(X) -> X;
to_string(Y) -> erlang:error({not_a_stringy_thing, Y}).

-spec normalize_env([term()]) -> [term()].
normalize_env(Env) ->
    lists:map(
        fun ({K, V}) -> {normalize_env_(K), normalize_env_(V)}
        end, Env).

normalize_env_(V) when is_binary(V) -> erlang:binary_to_list(V);
normalize_env_(V) when is_list(V) -> V;
normalize_env_(V) when is_integer(V) -> V;
normalize_env_(U) ->
    Msg = mzb_string:format("Env value of unknown type: ~p", [U]),
    erlang:error({error, {validation, [Msg]}}).
