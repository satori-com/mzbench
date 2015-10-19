-module(mzbl_ast).

-export([
    transform/1,
    add_meta/2,
    map_meta/2,
    mapfold/3,
    fold/3,
    var_eval/3,
    var_mapfold/4,
    find_operation_and_extract_args/2,
    find_operation_and_extract_args/3
    ]).

-include("mzbl_types.hrl").

-spec add_meta(abstract_expr(), meta()) -> abstract_expr().
add_meta(Expr, NewMeta) ->
    map_meta(fun (Meta, _) -> Meta ++ NewMeta end, Expr).

-spec map_meta(fun((meta(), atom()) -> meta()), abstract_expr()) -> abstract_expr().
map_meta(Fun, #operation{name = Name, meta = Meta, args = Args} = Op) ->
    Op#operation{meta = Fun(Meta, Name), args = map_meta(Fun, Args)};
map_meta(Fun, L) when is_list(L) -> lists:map(fun(X) -> map_meta(Fun, X) end, L);
map_meta(_, C) -> C.

-spec fold(fun((abstract_expr(), term()) -> term()), term(), abstract_expr()) -> term().
fold(Fun, Acc, #operation{args = Args} = Op) ->
    fold(Fun, Fun(Op, Acc), Args);
fold(Fun, Acc, L) when is_list(L) ->
    lists:foldl(fun(X, Acc2) -> fold(Fun, Acc2, X) end, Acc, L);
fold(Fun, Acc, #constant{value = Val} = C) ->
    fold(Fun, Fun(C, Acc), Val);
fold(Fun, Acc, C) ->
    Fun(C, Acc).

-spec mapfold(Fun, Acc, Tree) -> {Res, AccRes} when
    Fun :: fun((abstract_expr(), term()) -> {abstract_expr(), term()}),
    Acc :: term(),
    Tree :: abstract_expr(),
    Res :: abstract_expr(),
    AccRes :: term().
mapfold(Fun, Acc, #operation{args = Args} = Op) ->
    {NewArgs, NewAcc} = mapfold(Fun, Acc, Args),
    Fun(Op#operation{args = NewArgs}, NewAcc);
mapfold(Fun, Acc, #constant{value = Val} = C) ->
    {NewVal, NewAcc} = mapfold(Fun, Acc, Val),
    Fun(C#constant{value = NewVal}, NewAcc);
mapfold(Fun, Acc, L) when is_list(L) ->
    lists:mapfoldl(fun(X, Acc2) -> mapfold(Fun, Acc2, X) end, Acc, L);
mapfold(Fun, Acc, C) ->
    Fun(C, Acc).

-spec markup(abstract_expr()) -> abstract_expr().
markup({tuple, Line, [{atom, L2, Op} | Params]}) ->
    {tuple, Line, [{atom, L2, Op}, {cons, L2, {tuple, L2, [{atom, L2, line}, {integer, L2, Line}]}, {nil, L2}} |
                 markup(Params)]};
markup({tuple, Line, [{tuple, _, _} = S | T]}) ->
    {tuple, Line, [markup(S) | T]};
markup({tuple, Line, []}) ->
    {tuple, Line, [{cons, Line, {tuple, Line, [{atom, Line, line}, {integer, Line, Line}]}, {nil, Line}}]};

markup(L) when is_list(L) -> lists:map(fun markup/1, L);
markup(T) when is_tuple(T) ->
    case tuple_to_list(T) of
        [cons, L | S] -> list_to_tuple([cons, L | markup(S)]);
        _ -> T
    end;
markup(S) -> S.

-spec records(term()) -> term().
records(L) when is_list(L) -> lists:map(fun records/1, L);
records(T) when is_tuple(T) ->
    case tuple_to_list(T) of
        [N, Units] when is_number(N) -> #constant{value = N, units = Units};
        % FIXME: this doesn't handle {var, {{var, "name"}, float}}
        [VarName, VarType] when is_list(VarName) -> {VarName, VarType};
        [T2, Units] when is_tuple(T2) -> #constant{value = records(T2), units = Units};
        [Name, Meta | Params] when is_atom(Name) ->
            IsStd = mzbl_stdlib_signatures:is_std_function(Name, length(Params)),
            #operation{
                name = Name,
                meta = Meta,
                args = records(Params),
                is_std = IsStd};
        [Name, _ | _] -> erlang:error({invalid_operation_name, Name});
        [Meta] -> #operation{name = undefined, meta = Meta, args = []}
    end;
records(S) -> S.

-spec transform(abstract_expr()) -> term().
transform(AST) ->
    records(erl_parse:normalise(markup(AST))).

-spec find_operation_and_extract_args(term(), [tuple()]) -> term().
find_operation_and_extract_args(Key, L)  -> find_operation_and_extract_args(Key, L, undefined).

-spec find_operation_and_extract_args(term(), [tuple()], term()) -> term().
find_operation_and_extract_args(_, [], Default) -> Default;
find_operation_and_extract_args(Key, [#operation{name = Key, args = Args} | _], _) -> Args;
find_operation_and_extract_args(Key, [_ | T], Default) -> find_operation_and_extract_args(Key, T, Default).

-spec var_mapfold(Fun, Acc, Script, Env) -> {NewScript, NewAcc}
        when Fun :: fun((Name :: string(), Val :: term(), Acc) ->
                         {change, NewVal :: abstract_expr(), NewAcc} |
                         {nochange, NewAcc :: term()}),
             Acc :: term(),
             Script :: abstract_expr(),
             NewScript :: abstract_expr(),
             Env :: [{Name :: string(), Val :: term()}].
var_mapfold(Fun, AccStart, Script, Env) ->
    NormEnv = mzbl_script:normalize_env(Env),
    mapfold(
        fun (#operation{name = VarType, args = [VarName | _] = Args} = Op, Acc) when VarType == var; VarType == numvar ->
                ReplaceFun = fun (N, V, A) ->
                        case Fun(N, V, A) of
                            {change, NewV, NewA} -> {NewV, NewA};
                            {nochange, NewA} -> {Op, NewA}
                        end
                    end,
                try var_eval(VarType, NormEnv, Args) of
                    Value -> ReplaceFun(VarName, Value, Acc)
                catch
                    error:{var_is_unbound, _} -> ReplaceFun(VarName, unbound, Acc)
                end;
            (Op, Acc) ->
                {Op, Acc}
        end, AccStart, Script).

-spec var_eval(var | numvar, Env, Args) -> Value
        when Env :: [{string(), any()}],
             Args :: [any()],
             Value :: any().
var_eval(var, Env, [Name, Default]) ->
    case proplists:is_defined(Name, Env) of
        false -> Default;
        true -> mzb_utility:cast_to_type(proplists:get_value(Name, Env), Default)
    end;
var_eval(var, Env, [Name]) ->
    case proplists:is_defined(Name, Env) of
        false -> erlang:error({var_is_unbound, Name});
        true -> proplists:get_value(Name, Env)
    end;
var_eval(numvar, Env, Args) ->
    case var_eval(var, Env, Args) of
        undefined -> undefined;
        Value -> mzb_utility:any_to_num(Value)
    end.

