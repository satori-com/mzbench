-module(mzb_compiler).

-export([compile/2, var_eval/3]).

-include_lib("mzbench_language/include/mzbl_types.hrl").

compile(Pools, Env) ->
    {Pools2, Vars} = mzbl_ast:mapfold(
        fun (T, Acc) ->
            {T2, Vars} = substitute_var(T, Env),
            {T2, Vars ++ Acc}
        end, [], Pools),
    {Pools2, [generate_var_module(Vars)]}.

generate_var_module(Vars) ->
    lager:info("Generating a module for vars: ~p", [Vars]),
    Header = [{attribute,1, module, mzb_compiled_vars},
              {attribute,1,export,[{F, 0} || {F, _} <- Vars]}],

    Functions = lists:map(
        fun ({Function, Value}) ->
            {function, 1, Function, 0, [{clause, 1, [], [], [erl_syntax:revert(erl_syntax:abstract(Value))]}]}
        end, Vars),

    AST = Header ++ Functions,

    {ok, Mod, Bin} = compile:forms(AST),
    {Mod, Bin}.

substitute_var(#operation{name = VarType, args = [VarName | Defaults] = Args} = Op, Env) when VarType == var; VarType == numvar ->
    case is_list(VarName) and (Defaults == [] orelse is_value(hd(Defaults))) of
        true ->
            try var_eval(VarType, Env, Args) of
                Value ->
                    FunName = fun_name(VarName, mzb_utility:type_of(Value)),
                    NewOp = #operation{name = 'compiled-var',
                                       args = [FunName]},
                    {NewOp, [{FunName, Value}]}
            catch
                error:{var_is_undefined, _} -> {Op, []}
            end;
        false ->
            {Op, []}
    end;
substitute_var(Op, _Env) ->
    {Op, []}.

var_eval(var, Env, [Name, Default]) ->
    case proplists:is_defined(Name, Env) of
        false -> Default;
        true -> mzb_utility:cast_to_type(proplists:get_value(Name, Env), Default)
    end;
var_eval(var, Env, [Name]) ->
    case proplists:is_defined(Name, Env) of
        false -> erlang:error({var_is_undefined, Name});
        true -> proplists:get_value(Name, Env)
    end;
var_eval(numvar, Env, Args) ->
    mzb_utility:any_to_num(var_eval(var, Env, Args)).

fun_name(VarName, undefined) -> erlang:list_to_atom(VarName);
fun_name(VarName, Type) -> erlang:list_to_atom(mzb_string:format("~s_~s", [VarName, Type])).

is_value(#constant{}) -> false;
is_value(#operation{}) -> false;
is_value(_) -> true.

