-module(mzb_compiler).

-export([compile/2]).

-include_lib("mzbench_language/include/mzbl_types.hrl").

compile(Pools, Env) ->
    ReplaceFun =
        fun (_Name, undefined, Acc) -> {nochange, Acc};
            (Name, Value, Acc) ->
                FunName = fun_name(Name, mzb_utility:type_of(Value)),
                NewOp = #operation{name = 'compiled-var',
                                   args = [FunName]},
                NewAcc =
                    case proplists:get_all_values(FunName, Acc) of
                        [] -> [{FunName, Value}|Acc];
                        [_|_] -> Acc
                    end,
                {change, NewOp, NewAcc}
        end,
    {Pools2, Vars} = mzbl_ast:var_mapfold(ReplaceFun, [], Pools, Env),
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

fun_name(VarName, undefined) -> erlang:list_to_atom(VarName);
fun_name(VarName, Type) -> erlang:list_to_atom(mzb_string:format("~s_~s", [VarName, Type])).


