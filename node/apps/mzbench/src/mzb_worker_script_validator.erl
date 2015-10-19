-module(mzb_worker_script_validator).

-export([validate_worker_script/2]).

-include_lib("mzbench_language/include/mzbl_types.hrl").

-spec validate_worker_script([#operation{}], {module(), module()})
    -> script_validation_result().
validate_worker_script(Script, Worker) ->
    Errors = lists:flatmap(fun(Expr) ->
                                validate_expr(Expr, Worker)
                           end,
                           Script),
    case Errors of
        [] -> ok;
        _ -> {invalid_script, Errors}
    end.

-spec validate_expr(#operation{} | [#operation{}], term()) -> [string()].
validate_expr(#operation{} = Op, Worker) ->
    Meta = Op#operation.meta,
    ValidateList =
        fun(Exprs) when is_list(Exprs) ->
            validate_expr(Exprs, Worker);
           (X) ->
            [mzb_string:format(
                mzbl_script:meta_to_location_string(Meta) ++ "Expected list of expressions but got ~p.",
                [X])]
        end,
    AddLocation =
        fun(Messages) ->
            lists:map(
                fun(Msg) -> mzbl_script:meta_to_location_string(Meta) ++ Msg end,
                Messages)
        end,
    case Op of
        #operation{name = loop, args = [_Spec, Body]} -> ValidateList(Body);
        #operation{name = Fn, args = Args} ->
            Arity = length(Args),
            IsStdFun = mzbl_stdlib_signatures:is_std_function(Fn, Arity),
            {Provider, WorkerName} = Worker,
            IsWorkerFun = Provider:validate_function(WorkerName, Fn, Arity),
            case {IsStdFun, IsWorkerFun} of
                {true, _} -> ValidateList(Args);
                {_, ok} -> ValidateList(Args);
                {_, bad_arity} ->
                    AddLocation([mzb_string:format(
                                                 "Function with wrong arity ~p:~p/~p.",
                                                 [Worker, Fn, Arity])]);
                {_, _} ->
                    AddLocation([mzb_string:format(
                                                 "Unknown function ~p/~p.",
                                                 [Fn, Arity])])
            end
    end;
validate_expr(Ops, Worker) when is_list(Ops) ->
    lists:flatmap(
        fun(Op) ->
            validate_expr(Op, Worker)
        end,
        Ops);
validate_expr(_, _) -> [].