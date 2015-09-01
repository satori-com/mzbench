-module(mzb_worker_script_validator).

-export([validate_worker_script/2, validate_time/1]).

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
        #operation{name = undefined} ->
            AddLocation(["Empty instruction."]);
        #operation{name = parallel, args = Body} -> ValidateList(Body);
        #operation{name = loop, args = [Spec, Body]} ->
            validate_loopspec(Spec, mzbl_script:meta_to_location_string(Meta)) ++
            ValidateList(Body);
        #operation{name = t, args = Args} ->
            validate_expr(Args, Worker);
        #operation{name = loop} ->
            AddLocation(["Loop must have a spec and a body."]);
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

-spec validate_loopspec([#operation{}], string()) -> [string()].
validate_loopspec(LoopSpec, LoopLocation) ->
    case lists:keyfind(time, #operation.name, LoopSpec) of
        false -> [LoopLocation ++ "Loop spec must specify duration."];
        _ -> []
    end
    ++
    lists:flatmap(
        fun(#operation{name = Name, args = Args, meta = Meta}) ->
            lists:map(
                fun(Message) ->
                        mzbl_script:meta_to_location_string(Meta) ++ Message
                end,
                case {Name, Args} of
                    {time, [Value]} -> validate_time(Value);
                    {rate, [Value]} -> validate_loop_rate(Value);
                    {iterator, [Value]} -> validate_string_literal(Value);
                    {parallel, [N]} -> validate_pos_int(N);
                    {spawn, [Val]} -> validate_bool(Val);
                    {Name, Args} ->
                        [mzb_string:format(
                            "Unexpected loop option {~p, ~p}.", [Name, Args])]
                end);
            (_) -> ["Unexpected loop option."]
        end,
        LoopSpec).

-spec validate_time(any()) -> [string()].
validate_time(#constant{value = N, units = ms}) when is_number(N) -> [];
validate_time(#constant{value = #operation{}}) -> [];
validate_time(_) -> ["Unexpected time."].

-spec validate_rate(any()) -> [string()].
validate_rate(#constant{value = N, units = rps}) when is_number(N) -> [];
validate_rate(#constant{value = #operation{}}) -> [];
validate_rate(_) -> ["Unexpected rate."].

-spec validate_loop_rate(any()) -> [string()].
validate_loop_rate(#constant{} = C) -> validate_rate(C);
validate_loop_rate(#ramp{
    curve_type = linear,
    from = F,
    to = T}) -> validate_rate(F) ++ validate_rate(T);
validate_loop_rate(_) -> ["Unexpected rate."].

-spec validate_string_literal(any()) -> [string()].
validate_string_literal(Name) when is_list(Name) -> [];
validate_string_literal(_) -> ["Expression must be a string literal."].

-spec validate_bool(any()) -> [string()].
validate_bool(true) -> [];
validate_bool(false) -> [];
validate_bool(V) -> [io_lib:format("Unexpected boolean: ~p", [V])].

-spec validate_pos_int(any()) -> [string()].
validate_pos_int(N) when is_integer(N), N > 0 -> [];
validate_pos_int(N) ->
    [io_lib:format("Unexpected positive integer: ~p", [N])].

