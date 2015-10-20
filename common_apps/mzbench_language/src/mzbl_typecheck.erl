-module(mzbl_typecheck).

-export([check/2, check/3]).

-include("mzbl_types.hrl").

-spec check(abstract_expr(), type()) -> typecheck_result().
check(Term, Type) -> check(Term, Type, []).

-spec check(abstract_expr(), type(), worker_env()) -> typecheck_result().
check(#operation{name = Name, args = Args, meta = Meta}, T, Env) ->
    add_location(Meta, check_op(Name, Args, T, Env));
check(#constant{value = V, units = U, meta = Meta}, time, Env) ->
    add_location(Meta, and_(check(V, number, Env), check(U, timeunit, Env)));
check(#constant{value = V, units = U, meta = Meta}, rate, Env) ->
    add_location(Meta, and_(check(V, number, Env), check(U, rateunit, Env)));
check(X, number, Env) -> or_(check(X, integer, Env), check(X, float, Env));
check(X, integer, _) ->
    case mzb_utility:to_integer_with_default(X, false) of
        false -> {false, {X, is_not, integer}, undefined};
        _ -> true
    end;
check(X, float, _) ->
    case is_float(X) of
        true -> true;
        _ -> {false, {X, is_not, float}, undefined}
    end;
check(X, atom, _) when is_atom(X) -> true;
check(X, string, _) ->
    case io_lib:printable_unicode_list(X) of
        true -> true;
        _ -> {false, {X, is_not, string}, undefined}
    end;
check(X, binary, _) when is_binary(X) -> true;
check(X, timeunit, _) ->
    case lists:member(X, [min, sec, ms, us]) of
        true -> true;
        _ -> {false, {X, is_not, timeunit}, undefined}
    end;
check(X, rateunit, _) ->
    case lists:member(X, [rph, rpm, rps]) of
        true -> true;
        _ -> {false, {X, is_not, rateunit}, undefined}
    end;
check(X, list, Env) ->
    case is_list(X) of
        true -> all_([check(E, any, Env) || E <- X]);
        _ -> {false, {X, is_not, list}, undefined}
    end;
check(X, tuple, _) ->
    case is_tuple(X) of
        true -> true;
        _ -> {false, {X, is_not, tuple}, undefined}
    end;
check(X, boolean, _) when is_boolean(X) -> true;
check(L, any, Env) when is_list(L) ->
    all_([check(X, any, Env) || X <- L]);
check(_, any, _) -> true;
check(X, T, _) -> {false, {X, is_not, T}, undefined}.

-spec check_env(string(), type(), worker_env()) -> typecheck_result().
check_env(Name, Type, Env) ->
    case proplists:is_defined(Name, Env) of
        false -> not_sure;
        true -> check(proplists:get_value(Name, Env), Type, Env)
    end.

-spec check_op(atom(), [term()], type(), worker_env()) -> typecheck_result().
check_op(undefined, _, _, _) ->
    {false, "Empty operation", undefined};
check_op(loop, [Spec, Body], T, Env) ->
    all_([
        is(nil, T),
        all_([check(X, any, Env) || X <- Body]),
        all_([check_loop_spec_element(X, Env) || X <- Spec]),
        case lists:keyfind(time, #operation.name, Spec) of
            false -> {false, "Loop spec must specify duration.", undefined};
            _ -> true
        end]);
check_op(loop, _, _, _) ->
    {false, "Loop must have a spec and a body.", undefined};
check_op(var, [Name], T, Env) ->
    and_(check(Name, string, Env), check_env(Name, T, Env));
check_op(var, [Name, Default], T, Env) ->
    all_([
        check(Name, string, Env),
        check(Default, T, Env),
        check_env(Name, T, Env)]);
check_op(numvar, [Name], T, Env) ->
    all_([
        check(Name, string, Env),
        is(number, T),
        check_env(Name, T, Env)]);
check_op(numvar, [Name, Default], T, Env) ->
    all_([
        check(Name, string, Env),
        or_(is(integer, T), is(float, T)),
        check(Default, T, Env),
        check_env(Name, T, Env)]);
check_op(size, [Size], _, Env) ->
    check(Size, integer, Env);
check_op(random_binary, [Size], T, Env) ->
    and_(is(binary, T), check(Size, integer, Env));
check_op(random_list, [Size], T, Env) ->
    and_(is(list, T), check(Size, integer, Env));
check_op(t, [List], T, Env) ->
    and_(is(tuple, T), check(List, list, Env));
check_op(random_number, [N, M], T, Env) ->
    and_(
        is(integer, T),
        and_(
            check(N, integer, Env),
            check(M, integer, Env)));
check_op(choose, [N, Xs], T, Env) ->
    case is_list(Xs) of
        true ->
            and_(
                check(N, integer, Env),
                all_([check(X, T, Env) || X <- Xs]));
        false -> check(Xs, list, Env)
    end;
check_op(choose, [Xs], T, Env) ->
    case is_list(Xs) of
        true ->
            all_([check(X, T, Env) || X <- Xs]);
        false -> check(Xs, list, Env)
    end;
check_op(error, [Msg], _, Env) ->
    check(Msg, any, Env);
check_op(wait, [Time], T, Env) ->
    and_(
        is(nil, T),
        check(Time, time, Env));
check_op(term_to_binary, [X], T, Env) ->
    and_(is(binary, T), check(X, any, Env));
check_op(wait_signal, [Name], T, Env) ->
    and_(
        is(nil, T),
        check(Name, any, Env));
check_op(wait_signal, [Name, Count], T, Env) ->
    all_([
        is(nil, T),
        check(Count, integer, Env),
        check(Name, any, Env)]);
check_op(wait_signal, [Name, Count, Timeout], T, Env) ->
    all_([
        is(nil, T),
        check(Name, any, Env),
        check(Count, integer, Env),
        check(Timeout, time, Env)]);
check_op(sprintf, [Fmt, Args], T, Env) ->
    case is_list(Args) of
        true ->
            % typechecking Args for literal Fmt strings
            % is left as an exercise
            and_(
                is(string, T),
                and_(
                    check(Fmt, string, Env),
                    all_([check(A, any, Env) || A <- Args])));
        false -> check(Args, list, Env)
    end;
check_op(parallel, [Exprs], T, Env) ->
    case Exprs of
        [E | Es] ->
            and_(check(E, T, Env), all_([check(X, any, Env) || X <- Es]));
        _ -> check(Exprs, list, Env)
    end;
check_op(ignore_failure, [Expr], T, Env) ->
    check(Expr, T, Env);
check_op(seq, [From, To], T, Env) ->
    all_([
        is(list, T),
        check(From, integer, Env),
        check(To, integer, Env)]);
check_op(resource, [Name], _, Env) ->
    check(Name, atom, Env);
check_op(round_robin, [Exprs], T, Env) ->
    case is_list(Exprs) of
        true -> all_([check(E, T, Env) || E <- Exprs]);
        false -> check(Exprs, list, Env)
    end;
check_op(set_signal, [Name], T, Env) ->
    and_(is(nil, T), check(Name, any, Env));
check_op(set_signal, [Name, Count], T, Env) ->
    all_([
        is(nil, T),
        check(Name, any, Env),
        check(Count, integer, Env)]);
check_op(dump, [X], T, Env) ->
    and_(is(atom, T), check(X, any, Env));
check_op(think_time, [Rate, Time], T, Env) ->
    all_([
        is(rate, T),
        check(Rate, rate, Env),
        check(Time, time, Env)]);
check_op(ramp, [Profile, Rate1, Rate2], T, Env) ->
    all_([
        case Profile of
            linear -> true;
            _ -> {false, mzb_string:format("~p is not a valid ramp profile", [Profile]), undefined}
        end,
        is(rate, T),
        check(Rate1, rate, Env),
        check(Rate2, rate, Env)]);
check_op(comb, Args, T, Env) ->
    RatesAndTimes = every_other(lists:zip(lists:droplast(Args), tl(Args))),
    and_(
        is(rate, T),
        all_([and_(check(Rate, rate, Env), check(Time, time, Env))
            || {Rate, Time} <- RatesAndTimes]));
check_op(_, Args, _, Env) ->
    % this is probably worker's operation
    % we don't know anything about it
    % but we can still look into its arguments
    and_(not_sure, all_([check(A, any, Env) || A <- Args])).

check_loop_spec_element(#operation{name = time, args = [Time]}, Env) ->
    check(Time, time, Env);
check_loop_spec_element(#operation{name = rate, args = [Rate]}, Env) ->
    check(Rate, rate, Env);
check_loop_spec_element(#operation{name = spawn, args = [Arg]}, Env) ->
    check(Arg, boolean, Env);
check_loop_spec_element(#operation{name = parallel, args = [N]}, Env) ->
    check(N, integer, Env);
check_loop_spec_element(#operation{name = iterator, args = [Name]}, Env) ->
    check(Name, string, Env);
check_loop_spec_element(X, _Env) ->
    {false, mzb_string:format("Bad loop spec element ~p", [X]), undefined}.

-spec or_(typecheck_result(), typecheck_result()) -> typecheck_result().
or_(true, _) -> true;
or_(_, true) -> true;
or_({false, Reason1, Location1}, {false, Reason2, _}) -> {false, {neither, Reason1, Reason2}, Location1};
or_(_, _) -> not_sure.

-spec and_(typecheck_result(), typecheck_result()) -> typecheck_result().
and_({false, Reason, Location}, _) -> {false, Reason, Location};
and_(_, {false, Reason, Location}) -> {false, Reason, Location};
and_(true, true) -> true;
and_(_, _) -> not_sure.

-spec all_([typecheck_result()]) -> typecheck_result().
all_(Xs) ->
    lists:foldl(fun(X, Acc) -> and_(X, Acc) end, true, Xs).

-spec every_other(list()) -> list().
every_other([]) -> [];
every_other([X]) -> [X];
every_other([X, _ | Rest]) -> [X | every_other(Rest)].

-spec is(type(), type()) -> typecheck_result().
is(X, X) -> true;
is(_, any) -> true;
is(nil, T) -> {false, {nil, is_not, T}, undefined};
is(integer, number) -> true;
is(list, string) -> not_sure;
is(string, list) -> true;
is(X, T) -> {false, {X, is_not, T}, undefined}.

-spec add_location(meta(), typecheck_result()) -> typecheck_result().
add_location(Meta, {false, Reason, undefined}) ->
    {false, Reason, mzbl_script:meta_to_location_string(Meta)};
add_location(_, X) -> X.
