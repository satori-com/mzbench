-module(mzb_asserts).

-export([
    validate/1,
    init/1,
    update_state/2,
    get_failed/2
]).

-include("mzb_types.hrl").
-include("mzb_ast.hrl").

validate(#operation{name = assert, args = [always, Expression], meta = M}) ->
    validate_assert_expr(Expression, M);
validate(#operation{name = assert, args = [Time, Expression], meta = M}) ->
    mzb_worker_script_validator:validate_time(Time) ++
    validate_assert_expr(Expression, M).

validate_assert_expr(#operation{name = Name, args = [Op1, Op2]}, M) when is_list(Op1), is_number(Op2) ->
    validate_assert_op(Name, M);
validate_assert_expr(#operation{name = Name, args = [Op1, Op2]}, M) when is_list(Op2), is_number(Op1) ->
    validate_assert_op(Name, M);
validate_assert_expr(_Invalid, M) ->
    [lists:flatten(io_lib:format("~sInvalid assert expression", [mzb_script:meta_to_location_string(M)]))].

validate_assert_op(gt, _) -> [];
validate_assert_op(lt, _) -> [];
validate_assert_op(gte, _) -> [];
validate_assert_op(lte, _) -> [];
validate_assert_op(Name, M) ->
    lists:flatten(io_lib:format("~sInvalid assert operation: ~p",
        [mzb_script:meta_to_location_string(M), Name])).

get_failed(BenchTime, State) ->
    Failed = lists:filter(
        fun ({{always, _}, Time}) when Time < BenchTime -> true;
            ({{#constant{value = AssertTime, units = ms}, _}, Time})
                when Time < 1000 * AssertTime -> true;
            ({{_, _}, _}) -> false
    end, State),

    [{Expr, format(A, BenchTime)} || {_, Expr} = A <- Failed].

init(Asserts) ->
    [{A, 0} || A <- Asserts].

update_state(TimeSinceCheck, State) ->
    lists:map(
        fun ({{_, Expr} = Assert, Val}) ->
            case check(Expr) of
                true  -> {Assert, Val + TimeSinceCheck};
                false -> {Assert, Val}
            end
        end, State).

check(#operation{name = Op, args = [Metric, Value1]}) ->
    case mzb_metrics:get_metric_value(Metric) of
        {ok, Value2} -> check_value(Op, Value2, Value1);
        {error, not_found} -> false
    end.

check_value(gt, V1, V2) -> V1 > V2;
check_value(gte, V1, V2) -> V1 >= V2;
check_value(lt, V1, V2) -> V1 < V2;
check_value(lte, V1, V2) -> V1 =< V2.

format({{always, Expr}, Time}, BenchTime) ->
    io_lib:format("Assertion: ~s~nwas expected to hold for ~s (whole bench time)~nbut held for just ~s", [format_expr(Expr), format_time(BenchTime), format_time(Time)]);
format({{#constant{value = Value, units = ms}, Expr}, Time}, _) ->
    io_lib:format("Assertion: ~s~nwas expected to hold for ~s~nbut held for just ~s", [format_expr(Expr), format_time(Value * 1000), format_time(Time)]).


format_expr(#operation{name = Operation, args = [Op1, Op2]}) when is_list(Op1), is_number(Op2) ->
    io_lib:format("~s ~s ~p", [Op1, format_op(Operation), Op2]);
format_expr(#operation{name = Operation, args = [Op1, Op2]}) when is_number(Op1), is_list(Op2) ->
    io_lib:format("~s ~s ~p", [Op1, format_op(Operation), Op2]).

format_op(gt) -> ">";
format_op(lt) -> "<";
format_op(gte) -> ">=";
format_op(lte) -> "<=".

format_time(Time) when Time < 1000000 -> io_lib:format("~bus", [Time]);
format_time(Time) ->
    case {calendar:seconds_to_time(Time div 1000000), Time rem 1000000} of
        {{0, 0, S}, 0} -> io_lib:format("~bs", [S]);
        {{0, 0, S}, Mk} -> io_lib:format("~bs ~s", [S, format_time(Mk)]);
        {{0, M, S}, 0} -> io_lib:format("~bm ~bs", [M, S]);
        {{0, M, S}, Mk} -> io_lib:format("~bm ~bs ~s", [M, S, format_time(Mk)]);
        {{H, M, S}, _} -> io_lib:format("~bh ~bm ~bs", [H, M, S])
    end.
