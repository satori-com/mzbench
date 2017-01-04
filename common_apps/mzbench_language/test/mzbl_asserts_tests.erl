-module(mzbl_asserts_tests).
-include_lib("eunit/include/eunit.hrl").
-include("mzbl_types.hrl").

-define(OP(A, B, C), #operation{name = A, args = [B, C]}).
-define(NOP(A, B, C), #operation{name = 'not', args = [#operation{name = A, args = [B, C]}]}).

essential_test() ->
  ?assertEqual(mzbl_asserts:check_expr(?OP('gt', 1, 2)), false),
  ?assertEqual(mzbl_asserts:check_expr(?OP('gte', 2, 2)), true),
  ?assertEqual(mzbl_asserts:check_expr(?OP('lt', 1, 2)), true),
  ?assertEqual(mzbl_asserts:check_expr(?OP('lte', 2, 2)), true),
  ?assertEqual(mzbl_asserts:check_expr(?OP('gt', 1.1, 2)), false),
  ?assertEqual(mzbl_asserts:check_expr(?OP('gte', 2.1, 2)), true),
  ?assertEqual(mzbl_asserts:check_expr(?OP('lt', 1.1, 2)), true),
  ?assertEqual(mzbl_asserts:check_expr(?OP('lte', 2, 2.1)), true),
  ?assertEqual(mzbl_asserts:check_expr(?OP('gte', 1.1, 2)), false),
  ?assertEqual(mzbl_asserts:check_expr(?NOP('gt', 1, 2)), true),
  ?assertEqual(mzbl_asserts:check_expr(?NOP('gte', 2, 2)), false),
  ?assertEqual(mzbl_asserts:check_expr(?NOP('lt', 1, 2)), false),
  ?assertEqual(mzbl_asserts:check_expr(?NOP('lte', 2, 2)), false).

logic_test() ->
  False = ?OP('gt', 1, 2),
  True = ?OP('gt', 2, 1),
  NFalse = ?NOP('gt', 1, 2),
  NTrue = ?NOP('gt', 2, 1),
  ?assertEqual(mzbl_asserts:check_expr(?OP('and', False, False)), false),
  ?assertEqual(mzbl_asserts:check_expr(?OP('and', True, True)), true),
  ?assertEqual(mzbl_asserts:check_expr(?OP('and', False, True)), false),
  ?assertEqual(mzbl_asserts:check_expr(?OP('or', False, True)), true),
  ?assertEqual(mzbl_asserts:check_expr(?OP('or', True, False)), true),
  ?assertEqual(mzbl_asserts:check_expr(?OP('or', NTrue, False)), false),
  ?assertEqual(mzbl_asserts:check_expr(?OP('or', NTrue, NFalse)), true).
