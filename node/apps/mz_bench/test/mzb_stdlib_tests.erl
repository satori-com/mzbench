-module(mzb_stdlib_tests).
-include_lib("eunit/include/eunit.hrl").
-include("mzb_types.hrl").
-include("mzb_ast.hrl").

-define(
  VAR(Fun, Expected, Env, Input),
  ?assertEqual(
      {Expected, state},
      mzb_stdlib:Fun(state, Env, meta, Input))).
-define(
  VAR(Fun, Expected, Env, Input, Default),
  ?assertEqual(
      {Expected, state},
      mzb_stdlib:Fun(state, Env, meta, Input, Default))).

var_test() ->
  ?VAR(var, 42, [{"foo", 42}], "foo"),
  ?VAR(var, "42", [{"foo", "42"}], "foo"),
  ?VAR(numvar, 42, [{"foo", 42}], "foo"),
  ?VAR(numvar, 42, [{"foo", "42"}], "foo"),
  ?VAR(numvar, 42.5, [{"foo", "42.5"}], "foo").