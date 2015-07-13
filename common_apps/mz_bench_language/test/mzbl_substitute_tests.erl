-module(mzbl_substitute_tests).
-include_lib("eunit/include/eunit.hrl").
-include("mzbl_types.hrl").

id_test() ->
  Simple = "[{pool, [{size, 1}], []}].",
  check(Simple, [], Simple).

var_string_test() ->
  check(
      "{print, {var, \"foo\"}}.",
      [{"foo", "bar"}],
      "{print, \"bar\"}.").

var_int_test() ->
  check(
      "{print, {var, \"foo\"}}.",
      [{"foo", 42}],
      "{print, 42}.").

rate_test() ->
  check(
      "{{var, \"foo\"}, rps}.",
      [{"foo", 42}],
      "{42, rps}.").

ramp_test() ->
  check(
      "{ramp, linear, {{var, \"foo\"}, rps}, {72, rps}}.",
      [{"foo", 42}],
      "{ramp, linear, {42, rps}, {72, rps}}.").

var_string_to_int_default_test() ->
  check(
      "{print, {var, \"foo\", 1}}.",
      [{"foo", "42"}],
      "{print, 42}.").

var_int_to_atom_default_test() ->
  check(
      "{print, {var, \"foo\", undefined}}.",
      [{"foo", 1}],
      "{print, 1}.").

var_float_to_atom_default_test() ->
  check(
      "{print, {var, \"foo\", undefined}}.",
      [{"foo", 3.14}],
      "{print, 3.14}.").

var_string_to_atom_default_test() ->
  check(
      "{print, {var, \"foo\", undefined}}.",
      [{"foo", "bar"}],
      "{print, \"bar\"}.").

missing_var_is_a_failure_test() ->
  ?assertError(
      % {substitution_error, "foo", not_found_in_env, _},
      _,
      mzbl_script:substitute(
          mzbl_script:parse("{print, {var, \"foo\"}}."),
          [])
      ).

unused_iterator_test() ->
  check(
      "{loop, [{iterator, \"i\"}], []}.",
      [],
      "{loop, [{iterator, \"i\"}], []}.").

using_iterator_as_var_test() ->
  check(
      "{loop, [{iterator, \"i\"}], {var, \"i\"}}.",
      [],
      "{loop, [{iterator, \"i\"}], {var, \"i\"}}.").

using_iterator_as_numvar_test() ->
  check(
      "{loop, [{iterator, \"i\"}], {numvar, \"i\"}}.",
      [],
      "{loop, [{iterator, \"i\"}], {numvar, \"i\"}}.").

using_var_in_poolspec_test() ->
  check(
    "[ { pool
      , [ {per_node, {numvar, \"size_per_node\", 1}}
        , {size, {numvar, \"size\", 1}}
        , {worker_type, counter_and_histogram_inc_worker}
        ]
      , []
      }
    ].",
    [{"nodes_num",1},{"bench_hosts",["box-type0"]},{"bench_script_dir","/root"},{"bench_workers_dir","~/mz/mz_bench_workers"},{"exclusive_node_usage","true"},{"size","40"},{"size_per_node","40"}],
    "[ { pool
      , [ {per_node, 40}
        , {size, 40}
        , {worker_type, counter_and_histogram_inc_worker}
        ]
      , []
      }
    ].").

check(Input, Env, Output) ->
  ?assertEqual(
      mzbl_script:parse(Output),
      catch mzbl_script:substitute(
          mzbl_script:parse(Input),
          Env)).
