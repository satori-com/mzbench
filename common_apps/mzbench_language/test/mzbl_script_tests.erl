-module(mzbl_script_tests).
-include_lib("eunit/include/eunit.hrl").
-include("mzbl_types.hrl").

empty_test() ->
    install_specs_check([], "[{pool, [{size, 1}], [{do_stuff}]}].").

git_one_test() ->
    install_specs_check([#git_install_spec{repo = "git@github.com:foo/bar", branch = "b", dir = "d"}],
        "[{make_install, ["
            "{git, \"git@github.com:foo/bar\"},"
            "{branch, \"b\"},"
            "{dir, \"d\"}]}].").

git_two_test() ->
    install_specs_check([#git_install_spec{repo = "git@github.com:foo/bar", branch = "b", dir = "."},
        #git_install_spec{repo = "https://github.com/baz/quux", branch = "", dir = "d"}],
       "[{make_install, [{git, \"git@github.com:foo/bar\"}, {branch, \"b\"}]},"
       "{make_install, [{git, \"https://github.com/baz/quux\"}, {dir, \"d\"}]}].").

missed_git_opts_test() ->
    AST = mzbl_script:read_from_string("[{make_install, [{branch, \"b\"}]}].", []),
    ?assertError({install_spec_error, missed_mandatory_option, git}, mzbl_script:extract_install_specs(AST)).

import_json_test() ->
    AST = mzbl_script:read_from_string("[{include_resource, test_json, \"./file.json\", json}].",[]),
    {ok, MZBenchLanguageDir} = file:get_cwd(),
    ResourceDir = filename:join([MZBenchLanguageDir, "test", "resources"]),
    {_S, Env} = mzbl_script:extract_pools_and_env(AST, [{"bench_script_dir", ResourceDir}]),
    Json = proplists:get_value({resource,test_json}, Env),
    ?assertEqual(#{<<"widget">> => #{
                       <<"debug">> => true,
                       <<"image">> => #{
                           <<"alignment">> => <<"center">>,
                           <<"hOffset">> => 250,
                           <<"name">> => <<"sun1">>,
                           <<"src">> => <<"Images/Sun.png">>,
                           <<"vOffset">> => 250
                       }
                  }}, Json).

rsync_test() ->
    install_specs_check([#rsync_install_spec{remote = "../somewhere/nearby/", dir = "", excludes = []}],
       "[{make_install, [{rsync, \"../somewhere/nearby/\"}]}].").

rsync_with_excludes_test() ->
    install_specs_check([#rsync_install_spec{remote = "../somewhere/nearby/", dir = "node", excludes = ["deps", "ebin"]}],
       "[{make_install, [{rsync, \"../somewhere/nearby/\"}, {dir, <<\"node\">>}, {excludes, [\"deps\", \"ebin\"]}]}].").

install_specs_check(ExpectedInstallSpecs, Script) ->
    AST = mzbl_script:read_from_string(Script, []),
    ?assertEqual(ExpectedInstallSpecs, mzbl_script:extract_install_specs(AST)).

substitute_id_test() ->
  Simple = "[{pool, [{size, 1}], []}].",
  substitute_check(Simple, [], Simple).

substitute_var_string_test() ->
  substitute_check(
      "{print, {var, \"foo\"}}.",
      [{"foo", "bar"}],
      "{print, \"bar\"}.").

substitute_var_int_test() ->
  substitute_check(
      "{print, {var, \"foo\"}}.",
      [{"foo", 42}],
      "{print, 42}.").

substitute_rate_test() ->
  substitute_check(
      "{{var, \"foo\"}, rps}.",
      [{"foo", 42}],
      "{42, rps}.").

substitute_ramp_test() ->
  substitute_check(
      "{ramp, linear, {{var, \"foo\"}, rps}, {72, rps}}.",
      [{"foo", 42}],
      "{ramp, linear, {42, rps}, {72, rps}}.").

substitute_var_string_to_int_default_test() ->
  substitute_check(
      "{print, {var, \"foo\", 1}}.",
      [{"foo", "42"}],
      "{print, 42}.").

substitute_var_int_to_atom_default_test() ->
  substitute_check(
      "{print, {var, \"foo\", undefined}}.",
      [{"foo", 1}],
      "{print, 1}.").

substitute_var_float_to_atom_default_test() ->
  substitute_check(
      "{print, {var, \"foo\", undefined}}.",
      [{"foo", 3.14}],
      "{print, 3.14}.").

substitute_var_string_to_atom_default_test() ->
  substitute_check(
      "{print, {var, \"foo\", undefined}}.",
      [{"foo", "bar"}],
      "{print, \"bar\"}.").

substitute_missing_var_is_a_failure_test() ->
  ?assertError(
      % {substitution_error, "foo", not_found_in_env, _},
      _,
      mzbl_script:substitute(
          mzbl_script:parse("{print, {var, \"foo\"}}."),
          [])
      ).

substitute_unused_iterator_test() ->
  substitute_check(
      "{loop, [{iterator, \"i\"}], []}.",
      [],
      "{loop, [{iterator, \"i\"}], []}.").

substitute_using_iterator_as_var_test() ->
  substitute_check(
      "{loop, [{iterator, \"i\"}], {var, \"i\"}}.",
      [],
      "{loop, [{iterator, \"i\"}], {var, \"i\"}}.").

substitute_using_iterator_as_numvar_test() ->
  substitute_check(
      "{loop, [{iterator, \"i\"}], {numvar, \"i\"}}.",
      [],
      "{loop, [{iterator, \"i\"}], {numvar, \"i\"}}.").

substitute_using_var_in_poolspec_test() ->
  substitute_check(
    "[ { pool
      , [ {per_node, {numvar, \"size_per_node\", 1}}
        , {size, {numvar, \"size\", 1}}
        , {worker_type, counter_and_histogram_inc_worker}
        ]
      , []
      }
    ].",
    [{"nodes_num",1},{"bench_hosts",["box-type0"]},{"bench_script_dir","/root"},{"bench_workers_dir","~/mz/mzbench_workers"},{"exclusive_node_usage","true"},{"size","40"},{"size_per_node","40"}],
    "[ { pool
      , [ {per_node, 40}
        , {size, 40}
        , {worker_type, counter_and_histogram_inc_worker}
        ]
      , []
      }
    ].").

substitute_check(Input, Env, Output) ->
  ?assertEqual(
      mzbl_script:parse(Output),
      catch mzbl_script:substitute(
          mzbl_script:parse(Input),
          Env)).