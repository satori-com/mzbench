-module(mzbl_script_tests).
-include_lib("eunit/include/eunit.hrl").
-include("mzbl_types.hrl").

empty_test() ->
    install_specs_check([], "[{pool, [{size, 1}], [{do_stuff}]}].").

empty_bdl_test() ->
    install_specs_check([], "#!benchDL\n"
      "pool(size = 1):\n"
      " do_stuff()").

add_indents_test() ->
    ?assertEqual("A\n_INDENT_ B\n C\n_DEDENT_ ", mzbl_script:add_indents("A\n B\n C")).

add_indents_comments_test() ->
    ?assertEqual("A\n_INDENT_ B\n   # comment\n C\n_DEDENT_ ", mzbl_script:add_indents("A\n B\n   # comment\n C")).

add_indents_comments2_test() ->
    ?assertEqual("A\n_INDENT_ B\n   # comment(\n C\n_DEDENT_ ", mzbl_script:add_indents("A\n B\n   # comment(\n C")).

add_indents_string_test() ->
    ?assertEqual("A(\"(\\\"#\")\n_INDENT_ B\n C\n_DEDENT_ ", mzbl_script:add_indents("A(\"(\\\"#\")\n B\n C")).

add_indents_emptyline_test() ->
    ?assertEqual("A\n_INDENT_ B\n   \n C\n_DEDENT_ ", mzbl_script:add_indents("A\n B\n   \n C")).

add_indents_brackets_test() ->
    ?assertEqual("(A\n B\n C) A\n_INDENT_ B\n C\n_DEDENT_ ", mzbl_script:add_indents("(A\n B\n C) A\n B\n C")).

git_one_test() ->
    install_specs_check([#git_install_spec{repo = "git@github.com:foo/bar", branch = "b", dir = "d", build = ""}],
        "[{make_install, ["
            "{git, \"git@github.com:foo/bar\"},"
            "{branch, \"b\"},"
            "{dir, \"d\"}]}].").

git_one_bdl_test() ->
    install_specs_check([#git_install_spec{repo = "git@github.com:foo/bar", branch = "b", dir = "d", build = ""}],
        "#!benchDL\n"
        "make_install("
            "git = \"git@github.com:foo/bar\","
            "branch = \"b\","
            "dir = \"d\")").

git_one_bdl_comments_test() ->
    install_specs_check([#git_install_spec{repo = "git@github.com:foo/bar", branch = "b", dir = "d", build = ""}],
        "#!benchDL\n"
        "# one more comment\n"
        "make_install("
            "git = \"git@github.com:foo/bar\","
            "branch = \"b\","
            "dir = \"d\")").

git_two_test() ->
    install_specs_check([#git_install_spec{repo = "git@github.com:foo/bar", branch = "b", dir = ".", build = ""},
        #git_install_spec{repo = "https://github.com/baz/quux", branch = "", dir = "d", build = ""}],
       "[{make_install, [{git, \"git@github.com:foo/bar\"}, {branch, \"b\"}]},"
       "{make_install, [{git, \"https://github.com/baz/quux\"}, {dir, \"d\"}]}].").

git_two_bdl_test() ->
    install_specs_check([#git_install_spec{repo = "git@github.com:foo/bar", branch = "b", dir = ".", build = ""},
        #git_install_spec{repo = "https://github.com/baz/quux", branch = "", dir = "d", build = ""}],
       "#!benchDL\n"
       "make_install(git = \"git@github.com:foo/bar\", branch = \"b\")\n"
       "make_install(git = \"https://github.com/baz/quux\", dir = \"d\")").

wrong_script_test() ->
    ?assertError({parse_error, _}, mzbl_script:parse("Some text which is not valid")).

wrong_script2_test() ->
    ?assertError({parse_error, _}, mzbl_script:parse("pool{}.")).

wrong_script3_test() ->
    ?assertError({parse_error, _}, mzbl_script:parse("#!benchDL\n" "pool()\n .")).

missed_git_opts_test() ->
    AST = mzbl_script:read_from_string("[{make_install, [{branch, \"b\"}]}]."),
    ?assertError({install_spec_error, missed_mandatory_option, git}, mzbl_script:extract_install_specs(AST, [])).

missed_git_opts_bdl_test() ->
    AST = mzbl_script:read_from_string("#!benchDL\n" "make_install(branch = \"b\")"),
    ?assertError({install_spec_error, missed_mandatory_option, git}, mzbl_script:extract_install_specs(AST, [])).

import_json_test() ->
    import_json(mzbl_script:read_from_string("[{include_resource, test_json, \"./file.json\", json}].")).

import_json_bdl_test() ->
    import_json(mzbl_script:read_from_string("#!benchDL\n"
      "include_resource(test_json, \"./file.json\", json)")).

import_json(AST) ->
    {ok, MZBenchLanguageDir} = file:get_cwd(),
    ResourceDir = filename:join([MZBenchLanguageDir, "test", "resources"]),
    {_S, Env, _Asserts} = mzbl_script:extract_info(AST, [{"bench_script_dir", ResourceDir}]),
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

rsync_bdl_test() ->
    install_specs_check([#rsync_install_spec{remote = "../somewhere/nearby/", dir = "", excludes = []}],
       "#!benchDL\n"
       "make_install(rsync = \"../somewhere/nearby/\")").

rsync_with_excludes_test() ->
    install_specs_check([#rsync_install_spec{remote = "../somewhere/nearby/", dir = "node", excludes = ["deps", "ebin"]}],
       "[{make_install, [{rsync, \"../somewhere/nearby/\"}, {dir, <<\"node\">>}, {excludes, [\"deps\", \"ebin\"]}]}].").

rsync_with_excludes_bdl_test() ->
    install_specs_check([#rsync_install_spec{remote = "../somewhere/nearby/", dir = "node", excludes = ["deps", "ebin"]}],
       "#!benchDL\n"
       "make_install(rsync = \"../somewhere/nearby/\", dir = \"node\", excludes = [\"deps\", \"ebin\"])").

extract_while_metrics_test() ->
    AST = mzbl_script:read_from_string("#!benchDL\n"
          "pool(size = 1):\n"
          "    loop(rate = 1 rps, time = 1 min, while = \"asdf\" > 1):\n"
          "        do_stuff()"),
    ?assertEqual(["asdf"], mzbl_script:get_loop_assert_metrics(AST)).

install_specs_check(ExpectedInstallSpecs, Script) ->
    AST = mzbl_script:read_from_string(Script),
    ?assertEqual(ExpectedInstallSpecs, mzbl_script:extract_install_specs(AST, [])).

