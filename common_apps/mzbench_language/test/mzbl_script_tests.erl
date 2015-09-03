-module(mzbl_script_tests).
-include_lib("eunit/include/eunit.hrl").
-include("mzbl_types.hrl").

empty_test() ->
    check([], "[{pool, [{size, 1}], [{do_stuff}]}].").

git_one_test() ->
    check([#git_install_spec{repo = "git@github.com:foo/bar", branch = "b", dir = "d"}],
        "[{make_install, ["
            "{git, \"git@github.com:foo/bar\"},"
            "{branch, \"b\"},"
            "{dir, \"d\"}]}].").

git_two_test() ->
    check([#git_install_spec{repo = "git@github.com:foo/bar", branch = "b", dir = "."},
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
    check([#rsync_install_spec{remote = "../somewhere/nearby/", dir = "", excludes = []}],
       "[{make_install, [{rsync, \"../somewhere/nearby/\"}]}].").

rsync_with_excludes_test() ->
    check([#rsync_install_spec{remote = "../somewhere/nearby/", dir = "node", excludes = ["deps", "ebin"]}],
       "[{make_install, [{rsync, \"../somewhere/nearby/\"}, {dir, <<\"node\">>}, {excludes, [\"deps\", \"ebin\"]}]}].").

check(ExpectedInstallSpecs, Script) ->
    AST = mzbl_script:read_from_string(Script, []),
    ?assertEqual(ExpectedInstallSpecs, mzbl_script:extract_install_specs(AST)).
