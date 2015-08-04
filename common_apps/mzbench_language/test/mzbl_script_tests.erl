-module(mzbl_script_tests).
-include_lib("eunit/include/eunit.hrl").
-include("mzbl_types.hrl").

empty_test() ->
    check([], "[{pool, [{size, 1}], [{do_stuff}]}].").

one_test() ->
    check([#install_spec{repo = "git@github.com:foo/bar", branch = "b", dir = "d"}],
        "[{make_install, ["
            "{git, \"git@github.com:foo/bar\"},"
            "{branch, \"b\"},"
            "{dir, \"d\"}]}].").

two_test() ->
    check([#install_spec{repo = "git@github.com:foo/bar", branch = "b", dir = "."},
        #install_spec{repo = "https://github.com/baz/quux", branch = "", dir = "d"}],
       "[{make_install, [{git, \"git@github.com:foo/bar\"}, {branch, \"b\"}]},"
       "{make_install, [{git, \"https://github.com/baz/quux\"}, {dir, \"d\"}]}].").

missed_git_opts_test() ->
    AST = mzbl_script:read_from_string("[{make_install, [{branch, \"b\"}]}].", []),
    ?assertError({install_spec_error, missed_mandatory_option, git}, mzbl_script:extract_install_specs(AST)).

import_json_test() ->
    AST = mzbl_script:read_from_string("[{include_resource, test_json, \"./file.json\", json}].",[]),
    {ok, EUnitDir} = file:get_cwd(),
    ResourceDir = filename:join([EUnitDir, "..", "test", "resources"]),
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

check(ExpectedInstallSpecs, Script) ->
    AST = mzbl_script:read_from_string(Script, []),
    ?assertEqual(ExpectedInstallSpecs, mzbl_script:extract_install_specs(AST)).
