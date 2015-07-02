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
    check([#install_spec{repo = "git@github.com:foo/bar", branch = "b", dir = ""},
        #install_spec{repo = "https://github.com/baz/quux", branch = "", dir = "d"}],
       "[{make_install, [{git, \"git@github.com:foo/bar\"}, {branch, \"b\"}]},"
       "{make_install, [{git, \"https://github.com/baz/quux\"}, {dir, \"d\"}]}].").

check(ExpectedInstallSpecs, Script) ->
    AST = mzbl_script:read_from_string(Script, []),
    ?assertEqual(ExpectedInstallSpecs, mzbl_script:extract_install_specs(AST)).