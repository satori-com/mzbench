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
    check([#git_install_spec{repo = "git@github.com:foo/bar", branch = "b", dir = ""},
        #git_install_spec{repo = "https://github.com/baz/quux", branch = "", dir = "d"}],
       "[{make_install, [{git, \"git@github.com:foo/bar\"}, {branch, \"b\"}]},"
       "{make_install, [{git, \"https://github.com/baz/quux\"}, {dir, \"d\"}]}].").

rsync_test() ->
    check([#rsync_install_spec{remote = "../somewhere/nearby/", excludes = []}],
       "[{make_install, [{rsync, \"../somewhere/nearby/\"}]}].").

rsync_with_excludes_test() ->
    check([#rsync_install_spec{remote = "../somewhere/nearby/", excludes = ["deps", "ebin"]}],
       "[{make_install, [{rsync, \"../somewhere/nearby/\"}, {excludes, [\"deps\", \"ebin\"]}]}].").

check(ExpectedInstallSpecs, Script) ->
    AST = mzbl_script:read_from_string(Script, []),
    ?assertEqual(ExpectedInstallSpecs, mzbl_script:extract_install_specs(AST)).