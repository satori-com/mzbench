-module(mzb_api_auth_tests).
-include_lib("eunit/include/eunit.hrl").

match_wildcard_test() ->
    application:set_env(mzbench_api, admin_list, ["adm*.com", "nad*@test.com"]),
    application:set_env(mzbench_api, white_list, ["white*@test.com"]),
    application:set_env(mzbench_api, black_list, ["black*@test.com"]),
    ?assert(mzb_api_auth:check_admin_listed("admin@mysite.com")),
    ?assertNot(mzb_api_auth:check_admin_listed("a2dmin@mysite.com")),
    ?assert(mzb_api_auth:check_admin_listed("nadmin@test.com")),
    ?assertNot(mzb_api_auth:check_admin_listed("n2admin@test.com")),
    ?assertEqual(ok, mzb_api_auth:check_black_white_listed("white@test.com")),
    ?assertEqual(ok, mzb_api_auth:check_black_white_listed("white11@test.com")),
    ?assertEqual(error, mzb_api_auth:check_black_white_listed("black@test.com")),
    ?assertEqual(error, mzb_api_auth:check_black_white_listed("black11@test.com")).
