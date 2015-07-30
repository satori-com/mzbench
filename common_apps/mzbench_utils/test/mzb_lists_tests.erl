-module(mzb_lists_tests).
-include_lib("eunit/include/eunit.hrl").

unique_test() ->
    ?assertEqual([], lists:sort(mzb_lists:unique([]))),
    ?assertEqual([1], lists:sort(mzb_lists:unique([1]))),
    ?assertEqual([1, "1"], lists:sort(mzb_lists:unique([1, "1"]))),
    ?assertEqual([1, 2], lists:sort(mzb_lists:unique([1, 2]))),
    ?assertEqual([1, 2], lists:sort(mzb_lists:unique([1, 1, 2]))),
    ?assertEqual([1, 2], lists:sort(mzb_lists:unique([1, 2, 1]))).