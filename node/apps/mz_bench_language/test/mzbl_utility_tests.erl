-module(mzbl_utility_tests).
-include_lib("eunit/include/eunit.hrl").

choose_test() ->
    ?assertEqual(1, mzbl_utility:choose([1, 1, 1, 1, 1])),
    ?assertEqual(78, mzbl_utility:choose([78, 78, 78, 78, 78])),
    ?assertEqual([], mzbl_utility:choose(2, [])),
    ?assertEqual([], mzbl_utility:choose(0, [4, 4])),
    ?assertEqual([4, 4], mzbl_utility:choose(20, [4, 4])),
    ?assertEqual([1, 1], mzbl_utility:choose(2, [1, 1, 1, 1, 1])),
    ?assertEqual([78, 78, 78], mzbl_utility:choose(3, [78, 78, 78, 78, 78])).

to_integer_with_default_test() ->
    ?assertEqual(3, mzbl_utility:to_integer_with_default(3, 5)),
    ?assertEqual(3, mzbl_utility:to_integer_with_default("3", 5)),
    ?assertEqual(1, mzbl_utility:to_integer_with_default(<<"1">>, fail)),
    ?assertEqual(5, mzbl_utility:to_integer_with_default(three, 5)).
