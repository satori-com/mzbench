-module(mzb_lists_tests).
-include_lib("eunit/include/eunit.hrl").

pmap_test() ->
    ?assertEqual(
        lists:map(fun length/1, ["foo", "quux"]),
        mzb_lists:pmap(fun length/1, ["foo", "quux"])).
