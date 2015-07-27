-module(mzb_string_tests).
-include_lib("eunit/include/eunit.hrl").

char_substitute_test() ->
    ?assertEqual("abc_DEF_jhi", mzb_string:char_substitute("abc DEF jhi", $ , $_)).
