-module(mzb_string_tests).
-include_lib("eunit/include/eunit.hrl").

char_substitute_test() ->
    ?assertEqual("abc_DEF_jhi", mzb_string:char_substitute("abc DEF jhi", $ , $_)).

str_to_bstr_test() ->
    ?assertEqual([#{<<"foo">> => <<"bar">>, <<"zoo">> => [true, <<"ding">>, <<"dont">>]}],
                 mzb_string:str_to_bstr([#{<<"foo">> => "bar", "zoo" => [true, "ding", <<"dont">>]}])).

list_to_number_test() ->
    ?assertEqual(15, mzb_string:list_to_number("15")),
    ?assertEqual(15.0, mzb_string:list_to_number("15.0")),
    ?assertEqual(15.451, mzb_string:list_to_number("15.451")).
