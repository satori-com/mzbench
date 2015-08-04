-module(mzb_python_worker_test).
-include_lib("eunit/include/eunit.hrl").

encode_str_for_python_test() ->
    ?assertEqual("hello\\'world\\\\!!!11", mzb_python_worker:encode_str_for_python("hello'world\\!!!11")).

encode_for_python_test() ->
    ?assertEqual("'hello\\'world'", mzb_python_worker:encode_for_python("hello'world")),
    ?assertEqual("'hello'", mzb_python_worker:encode_for_python(hello)),
    ?assertEqual("2", mzb_python_worker:encode_for_python(2)),
    ?assertEqual("2.50000000000000000000e+00", mzb_python_worker:encode_for_python(2.5)).
