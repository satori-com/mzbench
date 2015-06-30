-module(mzbl_literals_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("mz_bench_language/include/mzbl_types.hrl").

bytes_test() ->
    ?assertEqual(#constant{value = 921, units = b}, mzbl_literals:convert(#constant{value = 921, units = b})),
    ?assertEqual(#constant{value = 9216, units = b}, mzbl_literals:convert(#constant{value = 9, units = kb})),
    ?assertEqual(#constant{value = 7340032, units = b}, mzbl_literals:convert(#constant{value = 7, units = mb})),
    ?assertEqual(#constant{value = 7516192768, units = b}, mzbl_literals:convert(#constant{value = 7, units = gb})),
    ?assertEqual(#constant{value = 7696581394432, units = b}, mzbl_literals:convert(#constant{value = 7, units = tb})).

seconds_test() ->
    ?assertEqual(#constant{value = 3000, units = ms}, mzbl_literals:convert(#constant{value = 3, units = sec})),
    ?assertEqual(#constant{value = 120000, units = ms}, mzbl_literals:convert(#constant{value = 2, units = min})),
    ?assertEqual(#constant{value = 7200000, units = ms}, mzbl_literals:convert(#constant{value = 2, units = h})).

rps_test() ->
    ?assertEqual(#constant{value = 12, units = rps}, mzbl_literals:convert(#constant{value = 12, units = rps})),
    ?assertEqual(#constant{value = 2/60, units = rps}, mzbl_literals:convert(#constant{value = 2, units = rpm})),
    ?assertEqual(#constant{value = 2/60/60, units = rps}, mzbl_literals:convert(#constant{value = 2, units = rph})).

complex_test() ->
    Expected = "[{pool, [ { size, {5120, b} },
           { time, { 600000, ms } },
           { worker_type, rmq_worker } ]}].",
    ?assertEqual(string_to_script(Expected),
                 string_to_script("[{pool, [ { size, {5, kb} },
           { time, { 10, min } },
           { worker_type, rmq_worker }]}].")).

string_to_script(S) ->
    {ok, Tokens, _} = erl_scan:string(S),
    {ok, [Expr]} = erl_parse:parse_exprs(Tokens),
    mzbl_literals:convert(mzbl_ast:transform(Expr)).
