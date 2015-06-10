-module(worker_script_validator_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("mz_bench/include/mzb_types.hrl").
-include_lib("mz_bench/include/mzb_ast.hrl").

validation_ok_simple_test() ->
    ?assertEqual(check("[{print, \"NaNNaNNaNNaNNaNNaN\"},
                         {print, \"BATMAN\"}]."),
                 ok).

validation_ok_larger_test() ->
    Script = "[{print, \"127.0.0.1\"},
               {loop, [{time, {1, min}},
                       {rate, {1, rps}}],
                [{print, {choose, [\"queue1\", \"queue2\", \"queue3\", \"queue4\"]}}]},
               {wait, {1, sec}},
               {loop, [{time, {1, min}},
                       {rate, {ramp, linear, {30, rpm}, {2, rps}}}],
                [{print, \"Getting there\"}]},
               {wait, {5, min}},
               {loop, [{time, {1, min}}, {rate, {10, rps}}],
                [{print, {choose, 1, [\"queue1\", \"queue2\", {choose, 2, [\"q0\", \"QUEUE_ENV_VAR\"]}]}}]},
               {error, \"Bye\"}
              ].",
    ?assertEqual(ok, check(Script)).

validation_loop_test() ->
    ?assertEqual({invalid_script, ["line 1: Loop spec must specify duration.",
                                   "line 3: Loop must have a spec and a body."]},
                 check("[{loop, [{rate, {3, rps}}],
                            [{print, \"NaN\"}]},
                         {loop, [{print, \"BATMAN\"}]}].")).

validation_empty_loopspec_test() ->
    ?assertEqual({invalid_script, ["line 1: Loop spec must specify duration."]},
        check("[{loop, [], [{print, \"BATMAN\"}]}].")).

validation_no_rate_in_loopspec_test() ->
    ?assertEqual(ok,
        check("[{loop, [{time, {1, min}}], [{print, \"BATMAN\"}]}].")).

validation_no_time_in_loopspec_test() ->
    ?assertEqual({invalid_script, ["line 1: Loop spec must specify duration."]},
        check("[{loop, [{rate, {1, rpm}}], [{print, \"BATMAN\"}]}].")).

validation_no_rate_no_time_in_loopspec_test() ->
    ?assertEqual({invalid_script, ["line 1: Loop spec must specify duration."]},
        check("[{loop, [{iterator, \"i\"}], [{print, \"BATMAN\"}]}].")).

validation_bad_rate_in_loopspec_test() ->
    ?assertEqual({invalid_script, ["line 2: Unexpected rate."]},
        check("[{loop, [{time, {1, min}},
                        {rate, billion_rps}],
                    [{print, \"BATMAN\"}]}].")).

validation_bad_time_in_loopspec_test() ->
    ?assertEqual({invalid_script, ["line 1: Unexpected time."]},
        check("[{loop, [{time, zomg_fast},
                        {rate, {1, rps}}],
                    [{print, \"BATMAN\"}]}].")).

validation_bad_ramp_type_in_loopspec_test() ->
    ?assertEqual({invalid_script, ["line 2: Unexpected rate."]},
        check("[{loop, [{time, {1, sec}},
                        {rate, {ramp, dirichlet, {1, rps}, {1, rps}}}],
                    [{print, \"BATMAN\"}]}].")).

validation_bad_ramp_values_in_loopspec_test() ->
    ?assertEqual({invalid_script, ["line 2: Unexpected rate."]},
        check("[{loop, [{time, {1, sec}},
                        {rate, {ramp, linear, {1, rps}, to_the_moon}}],
                    [{print, \"BATMAN\"}]}].")).

validation_empty_instruction_test() ->
    ?assertEqual({invalid_script, ["line 2: Empty instruction."]},
                 check("[{print, \"foo\"},
                         {},
                         {print, \"bar\"},
                         {print, \"baz\"}].")).

check(S) ->
    Script = string_to_script(S),
    mzb_worker_script_validator:validate_worker_script(
        Script,
        {mzb_erl_worker, dummy_worker}).

string_to_script(S) ->
    {ok, Tokens, _} = erl_scan:string(S),
    {ok, [Expr]} = erl_parse:parse_exprs(Tokens),
    mzb_literals:convert(mzb_ast:transform(Expr)).
