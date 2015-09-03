-module(dummy_worker_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("mzbench_language/include/mzbl_types.hrl").

empty_script_test() ->
    ?assertEqual("", run("")).

oneliner_test() ->
    Script = "{test_method, \"Hello\"}.",
    ?assertEqual("Hello", run(Script)).

print_three_test() ->
    Script = "[{test_method, \"FOO\"}, {test_method, \"BAR\"}, {test_method, \"BAZ\"}].",
    ?assertEqual("BAZBARFOO", run(Script)).

print_loop_test() ->
    Script =
        "[{loop, [{time, {1000, ms}},
                  {rate, {4, rps}}],
           [{test_method, \"F\"}]}].",
    Res = case run(Script) of
        "FFFF" -> ok;
        "FFFFF" -> ok;
        S -> {error, S}
    end,
    ?assertEqual(ok, Res).

print_degenerate_ramp_test() ->
    Script =
        "[{loop, [{time, {4000, ms}},
                  {rate, {ramp, linear, {3, rps}, {3, rps}}}],
           [{test_method, \"F\"}]}].",
    Res = case run(Script) of
        "FFFFFFFFFFFF"  -> ok;
        "FFFFFFFFFFFFF" -> ok;
        S -> {error, S}
    end,
    ?assertEqual(ok, Res).

print_ramp_test() ->
    Script =
        "[{loop, [{time, {3000, ms}},
                  {rate, {ramp, linear, {2, rps}, {6, rps}}}],
           [{test_method, \"F\"}]}].",
    Res = case run(Script) of
        "FFFFFFFFFFF"  -> ok;
        "FFFFFFFFFFFF" -> ok;
        S -> {error, S}
    end,
    ?assertEqual(ok, Res).

print_downward_ramp_test() ->
    Script =
        "[{loop, [{time, {3000, ms}},
                  {rate, {ramp, linear, {7, rps}, {1, rps}}}],
           [{test_method, \"F\"}]}].",
    Res = case run(Script) of
        "FFFFFFFFFFFF" -> ok;
        "FFFFFFFFFFF" ->  ok;
        S -> {error, S}
    end,
    ?assertEqual(ok, Res).

print_loop_iterator_test() ->
    Script =
        "[{loop, [{time, {1100, ms}},
                  {iterator, \"i\"},
                  {rate, {4, rps}}],
           [{test_method, {sprintf, \"~p~w\", [{var, \"i\"}, {numvar, \"i\"}]}}]}].",
    ?assertEqual("4433221100", run(Script)).

empty_loop_test() ->
    Script =
        "[{loop, [{time, {0, ms}},
                  {rate, {4, rps}}],
           [{print, \"foo\"}]}].",
    ?assertEqual("", run(Script)).

empty_ramp_test() ->
    Script =
        "[{loop, [{time, {0, ms}},
                  {rate, {ramp, linear, {1, rps}, {4, rps}}}],
           [{print, \"foo\"}]}].",
    ?assertEqual("", run(Script)).

empty_loop2_test() ->
    Script =
        "[{loop, [{time, {1100, ms}},
                  {rate, {4, rps}}],
           []}].",
    ?assertEqual("", run(Script)).

empty_loop3_test() ->
    Script =
        "[{loop, [{time, {1100, ms}},
                  {rate, {0, rps}}],
           [{print, \"FOO\"}]}].",
    ?assertEqual("", run(Script)).

parallel_loop_test() ->
    Script =
        "[{loop, [{time, {1100, ms}},
                  {parallel, 2},
                  {rate, {4, rps}}],
           [{print, \"ohai\"}]}].",
    ?assertEqual("", run(Script)).

error_test() ->
    Script = "[{print, \"FOO\"},
               {print, \"BAR\"},
               {error, \"Error successfully failed.\"},
               {print, \"BAZ\"}].",
    ?assertError("Error successfully failed.", run(Script)).

tuple_test() ->
    Script = "[{test_method, {sprintf, \"~p\", [{t, {test_method, \"Foo\"}, 2, 3}]}}].",
    ?assertEqual("{nil,2,3}Foo", catch run(Script)).

round_robin_test_() ->
    Script = "[{test_method, {round_robin, [\"1\", \"2\", \"3\"]}}].",
    [?_assertEqual("1", catch run(Script, [{worker_id, 0}])),
     ?_assertEqual("2", catch run(Script, [{worker_id, 1}])),
     ?_assertEqual("2", catch run(Script, [{worker_id, 301}]))].

sequence_test() ->
    Script = "[{test_method, {sprintf, \"~p\", [{seq, 1, 7}]}}].",
    ?assertEqual("[1,2,3,4,5,6,7]", catch run(Script)).

run(Script) ->
    run(Script, []).

run(Script, Meta) ->
    run(Script, Meta, []).

run(Script, Meta, Env) ->
    try
        AST = mzbl_ast:add_meta(mzbl_script:parse(Script), Meta),
        meck:new(exometer),
        meck:expect(exometer, update_or_create, fun(_,_,_,_) -> ok end),
        meck:new(mz_histogram),
        meck:expect(mz_histogram, notify, fun(_,_) -> ok end),
        {_, {dummy_worker, R}} = mzb_worker_runner:eval_expr(
            AST,
            mzb_erl_worker:init(dummy_worker),
            Env,
            mzb_erl_worker),
        R
    after
%        catch meck:unload(folsom_metrics),
        catch meck:unload(exometer),
        catch meck:unload(mz_histogram)
    end.

ramp_solver_test() ->
    ?assertEqual(
        3000000,
        round(mzb_worker_runner:time_of_next_iteration_in_ramp(2, 6, 3000000, 12))),
    ?assertEqual(
        0,
        round(mzb_worker_runner:time_of_next_iteration_in_ramp(2, 8, 4000000, 0))),
    ?assertEqual(
        4000000,
        round(mzb_worker_runner:time_of_next_iteration_in_ramp(2, 8, 4000000, 20))).

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
    mzbl_literals:convert(mzbl_ast:transform(Expr)).