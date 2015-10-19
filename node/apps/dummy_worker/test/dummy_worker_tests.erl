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
    ?assertEqual("4433221100", catch run(Script)).

empty_loop_test() ->
    Script =
        "[{loop, [{time, {0, ms}},
                  {rate, {4, rps}}],
           [{test_method, \"foo\"}]}].",
    ?assertEqual("", run(Script)).

empty_ramp_test() ->
    Script =
        "[{loop, [{time, {0, ms}},
                  {rate, {ramp, linear, {1, rps}, {4, rps}}}],
           [{test_method, \"foo\"}]}].",
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
           [{test_method, \"FOO\"}]}].",
    ?assertEqual("", run(Script)).

parallel_loop_test() ->
    Script =
        "[{loop, [{time, {1100, ms}},
                  {parallel, 2},
                  {rate, {4, rps}}],
           [{test_method, \"ohai\"}]}].",
    ?assertEqual("", run(Script)).

error_test() ->
    Script = "[{test_method, \"FOO\"},
               {test_method, \"BAR\"},
               {error, \"Error successfully failed.\"},
               {test_method, \"BAZ\"}].",
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

eval_std_test() ->
    Script = "{seq, {numvar, \"V1\"}, {round_robin, [10,11]}}.",
    Meta = [{worker_id, 3}],
    Env = [{"V1", "5"}],
    AST = mzbl_ast:add_meta(mzbl_script:parse(Script), Meta),
    R = mzbl_interpreter:eval_std(AST, Env),
    ?assertEqual([5,6,7,8,9,10,11], R).

eval_std_compiled_test() ->
    Script = "{seq, {numvar, \"V1\"}, {round_robin, [10,11]}}.",
    Meta = [{worker_id, 3}],
    Env = [{"V1", "5"}],
    AST = mzbl_ast:add_meta(mzbl_script:parse(Script), Meta),
    NewAST = compile_and_load(AST, Env),
    R = mzbl_interpreter:eval_std(NewAST, []),
    ?assertEqual([5,6,7,8,9,10,11], R).

eval_replace_int_with_float_test() ->
    Script = "{sprintf, \"~p\", [{numvar, \"V1\"}]}.",
    AST = mzbl_script:parse(Script),
    Env1 = [{"V1", "5"}],
    NewAST = compile_and_load(AST, Env1),
    R1 = mzbl_interpreter:eval_std(NewAST, []),
    ?assertEqual("5", R1),
    Env2 = [{"V1", "5.6"}],
    _ = compile_and_load(AST, Env2),
    R2 = mzbl_interpreter:eval_std(NewAST, []),
    ?assertEqual("5.6", R2).

compile_and_load(AST, Env) ->
    {NewAST, Modules} = mzb_compiler:compile(AST, Env),
    lists:foreach(fun ({Mod, Bin}) ->
            {module, _} = code:load_binary(Mod, mzb_string:format("~s.erl", [Mod]), Bin)
        end, Modules),
    NewAST.

run(Script) ->
    run(Script, []).

run(Script, Meta) ->
    run(Script, Meta, []).

run(Script, Meta, Env) ->
    AST = mzbl_ast:add_meta(mzbl_script:parse(Script), Meta),
    {_, {dummy_worker, R}} = mzbl_interpreter:eval(
        AST,
        mzb_erl_worker:init(dummy_worker),
        Env,
        mzb_erl_worker),
    R.

ramp_solver_test() ->
    ?assertEqual(
        3000,
        round(mzbl_loop:time_of_next_iteration({linear_rate, undefined, undefined, 2, 6}, 3000, 12))),
    ?assertEqual(
        0,
        round(mzbl_loop:time_of_next_iteration({linear_rate, undefined, undefined, 2, 8}, 4000, 0))),
    ?assertEqual(
        4000,
        round(mzbl_loop:time_of_next_iteration({linear_rate, undefined, undefined, 2, 8}, 4000, 20))),
    ?assertEqual(
        2000,
        round(mzbl_loop:time_of_next_iteration({const_rate, undefined, 10}, 5000, 20))).

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

check(S) ->
    Script = string_to_script(S),
    mzb_worker_script_validator:validate_worker_script(
        Script,
        {mzb_erl_worker, dummy_worker}).

string_to_script(S) ->
    {ok, Tokens, _} = erl_scan:string(S),
    {ok, [Expr]} = erl_parse:parse_exprs(Tokens),
    mzbl_literals:convert(mzbl_ast:transform(Expr)).
