-module(mzb_compiler_tests).
-include_lib("eunit/include/eunit.hrl").

compile_id_test() ->
    Simple = "[{pool, [{size, 1}], []}].",
    _ = compile_check(Simple, []),
    ?assertEqual(0 + 2, length(mzb_compiled_vars:module_info(exports))).

compile_var_string_test() ->
    _ = compile_check(
        "{print, {var, \"foo\"}}.",
        [{"foo", "bar"}]),
    Functions = mzb_compiled_vars:module_info(exports),
    io:format("~p", [Functions]),
    ?assertEqual(true , lists:member({foo_list, 0}, Functions)).

compile_var_int_test() ->
    _ = compile_check(
        "{print, {var, \"foo\"}}.",
        [{"foo", 42}]),
    Functions = mzb_compiled_vars:module_info(exports),
    io:format("~p", [Functions]),
    ?assertEqual(true , lists:member({foo_integer, 0}, Functions)).

compile_numvar_test() ->
    _ = compile_check(
        "{print, {numvar, \"foo\"}}.",
        [{"foo", "42"}]),
    Functions = mzb_compiled_vars:module_info(exports),
    io:format("~p", [Functions]),
    ?assertEqual(42, mzb_compiled_vars:foo_integer()).

compile_rate_test() ->
    _ = compile_check(
        "{{var, \"foo\"}, rps}.",
        [{"foo", 42}]),
    Functions = mzb_compiled_vars:module_info(exports),
    io:format("~p", [Functions]),
    ?assertEqual(true , lists:member({foo_integer, 0}, Functions)).


compile_var_string_to_int_default_test() ->
    _ = compile_check(
        "{print, {var, \"foo\", 1}}.",
        [{"foo", "42"}]),

    Functions = mzb_compiled_vars:module_info(exports),
    io:format("~p", [Functions]),
    ?assertEqual(42, mzb_compiled_vars:foo_integer()).

compile_var_string_to_int_default2_test() ->
    _ = compile_check(
        "{print, {var, \"foo\", 1}}.",
        []),

    Functions = mzb_compiled_vars:module_info(exports),
    io:format("~p", [Functions]),
    ?assertEqual(1, mzb_compiled_vars:foo_integer()).

compile_var_int_to_atom_default_test() ->
    _ = compile_check(
        "{print, {var, \"foo\", undefined}}.",
        [{"foo", 1}]),
    Functions = mzb_compiled_vars:module_info(exports),
    io:format("~p", [Functions]),
    ?assertEqual(1, mzb_compiled_vars:foo_integer()).

compile_var_float_to_atom_default_test() ->
    _ = compile_check(
        "{print, {var, \"foo\", undefined}}.",
        [{"foo", 3.14}]),
    Functions = mzb_compiled_vars:module_info(exports),
    io:format("~p", [Functions]),
    ?assertEqual(3.14, mzb_compiled_vars:foo_float()).

compile_missing_var_is_a_failure_test() ->
    _ = compile_check("{print, {var, \"foo\"}}.", []),
    ?assertEqual(0 + 2, length(mzb_compiled_vars:module_info(exports))).

compile_using_iterator_as_var_test() ->
    _ = compile_check("{loop, [{iterator, \"i\"}], {var, \"i\"}}.", []),
    ?assertEqual(0 + 2, length(mzb_compiled_vars:module_info(exports))).

compile_using_iterator_as_numvar_test() ->
    _ = compile_check("{loop, [{iterator, \"i\"}], {numvar, \"i\"}}.", []),
    ?assertEqual(0 + 2, length(mzb_compiled_vars:module_info(exports))).


compile_check(Input, Env) ->
    AST = mzbl_script:parse(Input),
    {NewScript, Modules} = mzb_compiler:compile(AST, Env),
    lists:foreach(fun ({Mod, Bin}) ->
            {module, _} = code:load_binary(Mod, mzb_string:format("~s.erl", [Mod]), Bin)
        end, Modules),
    NewScript.

