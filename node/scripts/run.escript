#!/usr/bin/env escript
-mode(compile).

add_libs() ->
    CodePaths = [filename:join(filename:dirname(escript:script_name()), "../apps/mz_bench/ebin/")] ++
                 filelib:wildcard("/mz/mz_bench/lib/mz_bench-*/ebin/") ++
                 filelib:wildcard("/mz/mz_bench/lib/jiffy-*/ebin/"),
    code:add_pathsz(CodePaths).

main([Node, ScriptName]) ->
    run(Node, ScriptName, undefined, undefined);
main([Node, ScriptName, ReportFile]) ->
    run(Node, ScriptName, ReportFile, undefined);
main([Node, ScriptName, ReportFile, EnvFile]) ->
    run(Node, ScriptName, ReportFile, EnvFile);

main(_) ->
    usage().

run(NodeStr, Script, ReportFile, EnvFile) ->
    add_libs(),
    Node = erlang:list_to_atom(NodeStr),
    mzb_escript_shared:start_and_connect(Node),
    Env = mzb_escript_shared:read_env(EnvFile),
    case rpc:call(Node, mzb_bench_sup, run_script, [Script, Env, ReportFile]) of
        {ok, Ref} ->
            case rpc:call(Node, mzb_bench_sup, get_results, [Ref], infinity) of
                {ok, Str} ->
                    io:format("~s~n", [Str]);
                {error, _, Str} ->
                    io:format("~s~n", [Str]),
                    erlang:halt(1)
            end;
        {error, _, _E, _ST, Messages} -> 
            lists:map(fun(X) -> io:format(standard_error, X ++ "~n", []) end, Messages),
            erlang:halt(1)
    end.

usage() ->
    io:format("Usage: ~s ScriptName [Host]~n", [escript:script_name()]).

