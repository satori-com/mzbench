#!/usr/bin/env escript
-mode(compile).

add_libs() ->
    BinDir = filename:dirname(escript:script_name()),
    CodePaths = [filename:join(BinDir, "../apps/mzbench/ebin/")] ++
                 filelib:wildcard(filename:join(BinDir, "../lib/mzbench-*/ebin/")) ++
                 filelib:wildcard(filename:join(BinDir, "../lib/jiffy-*/ebin/")),
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

    case rpc:call(Node, mzb_bench_sup, run_bench, [Script, Env]) of
        {ok, R} ->
            io:format("~s~n", [R]);
        {error, Messages} -> 
            io:format(standard_error, string:join(Messages, "\n"), []),
            erlang:halt(1)
    end.

usage() ->
    io:format("Usage: ~s ScriptName [Host]~n", [escript:script_name()]).

