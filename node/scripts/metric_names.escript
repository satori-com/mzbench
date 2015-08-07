#!/usr/bin/env escript
-mode(compile).

add_libs() ->
    BinDir = filename:dirname(escript:script_name()),
    CodePaths = [filename:join(BinDir, "../apps/mzbench/ebin/")] ++
                 filelib:wildcard(filename:join(BinDir, "../lib/mzbench-*/ebin/")) ++
                 filelib:wildcard(filename:join(BinDir, "../lib/jiffy-*/ebin/")),
    code:add_pathsz(CodePaths).

main([NodeStr, Path]) -> main([NodeStr, Path, undefined]);
main([NodeStr, Path, EnvFile]) ->
    add_libs(),
    Node = erlang:list_to_atom(NodeStr),
    mzb_escript_shared:start_and_connect(Node),
    Env = mzb_escript_shared:read_env(EnvFile),
    case rpc:call(Node, mzb_script_validator, read_and_validate, [filename:absname(Path), Env]) of
        {ok, _, Env2} ->
            Data = case rpc:call(Node, mzb_script_metrics, metrics, [Path, Env2]) of
                {badrpc, Reason} ->
                    io:format("rpc failed with reason ~p~n", [Reason]),
                    erlang:halt(1);
                Metrics when is_map(Metrics) ->
                    Metrics;
                _ -> {[]}
            end,
            io:format("~s~n", [jiffy:encode(Data)]);
        {error, _, _, _, Messages} ->
            lists:map(fun(X)->io:format("~s~n", [X]) end, Messages),
            erlang:halt(1);
        {badrpc, Reason} ->
            io:format("rpc failed with reason ~p~n", [Reason]),
            erlang:halt(1)
    end.
