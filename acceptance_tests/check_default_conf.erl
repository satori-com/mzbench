#!/usr/bin/env escript
-mode(compile).

add_libs() ->
    BinDir = filename:dirname(escript:script_name()),
    CodePaths = filelib:wildcard(filename:join(BinDir, "../server/_build/default/deps/*/ebin/")),
    code:add_pathsz(CodePaths).

compare(List1, List2) ->
    lists:all(fun(X) -> X == ok end,
        lists:map(fun({X, Y}) -> case Y == proplists:get_value(X, List1) of
                                true -> ok;
                                _ -> io:format("Error: ~p should be equal to ~p for ~p~n", 
                                    [Y, proplists:get_value(X, List1), X]),
                                    error end end, List2)).
check_configs({App, List}) ->
    XList = [server_configs, included_applications, node_management_port, mail,
             node_log_port],
    Original = application:get_all_env(App),
    Original2 = lists:foldl(fun proplists:delete/2, Original, XList),
    case {compare(List, Original2),compare(Original2, List)} of
        {true, true} -> ok;
        _ -> erlang:error(default_config_mismatch)
    end.

main(_) ->
    add_libs(),
    {ok, _} = application:ensure_all_started(mzbench_api),
    {ok, [Terms]} = file:consult("../server/server.config.example"),
    lists:map(fun check_configs/1, Terms),ok.
