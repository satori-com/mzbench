#!/usr/bin/env escript

%% returns (output to stdout) current release vsn from reltool.config

main([AppName]) ->
  main([AppName, "rel/reltool.config"]);

main([AppName, ReltoolPath]) ->
    Rel = case file:consult(ReltoolPath) of
        {ok, Reltool} ->
            Sys = proplists:get_value(sys, Reltool),
            lists:foldl(fun(X, Acc)->
                case X of
                    {rel, AppName, VSN, _} -> VSN;
                    _ -> Acc
                end
            end, "", Sys);
        {error, _} ->
            io:format(standard_error, "Error parsing rel/reltool.config", []),
            halt(1)
    end,
    io:format("~s", [Rel]);

main([]) ->
    io:format("Usage: ./relvsn.erl <AppName> [<ReltoolPath]~n").
