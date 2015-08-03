#!/usr/bin/env escript

main([BenchDir]) ->
    LogFile = filename:join(BenchDir, "log.txt"),
    StatusFile = filename:join(BenchDir, "status"),
    case {file:read_file(LogFile), file:consult(StatusFile)} of
        {{ok, B}, {ok, [Status = #{config:= Config}]}} ->
            case Config of
                #{log_compression:= _} -> ok;
                undefined -> ok;
                _ ->
                    Compressed = zlib:compress(B),
                    ok = file:write_file(LogFile, Compressed),
                    NewConfig = Config#{log_compression => deflate, metrics_compression => none},
                    NewStatusContent = io_lib:format("~p.", [Status#{config => NewConfig}]),
                    ok = file:write_file(StatusFile, NewStatusContent)
            end;
        {{error, enoent}, _} -> ok;
        {_, {error, enoent}} -> ok
    end.

