#!/usr/bin/env escript

main([BenchDir]) ->
    MetricsFile = filename:join(BenchDir, "metrics.txt"),
    OutputFilePattern = filename:join(BenchDir, "metrics_~s.txt"),
    case file:open(MetricsFile, [raw, binary, read]) of
        {ok, H} ->
            _ = parse_file(H, OutputFilePattern, _State = #{}),
            ok = file:close(H),
            ok = file:delete(MetricsFile);
        {error, enoent} -> ok;
        {error, Reason} ->
            io:format("Can't read metrics file: ~s with reason: ~p", [MetricsFile, Reason]),
            erlang:error({file_read_error, MetricsFile, Reason})
    end,

    StatusFile = filename:join(BenchDir, "status"),
    case file:consult(StatusFile) of
        {ok, [Status]} ->
            NewStatusContent = io_lib:format("~p.", [migrate(Status)]),
            ok = file:write_file(StatusFile, NewStatusContent);
        {error, enoent} -> ok;
        {error, Reason2} ->
            io:format("Can't read status file: ~s with reason: ~p", [StatusFile, Reason2]),
            erlang:error({file_read_error, StatusFile, Reason2})
    end.

parse_file(H, OutputFile, State) ->
    case file:read_line(H) of
        {ok, D} ->
            [TS, M, V] = binary:split(D, <<"\t">>, [global]),
            case maps:find(M, State) of
                {ok, Output} ->
                    ok = file:write(Output, <<TS/binary, "\t", V/binary>>),
                    parse_file(H, OutputFile, State);
                error ->
                    F = lists:flatten(io_lib:format(OutputFile, [re:replace(M, "\\W", "_", [global, {return, list}])])),
                    {ok, Output} = file:open(F, [raw, binary, write]),
                    ok = file:write(Output, <<TS/binary, "\t", V/binary>>),
                    parse_file(H, OutputFile, maps:put(M, Output, State))
            end;
        eof ->
            _ = [ file:close(O) || {_, O} <- maps:to_list(State)],
            #{};
        {error, Error} ->
            io:format("Failed to read from metrics file: ~p",[Error]),
            erlang:error({read_metrics_error, Error})
    end.

migrate(Status = #{config:= Config}) ->
    Status#{config => Config#{metrics_file => "metrics_~s.txt"}};
migrate(Status = #{}) ->
    Status.

