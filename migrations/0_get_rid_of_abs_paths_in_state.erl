#!/usr/bin/env escript

main([BenchDir]) ->
    StatusFile = filename:join(BenchDir, "status"),
    case file:consult(StatusFile) of
        {ok, [Status]} ->
            NewStatusContent = io_lib:format("~p.", [migrate(Status)]),
            ok = file:write_file(StatusFile, NewStatusContent);
        {error, enoent} -> ok;
        {error, Reason} ->
            io:format("Can't read status file: ~s with reason: ~p", [StatusFile, Reason]),
            erlang:error({file_read_error, StatusFile, Reason})
    end.

migrate(Status = #{config:= Config}) ->
    Config1 = maps:remove(remote_dir, Config),
    Config2 = maps:remove(local_dir, Config1),
    Config3 =
        case maps:find(metrics_file, Status) of
            {ok, Val1} -> Config2#{metrics_file => filename:basename(Val1)};
            error -> Config2
        end,
    Config4 =
        case maps:find(log_file, Status) of
            {ok, Val2} -> Config3#{log_file => filename:basename(Val2)};
            error -> Config3
        end,
    Status1 = maps:remove(metrics_file, Status),
    Status2 = maps:remove(log_file, Status),
    Status2#{config => Config4}.
