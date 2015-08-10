#!/usr/bin/env escript

main([BenchDir]) ->
    StatusFile = filename:join(BenchDir, "status"),
    case file:consult(StatusFile) of
        {ok, [Status = #{metrics:= Metrics}]} ->
            case Metrics of
                #{graphite_url := _GraphiteUrl} -> ok;
                #{<<"graphite_url">> := _GraphiteUrl} -> ok;
                M when is_map(M), map_size(M) == 0 -> ok;
                M when is_map(M) ->
                    [{GraphiteUrl, OldMetics}] = maps:to_list(Metrics),
                    Graphs = [#{<<"metrics">> => [#{<<"name">> => T} || T<- Targets]} || Targets <- OldMetics],
                    NewMetrics = #{ <<"graphite_url">> => GraphiteUrl,
                                    <<"groups">> => [#{<<"name">> => <<"Default">>,
                                                       <<"graphs">> => Graphs}]},
                    NewStatusContent = io_lib:format("~p.", [Status#{metrics => NewMetrics}]),
                    ok = file:write_file(StatusFile, NewStatusContent)
            end;
        {error, enoent} -> ok;
        {error, Reason} ->
            io:format("Can't read status file: ~s with reason: ~p", [StatusFile, Reason]),
            ok
    end.


