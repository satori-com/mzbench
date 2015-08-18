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
                    ScriptName = case Status of
                                     #{config := #{script:= #{name:= SName}}} -> SName;
                                     _ -> ""
                                 end,

                    Name = filename:basename(ScriptName, ".erl"),
                    GraphitePrefix = re:replace(Name, "[^a-zA-Z0-9]", "_", [{return, list}, global]),
                    BinGraphitePrefix = list_to_binary(GraphitePrefix),
                    PrefixSize = byte_size(BinGraphitePrefix),

                    TargetMigrator = fun (Target) ->
                        case Target of
                            <<BinGraphitePrefix:PrefixSize/binary, $., Rest/binary>> -> Rest;
                            _ -> Target
                        end
                    end,

                    [{GraphiteUrl, OldMetics}] = maps:to_list(Metrics),

                    Graphs = [#{<<"metrics">> => [#{ <<"name">> => TargetMigrator(T) } || T <- Targets]} || Targets <- OldMetics],
                    NewMetrics = #{ <<"graphite_url">> => GraphiteUrl,
                                    <<"graphite_prefix">> => BinGraphitePrefix,
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


