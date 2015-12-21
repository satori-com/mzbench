#!/usr/bin/env escript

main([BenchDir]) ->
    StatusFile = filename:join(BenchDir, "status"),
    case file:consult(StatusFile) of
        {ok, [Status]} ->
            NewStatusContent = io_lib:format("~p.", [migrate(Status)]),
            ok = file:write_file(StatusFile, NewStatusContent);
        {error, enoent} ->
            ok;
        {error, Reason} ->
            io:format("Can't read status file: ~s with reason: ~p", [StatusFile, Reason]),
            erlang:error({file_read_error, StatusFile, Reason})
    end.

migrate(Status = #{metrics:= Metrics}) ->
    Metrics1 = normalize(Metrics),
    Status#{metrics => add_metrics_visibility(Metrics1)};
migrate(Status = #{}) ->
    Status.

normalize(Map) when is_map(Map) ->
    maps:from_list(lists:map(
        fun ({K, V}) ->
            {to_atom(K), to_list(V)}
        end, maps:to_list(Map))).

to_list(B) when is_binary(B) -> erlang:binary_to_list(B);
to_list(L) when is_list(L) -> [to_list(E) || E <- L];
to_list(M) when is_map(M) -> normalize(M);
to_list(B) -> B.

to_atom(B) when is_binary(B) -> erlang:list_to_atom(erlang:binary_to_list(B));
to_atom(B) -> B.

add_metrics_visibility(#{groups:= Groups} = Metrics) ->
    Metrics#{groups:= [add_group_visibility(G) || G <- Groups]};
add_metrics_visibility(#{} = Metrics) -> Metrics.

add_group_visibility(#{graphs:= Graphs} = Group) ->
    Group#{graphs => [add_graph_visibility(G) || G <- Graphs]}.

add_graph_visibility(#{metrics:= Metrics} = Graph) ->
    Graph#{metrics => [add_visibility(M) || M <- Metrics]}.

add_visibility(#{visibility:= _} = Metric) -> Metric;
add_visibility(#{} = Metric) ->
    Metric#{visibility => true}.
