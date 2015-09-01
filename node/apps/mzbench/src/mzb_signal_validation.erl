-module(mzb_signal_validation).

-export([validate/1]).

-include_lib("mzbench_language/include/mzbl_types.hrl").

validate(Script) ->
    Graph = signals_to_graph(Script),
    SCCs = [SCC ||
            SCC <- digraph_utils:cyclic_strong_components(Graph),
            length(lists:usort(SCC)) > 1],
    Standalones = standalone_vertices(Graph),
    Errors = [format_signal_deadlock_error(SCC) || SCC <- SCCs] ++
             [format_standalone_error(V) || V <- Standalones],
    Edges = [digraph:edge(Graph, E) || E <- digraph:edges(Graph)],
    lager:info("signals graph: ~p", [Edges]),
    lager:info("signals graph sccs: ~p", [SCCs]),
    lager:info("standalone signals: ~p", [Standalones]),
    case Errors of
        [] -> ok;
        _  ->
            erlang:error({error, {validation, Errors}})
    end.

signals_to_graph(Script) ->
    Pools = [S || #operation{name = pool, args = [_, S]} <- Script],
    G = digraph:new(),
    lists:foreach(fun (X) -> populate_graph(X, undefined, G) end, Pools),
    _ = add_cross_pool_vertices(G, digraph:vertices(G)),
    G.

add_and_connect(FName, SName, Meta, PrevVertex, Graph) ->
    [PoolName] = proplists:get_all_values(pool_name, Meta),
    NewVertex = {PoolName, FName, SName},
    digraph:add_vertex(Graph, NewVertex, Meta),
    case PrevVertex of
        undefined -> ok;
        _ -> digraph:add_edge(Graph, PrevVertex, NewVertex)
    end,
    NewVertex.

populate_graph(L, PrevVertex, G) when is_list(L) ->
    lists:foldl(fun(X, Acc2) -> populate_graph(X, Acc2, G) end, PrevVertex, L);
populate_graph(#operation{name = FName, args = [L], meta = Meta},
               PrevVertex, G) when FName == parallel ->
    ParallelVertex = add_and_connect(FName, start, Meta, PrevVertex, G),
    Vertices = lists:map(fun(X) -> populate_graph(X, ParallelVertex, G) end, L),
    NewVertices = [X || X <- Vertices, X =/= ParallelVertex],
    case NewVertices of
        [] -> ParallelVertex;
        [H | T] -> EndParallelVertex = add_and_connect(FName, finish, Meta, H, G),
                   lists:map(fun(X) -> digraph:add_edge(G, X, EndParallelVertex) end, T),
                   EndParallelVertex
    end;
populate_graph(#operation{name = loop, args = [_, Body]},
               PrevVertex, G) -> populate_graph(Body, PrevVertex, G);
populate_graph(#operation{name = FName, args = [SName], meta = Meta},
               PrevVertex, Graph) when FName == wait_signal;
                                        FName == set_signal ->
    add_and_connect(FName, SName, Meta, PrevVertex, Graph);

populate_graph(#operation{args = L}, PrevVertex, G) -> populate_graph(L, PrevVertex, G);
populate_graph(#constant{value = V}, PrevVertex, G) -> populate_graph(V, PrevVertex, G);
populate_graph(_, V, _) -> V.

add_cross_pool_vertices(Graph, Vertices) ->
    [ digraph:add_edge(Graph, From, To) ||
        {_, set_signal, S1} = From <- Vertices,
        {_, wait_signal, S2} = To  <- Vertices,
        S1 == S2
    ].

standalone_vertices(G) ->
    lists:filter(
        fun ({_, set_signal, N} = V) ->
                [] == [V2 || {_, wait_signal, N2} = V2 <- digraph:out_neighbours(G, V), N2 == N];
            ({_, wait_signal, N} = V) ->
                [] == [V2 || {_, set_signal, N2} = V2 <- digraph:in_neighbours(G, V), N2 == N];
            ({_, parallel, _}) -> false
        end, digraph:vertices(G)).

format_standalone_error({_, wait_signal, N}) ->
    mzb_string:format("Nobody sets signal ~p", [N]);
format_standalone_error({_, set_signal, N}) ->
    mzb_string:format("Nobody waits for signal ~p", [N]).

format_signal_deadlock_error(Cycle) ->
    FormatedCycle = [format_vertex(V) || V <- Cycle],
    mzb_string:format("Deadlock is posible: ~s", [string:join(FormatedCycle, " -> ")]).

format_vertex({Pool, Op, Arg}) ->
    io_lib:format("~s:~s(~p)", [Pool, Op, Arg]).

