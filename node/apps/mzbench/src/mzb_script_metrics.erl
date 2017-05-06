-module(mzb_script_metrics).

-export([script_metrics/2, normalize/1, format_error/1, build_metric_groups_json/1]).

-include_lib("mzbench_language/include/mzbl_types.hrl").

script_metrics(Pools, _WorkerNodes) ->

    WorkerStatusGraphs = lists:map(fun (P) ->
        {graph, #{title => mzb_string:format("Worker status (~s)", [pool_name(P)]),
                  metrics => [{mzb_string:format("workers.~s.~s", [pool_name(P), X]), counter} || X <- ["started", "ended", "failed"]]
                            }}
        end, Pools),

    MZBenchInternal = [{group, "MZBench Internals",
                        WorkerStatusGraphs ++
                        [
                          {graph, #{title => "Errors and Blocked workers",
                                    metrics => [{"errors.system", counter},
                                                {"errors.user", counter},
                                                {"blocked.workers", counter}]}},
                          {graph, #{title => "Logs",
                                    metrics => [{"logs.written", counter},
                                                {"logs.dropped.mailbox_overflow", counter},
                                                {"logs.dropped.rate_limiter", counter}
                                                ]}},
                          {graph, #{title => "Metric merging time",
                                    units => "ms",
                                    metrics => [{"metric_merging_time", gauge}]}}
                        ]}],
    try
        PoolMetrics = pool_metrics(Pools),
        normalize(PoolMetrics ++ MZBenchInternal)
    catch
        _:Error ->
            ST = erlang:get_stacktrace(),
            system_log:error("Metrics declaration error: ~s", [mzb_script_metrics:format_error(Error)]),
            erlang:raise(error, Error, ST)
    end.

pool_metrics(Pools) ->
    PoolWorkers = [mzbl_script:extract_worker(PoolOpts) ||
                   #operation{name = pool, args = [PoolOpts, _Script]} <- Pools],
    UniqPoolWorkers = mzb_lists:uniq(PoolWorkers),
    AddWorker = fun(MetricGroups, Worker) ->
                    lists:map(fun ({group, Name, Graphs}) ->
                        {group, Name, lists:map(fun ({graph, Opts = #{metrics:= Metrics}}) ->
                            {graph, Opts#{metrics => [{N, T, O#{worker => Worker}} || {N, T, O} <- Metrics]}}
                        end, Graphs)}
                    end, MetricGroups)
                end,
    lists:append([AddWorker(normalize(P:metrics(W)), Worker) || {P, W} = Worker <- UniqPoolWorkers]).

pool_name(Pool) ->
    #operation{name = pool, meta = Meta} = Pool,
    proplists:get_value(pool_name, Meta).

normalize(Seq) when is_list(Seq) ->
    {Grouped, Groupless} = lists:partition(fun (X) ->
                                                   is_tuple(X) andalso element(1, X) == group
                                           end, Seq),

    GrouplessNormalized = [normalize_graph(G) || G <- Groupless],
    DefaultGroups =
        case length(GrouplessNormalized) > 0 of
            true -> [{group, "Default", GrouplessNormalized}];
            false -> []
        end,

    NormalizedGroup = [normalize_group(G) || G <- Grouped],

    merge_groups(DefaultGroups ++ NormalizedGroup);
normalize(Seq) ->
    erlang:error({metrics_not_list, Seq}).

merge_groups(Groups) -> merge_groups(lists:reverse(Groups), []).

merge_groups([], Res) -> Res;
merge_groups([{group, Name, Graphs} = G | T], Res) ->
    case lists:keytake(Name, 2, Res) of
        {value, {group, _, MoreGraphs}, Res2} ->
            merge_groups(T, [{group, Name, merge_graphs(Graphs ++ MoreGraphs)}|Res2]);
        false ->
            merge_groups(T, [G|Res])
    end;
merge_groups([G|_], _) ->
    erlang:error({invalid_group_format, G}).

merge_graphs(Metrics) when is_list(Metrics) -> merge_graphs(lists:reverse(Metrics), []);
merge_graphs(Metrics) -> erlang:error({graphs_not_list, Metrics}).

merge_graphs([], Res) -> Res;
merge_graphs([{graph, #{title:= Title, metrics:= Metrics} = Opts} = G|T], Res) ->
    case take_metric(Title, Res, []) of
        {{graph, #{metrics:= OldMetrics}}, NewRes} ->
            OldMetricsFiltered = lists:filter(fun ({MName, _, _}) ->
                false == lists:keyfind(MName, 1, Metrics)
                end, OldMetrics),
            merge_graphs(T, [{graph, maps:put(metrics, Metrics ++ OldMetricsFiltered, Opts)}|NewRes]);
        not_found -> merge_graphs(T, [G|Res])
    end;
merge_graphs([G|_], _) ->
    erlang:error({invalid_graph_format, G}).

take_metric(_Title, [], _Acc) -> not_found;
take_metric(Title, [{graph, #{title:= Title}} = M | T], Acc) ->
    {M, lists:reverse(Acc) ++ T};
take_metric(Title, [M | T], Acc) -> take_metric(Title, T, [M|Acc]).

normalize_group({group, Name, Graphs}) when is_list(Graphs) ->
    is_str(Name) orelse erlang:error({invalid_group_name, Name}),
    NormalizedGraphs = [normalize_graph(G) || G <- Graphs],
    {group, Name, NormalizedGraphs};
normalize_group(UnknownFormat) ->
    erlang:error({invalid_group_format, UnknownFormat}).

normalize_graph({graph, Opts}) when is_map(Opts) ->
    Title = mzb_bc:maps_get(title, Opts, "undefined"),
    is_str(Title) orelse erlang:error({invalid_graph_title, Title}),
    Units = mzb_bc:maps_get(units, Opts, "undefined"),
    is_str(Units) orelse erlang:error({invalid_graph_units, Units}),
    Metrics = mzb_bc:maps_get(metrics, Opts, []),
    is_list(Metrics) orelse erlang:error({metrics_not_list, Metrics}),
    NormalizedMetrics = [normalize_metric(M) || M <- Metrics],
    {graph, Opts#{metrics => NormalizedMetrics}};
normalize_graph(OneMetric) when is_tuple(OneMetric) ->
    {graph, #{metrics => [normalize_metric(OneMetric)]}};
normalize_graph(SeveralMetric) when is_list(SeveralMetric) ->
    NormalizedMetrics = [normalize_metric(M) || M <- SeveralMetric],
    {graph, #{metrics => NormalizedMetrics}};
normalize_graph(UnknownFormat) ->
    erlang:error({invalid_graph_format, UnknownFormat}).

normalize_metric({Name, Type}) when is_list(Type) ->
    normalize_metric({Name, list_to_atom(Type)});
normalize_metric({Name, Type, Opts}) when is_list(Type) ->
    normalize_metric({Name, list_to_atom(Type), Opts});
normalize_metric({Name, Type}) when is_atom(Type) ->
    normalize_metric({Name, Type, #{}});
normalize_metric({Name, Type, Opts}) when is_atom(Type) ->
    is_str(Name) orelse erlang:error({invalid_metric_name, Name}),
    NewOpts = normalize_metric_opts(Opts),
    case Type of
        counter -> ok;
        gauge -> ok;
        histogram -> ok;
        derived ->
            case maps:find(resolver, Opts) of
                {ok, FunctionName} when is_atom(FunctionName),
                                        FunctionName =/= undefined -> ok;
                {ok, FunctionName} -> erlang:error({invalid_resolver_function, FunctionName});
                error -> erlang:error({missing_resolver_function, Name})
            end;
        Invalid ->
            erlang:error({invalid_metric_type, Invalid})
    end,
    {Name, Type, NewOpts};
normalize_metric(UnknownFormat) ->
    erlang:error({invalid_metric_format, UnknownFormat}).

normalize_metric_opts(#{} = Opts) ->
    Visibility =
        case maps:find(visibility, Opts) of
            {ok, V} -> V;
            error -> true
        end,
    maps:from_list(lists:map(
        fun ({K, V}) when is_list(K) -> {list_to_atom(K), V};
            ({K, V}) when is_atom(K) -> {K, V}
        end, maps:to_list(maps:put(visibility, Visibility,  Opts))));
normalize_metric_opts(Opts) ->
    erlang:error({opts_not_map, Opts}).

maybe_append_rps_units(GraphOpts, Metrics) ->
    IsRPSGraph = ([] == [M || M = {_,_, Opts} <- Metrics, not mzb_bc:maps_get(rps, Opts, false)]),
    case IsRPSGraph of
        true ->
            Units = case mzb_bc:maps_get(units, GraphOpts, "") of
                "" -> "rps";
                U -> U ++ "/sec"
            end,
            GraphOpts#{units => Units};
        _ -> GraphOpts
    end.

build_metric_groups_json(Groups) ->
    MetricGroups = mzb_metrics:build_metric_groups(Groups),
    lists:map(fun ({group, GroupName, Graphs}) ->
        NewGraphs = lists:map(fun ({graph, GraphOpts}) ->
            Metatype = mzb_bc:maps_get(metatype, GraphOpts, undefined),
            Metrics = mzb_bc:maps_get(metrics, GraphOpts, []),

            MetricMap = lists:flatmap(fun({Name, _Type, Opts}) ->
                Opts1 = mzb_bc:maps_without([rps, worker], Opts),
                [Opts1#{name => Name}]
            end, Metrics),

            GraphOpts1 = maybe_append_rps_units(GraphOpts, Metrics),

            GraphOpts1#{metrics => MetricMap, metatype => Metatype}
        end, Graphs),
        #{name => GroupName, graphs => NewGraphs}
    end, MetricGroups).

is_str(S) ->
    io_lib:printable_list(S).

format_error({metrics_not_list, Seq}) -> mzb_string:format("Metrics should be a list: ~p", [Seq]);
format_error({invalid_group_format, G}) -> mzb_string:format("Invalid group format: ~p", [G]);
format_error({graphs_not_list, Grpahs}) -> mzb_string:format("Graphs should be a list: ~p", [Grpahs]);
format_error({invalid_graph_format, G}) -> mzb_string:format("Invalid graph format: ~p", [G]);
format_error({invalid_group_name, Name}) -> mzb_string:format("Invalid group name: ~p", [Name]);
format_error({invalid_graph_title, Title}) -> mzb_string:format("Invalid graph title: ~p", [Title]);
format_error({invalid_graph_units, Units}) -> mzb_string:format("Invalid graph units: ~p", [Units]);
format_error({invalid_metric_name, Name}) -> mzb_string:format("Invalid metric name: ~p", [Name]);
format_error({invalid_resolver_function, FunctionName}) -> mzb_string:format("Invalid resolver function: ~p", [FunctionName]);
format_error({missing_resolver_function, Name}) -> mzb_string:format("Missing resolver function for derived metric: ~p", [Name]);
format_error({invalid_metric_type, Invalid}) -> mzb_string:format("Invalid metric type: ~p", [Invalid]);
format_error({invalid_metric_format, UnknownFormat}) -> mzb_string:format("Invalid metric format: ~p", [UnknownFormat]);
format_error({opts_not_map, Opts}) -> mzb_string:format("Metric options should be a map: ~p", [Opts]);
format_error(E) -> mzb_string:format("~p", [E]).
