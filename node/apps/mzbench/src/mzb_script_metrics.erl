-module(mzb_script_metrics).

-export([script_metrics/2, metrics/2, normalize/1, build_metric_groups_json/1]).

-include_lib("mzbench_language/include/mzbl_types.hrl").

script_metrics(Pools, _WorkerNodes) ->
    PoolMetrics = pool_metrics(Pools),

    WorkerStatusGraphs = lists:map(fun (P) ->
        {graph, #{title => mzb_string:format("Worker status (~s)", [pool_name(P)]),
                  metrics => [{mzb_string:format("workers.~s.~s", [pool_name(P), X]), counter} || X <- ["started", "ended", "failed"]]
                             %++ [{mzb_string:format("workers.~s.~s.~s", [pool_name(P), mzb_utility:hostname_str(N), X]), counter} ||
                             %       X <- ["started", "ended", "failed"],
                             %       N <- WorkerNodes]
                            }}
        end, Pools),

    MZBenchInternal = [{group, "MZBench Internals",
                        WorkerStatusGraphs ++
                        [
                          {graph, #{title => "Metric merging time",
                                    units => "ms",
                                    metrics => [{"metric_merging_time", gauge}]}},
                          {graph, #{title => "Errors",
                                    metrics => [{"errors", counter}]}},
                          {graph, #{title => "Logs",
                                    metrics => [{"logs.written", counter},
                                                {"logs.dropped.mailbox_overflow", counter},
                                                {"logs.dropped.rate_limiter", counter}
                                                ]}}
                        ]}],

    SystemLoadMetrics = mzb_system_load_monitor:metric_names([node() | nodes()]),

    normalize(PoolMetrics ++ SystemLoadMetrics ++ MZBenchInternal).

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

metrics(Path, EnvFromClient) ->
    Script = mzbl_script:read(Path),
    Nodes = erlang:nodes(),
    {Pools, _} = mzbl_script:extract_pools_and_env(Script, EnvFromClient),

    ScriptMetrics = script_metrics(Pools, Nodes),

    #{ groups => build_metric_groups_json(ScriptMetrics) }.

%% normalize any user metrics to inner format

normalize(Seq) ->
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

    merge_groups(DefaultGroups ++ NormalizedGroup, []).

merge_groups([], Res) -> lists:reverse(Res);
merge_groups([{group, Name, Graphs} = G | T], Res) ->
    case lists:keytake(Name, 2, Res) of
        {value, {group, _, MoreGraphs}, Res2} ->
            merge_groups(T, [{group, Name, Graphs ++ MoreGraphs}|Res2]);
        false ->
            merge_groups(T, [G|Res])
    end.

normalize_group({group, Name, Graphs}) ->
    NormalizedGraphs = [normalize_graph(G) || G <- Graphs],
    {group, Name, NormalizedGraphs};
normalize_group(UnknownFormat) ->
    erlang:error({unknown_group_format, UnknownFormat}).

normalize_graph({graph, Opts}) when is_map(Opts) ->
    Metrics = mzb_bc:maps_get(metrics, Opts, []),
    NormalizedMetrics = [normalize_metric(M) || M <- Metrics],
    {graph, Opts#{metrics => NormalizedMetrics}};
normalize_graph(OneMetric) when is_tuple(OneMetric) ->
    {graph, #{metrics => [normalize_metric(OneMetric)]}};
normalize_graph(SeveralMetric) when is_list(SeveralMetric) ->
    NormalizedMetrics = [normalize_metric(M) || M <- SeveralMetric],
    {graph, #{metrics => NormalizedMetrics}};
normalize_graph(UnknownFormat) ->
    erlang:error({unknown_graph_format, UnknownFormat}).

normalize_metric({Name, Type}) when is_list(Name), is_list(Type) ->
    normalize_metric({Name, list_to_atom(Type)});
normalize_metric({Name, Type, Opts}) when is_list(Name), is_list(Type) ->
    {Name, list_to_atom(Type), normalize_metric_opts(Opts)};
normalize_metric({Name, Type}) when is_list(Name),
                                    is_atom(Type) ->
    {Name, Type, normalize_metric_opts(#{})};
normalize_metric({Name, Type, Opts}) when is_list(Name),
                                                   is_atom(Type),
                                                   is_map(Opts) ->
    {Name, Type, normalize_metric_opts(Opts)};
normalize_metric(UnknownFormat) ->
    erlang:error({unknown_metric_format, UnknownFormat}).

normalize_metric_opts(#{} = Opts) ->
    maps:from_list(lists:map(
        fun ({K, V}) when is_list(K) -> {list_to_atom(K), V};
            ({K, V}) when is_atom(K) -> {K, V}
        end, maps:to_list(Opts))).

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
            Metrics = mzb_bc:maps_get(metrics, GraphOpts, []),

            MetricMap = lists:flatmap(fun({Name, _Type, Opts}) ->
                Opts1 = mzb_bc:maps_without([rps, worker], Opts),
                [Opts1#{name => Name}]
            end, Metrics),

            GraphOpts1 = maybe_append_rps_units(GraphOpts, Metrics),

            GraphOpts1#{metrics => MetricMap}
        end, Graphs),
        #{name => GroupName, graphs => NewGraphs}
    end, MetricGroups).
