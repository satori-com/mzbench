-module(mzb_script_metrics).

-export([script_metrics/2, metrics/2, normalize/1, build_metric_groups_json/1]).

-include_lib("mzbench_language/include/mzbl_types.hrl").

script_metrics(Pools, Nodes) ->
    PoolMetrics = pool_metrics(Pools),

    WorkerStatusGraphs = lists:map(fun (P) ->
        {graph, #{title => mzb_string:format("Worker status (~s)", [pool_name(P)]),
                  metrics => [{mzb_string:format("workers.~s.~s", [pool_name(P), X]), counter} || X <- ["started", "ended", "failed"]] ++
                             [{mzb_string:format("workers.~s.~s.~s", [pool_name(P), mzb_utility:hostname_str(N), X]), counter} ||
                                    X <- ["started", "ended", "failed"],
                                    N <- Nodes]}}
        end, Pools),

    MZBenchInternal = [{group, "MZBench Internals",
                        WorkerStatusGraphs ++
                        [
                          {graph, #{title => "Metric merging time",
                                    units => "ms",
                                    metrics => [{"metric_merging_time", gauge}]}},
                          {graph, #{title => "Errors",
                                    metrics => [{"errors", counter}]}}
                        ]}],

    SystemLoadMetrics = mzb_system_load_monitor:metric_names(Nodes),

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
    Script = mzbl_script:read(Path, EnvFromClient),
    BenchName = mzbl_script:get_benchname(mzbl_script:get_real_script_name(EnvFromClient)),
    Nodes = [node() | nodes()],
    {Pools, Env} = mzbl_script:extract_pools_and_env(Script, EnvFromClient),

    ScriptMetrics = script_metrics(Pools, Nodes),

    MetricJson = #{ groups => build_metric_groups_json(ScriptMetrics) },

    MetricJson1 = case mzb_metrics:get_graphite_url(Env) of
        undefined -> MetricJson;
        GraphiteUrl ->
            MetricJson#{ graphite_prefix => BenchName,
                         graphite_url => GraphiteUrl }
    end,

    mzb_string:str_to_bstr(MetricJson1).

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

    DefaultGroups ++ NormalizedGroup.

normalize_group({group, Name, Graphs}) ->
    NormalizedGraphs = [normalize_graph(G) || G <- Graphs],
    {group, Name, NormalizedGraphs};
normalize_group(UnknownFormat) ->
    erlang:error({unknown_group_format, UnknownFormat}).

normalize_graph({graph, Opts}) when is_map(Opts) ->
    Metrics = maps:get(metrics, Opts, []),
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
    IsRPSGraph = ([] == [M || M = {_,_, Opts} <- Metrics, not maps:get(rps, Opts, false)]),
    case IsRPSGraph of
        true ->
            Units = case maps:get(units, GraphOpts, "") of
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
            Metrics = maps:get(metrics, GraphOpts, []),

            MetricMap = lists:flatmap(fun({Name, Type, Opts}) ->
                DPs = [mzb_metrics:datapoint2str(DP) || DP <- mzb_metrics:datapoints(Type)],
                Opts1 = maps:without([rps, worker], Opts),
                [Opts1#{name => (Name ++ "."++ S)} || S <- DPs]
            end, Metrics),

            GraphOpts1 = maybe_append_rps_units(GraphOpts, Metrics),

            GraphOpts1#{metrics => MetricMap}
        end, Graphs),
        #{name => GroupName, graphs => NewGraphs}
    end, MetricGroups).
