-module(mzb_script_metrics).

-export([script_metrics/2, pool_metrics/1, metrics/2, normalize/1, build_graphite_groups/2]).

-include_lib("mzbench_language/include/mzbl_types.hrl").

script_metrics(Pools, Nodes) ->
    PoolMetrics = lists:flatmap(fun pool_metrics/1, Pools),

    SystemLoadMetrics = mzb_system_load_monitor:metric_names(Nodes),

    MZBenchInternal = [{group, "MZBench Internals", [
                          {graph, #{title => "Worker Status",
                                    metrics => [{"workers." ++ X, counter} || X <- ["started", "ended", "failed"]]}},
                          {graph, #{title => "Metric merging time",
                                    units => "ms", 
                                    metrics => [{"metric_merging_time", gauge}]}}
                      ]}],


    normalize(PoolMetrics ++ SystemLoadMetrics ++ MZBenchInternal).

pool_metrics(Pool) ->
    #operation{name = pool, args = [PoolOpts, _Script]} = Pool,
    {Provider, Worker} = mzbl_script:extract_worker(PoolOpts),
    Provider:metrics(Worker).

metrics(Path, EnvFromClient) ->
    Script = mzbl_script:read(Path, EnvFromClient),
    BenchName = mzbl_script:get_benchname(mzbl_script:get_real_script_name(EnvFromClient)),
    Nodes = [node() | nodes()],
    {Pools, Env} = mzbl_script:extract_pools_and_env(Script, EnvFromClient),

    case mzb_metrics:get_graphite_url(Env) of
        undefined -> undefined_graphite;
        GraphiteUrl ->
            ScriptMetrics = script_metrics(Pools, Nodes),
            GraphiteGroups = build_graphite_groups(BenchName ++ ".mzb", ScriptMetrics),
            mzb_string:str_to_bstr(#{graphite_url => GraphiteUrl, groups => GraphiteGroups})
    end.


%% normalize any user metrics to inner format

normalize(Seq) ->
    {Grouped, Groupless} = lists:partition(fun (X) ->
                                                   is_tuple(X) andalso element(1, X) == group
                                           end, Seq),

    GrouplessNormalized = [normalize_graph(G) || G <- Groupless],
    DefaultGroup = {group, "Default", GrouplessNormalized},

    NormalizedGroup = [normalize_group(G) || G <- Grouped],

    [DefaultGroup | NormalizedGroup].

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

normalize_metric({Name, Type}) when is_list(Name),
                                    is_atom(Type) ->
    {Name, Type, #{}};
normalize_metric(Metric = {Name, Type, Opts}) when is_list(Name),
                                                   is_atom(Type),
                                                   is_map(Opts) ->
    Metric;
normalize_metric(UnknownFormat) ->
    erlang:error({unknown_metric_format, UnknownFormat}).

%% build graphite groups in jiffy compatible format

build_graphite_groups(Prefix, Groups) ->
    MetricGroups = mzb_metrics:build_metric_groups(Groups),

    lists:map(fun ({group, GroupName, Graphs}) ->
        NewGraphs = lists:map(fun ({graph, GraphOpts}) ->
            Metrics = maps:get(metrics, GraphOpts, []),


            MetricMap = lists:flatmap(fun({Name, Type, Opts}) ->
                DPs = [mzb_metrics:datapoint2str(DP) || DP <- mzb_metrics:datapoints(Type)],
                Opts1 = maps:without([rps], Opts),
                [Opts1#{name => (Prefix ++ "." ++ Name ++ "."++ S)} || S <- DPs]
            end, Metrics),

            % add 'rps' suffix if graph contains only rps metrics
            IsRPSGraph = ([] == [M || M = {_,_, Opts} <- Metrics, not maps:get(rps, Opts, false)]),
            GraphOpts1 = case IsRPSGraph of
                true ->
                    Units = case maps:get(units, GraphOpts, "") of
                        "" -> "rps";
                        U -> U ++ "/sec"
                    end,
                    GraphOpts#{units => Units};
                _ -> GraphOpts
            end,

            GraphOpts1#{metrics => MetricMap}
        end, Graphs),
        #{name => GroupName, graphs => NewGraphs}
    end, MetricGroups).
