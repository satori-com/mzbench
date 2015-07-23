-module(mzb_script_metrics).

-export([script_metrics/2, pool_metrics/1, metrics/2]).

-include_lib("mzbench_language/include/mzbl_types.hrl").

script_metrics(Pools, Nodes) ->
    Metrics = lists:usort(lists:flatmap(fun pool_metrics/1, Pools)),
    SystemLoadMetrics = mzb_system_load_monitor:metric_names(Nodes),
    WorkerCountMetrics =
        [[{"workers." ++ X, counter} || X <- ["started", "ended", "failed"]]],
    Metrics ++ SystemLoadMetrics ++ WorkerCountMetrics ++ [{"metric_merging_time", gauge}].

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
            {GraphiteUrl, mzb_metrics:build_graphite_groups(BenchName ++ ".mzb", script_metrics(Pools, Nodes))}
    end.
