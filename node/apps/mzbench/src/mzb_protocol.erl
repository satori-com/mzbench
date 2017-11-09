-module(mzb_protocol).

-export([handle/2]).

handle(get_system_log_port, _) ->
    {reply, ranch:get_port(lager_tcp_server)};

handle(get_user_log_port, _) ->
    {reply, ranch:get_port(lager_tcp_server_user)};

handle({set_signaler_nodes, Nodes}, _) ->
    {reply, mzb_signaler:set_nodes(Nodes)};

handle({create_histogram, Name}, _) ->
    {reply, mzb_histigram:create(Name)};

handle({mzb_watchdog, activate}, _) ->
    {reply, mzb_watchdog:activate()};

handle({start_pool, Pool, Env, NumNodes, Offset}, _) ->
    {reply, mzb_bench_sup:start_pool(Pool, Env, NumNodes, Offset)};

handle({stop_pool, Pool}, _) ->
    {reply, mzb_pool:stop(Pool)};

handle({pool_report, PoolPid, Info, IsFinal}, _) ->
    {reply, mzb_director:pool_report(PoolPid, Info, IsFinal)};

handle({compile_env, Script, Env}, _) ->
    {reply, mzb_director:compile_and_load(Script, Env)};

handle({run_command, Pool, Percent, Command}, _) ->
    AST = mzbl_script:read_from_string("#!benchDL\n" ++ binary_to_list(Command)),
    Res = [gen_server:cast(C, {run_command, Pool, Percent, AST}) ||
            {_, C, _, M} <- supervisor:which_children(mzb_bench_sup), M == [mzb_pool]],
    {reply, Res};

handle({get_local_metrics_values, Metrics}, _) ->
    {reply, mzb_metrics:get_local_values(Metrics)};

handle(get_all_signals, _) ->
    {reply, mzb_signaler:get_all_signals()};

handle({add_signal, Name, Count}, _) ->
    {reply, mzb_signaler:add_local_signal(Name, Count)};

handle({cache_metric, Name, Value}, _) ->
    {reply, gen_server:cast(mzb_metrics_cache, {cache_metric, Name, Value})};

handle(is_director_alive, _) ->
    {reply, mzb_director:is_alive()};

handle(get_local_timestamp, _) ->
    {reply, os:timestamp()};

handle(get_system_metrics, _) ->
    {reply, mzb_system_load_monitor:metric_names()};

handle({declare_metrics, MetricGroups}, _) ->
    {reply, mzb_metrics:local_declare_metrics(MetricGroups)};

handle({user_metric_subscribe, Ref, NodeFrom, Metric}, _) ->
    Callback =
        fun (MetricName, Value) ->
            Message = {user_metric_value, Ref, {MetricName, Value}},
            mzb_interconnect:cast(NodeFrom, Message)
        end,
    {reply, mzb_metrics:local_subscribe(Metric, Callback)};

handle({user_metric_value, _Ref = Callback, {Metric, Value}}, _) ->
    Callback(Metric, Value),
    noreply;

handle(Unhandled, _) ->
    system_log:error("Unhandled node message: ~p", [Unhandled]),
    erlang:error({unknown_message, Unhandled}).

