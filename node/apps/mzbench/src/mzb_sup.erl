-module(mzb_sup).

-export([start_link/0, stop_bench/0, ensure_started/0]).

-behaviour(supervisor).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

ensure_started() ->
    [_|_] = supervisor:which_children(?MODULE),
    ok.

stop_bench() ->
    ok = supervisor:terminate_child(?MODULE, bench_sup),
    supervisor:restart_child(?MODULE, bench_sup).

init([]) ->
    {ok, LogPort} = application:get_env(mzbench, node_log_port),
    {ok, LogUserPort} = application:get_env(mzbench, node_log_user_port),
    {ok, ManagementPort} = application:get_env(mzbench, node_management_port),
    {ok, MetricUpdateIntervalMs} = application:get_env(mzbench, metric_update_interval_ms),
    {ok, InterconnectPort} = application:get_env(mzbench, node_interconnect_port),
    {ok, {{one_for_one, 5, 60}, [
        ranch:child_spec(management_tcp_server, 10, ranch_tcp, [{port, ManagementPort}], mzb_management_tcp_protocol, []),
        ranch:child_spec(lager_tcp_server, 10, ranch_tcp, [{port, LogPort}], mzb_lager_tcp_protocol, [system]),
        ranch:child_spec(lager_tcp_server_user, 10, ranch_tcp, [{port, LogUserPort}], mzb_lager_tcp_protocol, [user]),

        child_spec(worker, gauges, mzb_gauge, permanent, []),
        child_spec(worker, metrics_event_manager, gen_event, permanent, [{local, metrics_event_manager}]),
        child_spec(worker, system_load_monitor, mzb_system_load_monitor, permanent, [MetricUpdateIntervalMs]),
        child_spec(supervisor, bench_sup, mzb_bench_sup, permanent, []),
        child_spec(worker, garbage_cleaner, mzb_gc, permanent, []),
        child_spec(worker, watchdog, mzb_watchdog, permanent, []),
        child_spec(supervisor, interconnect, mzb_interconnect_sup, permanent, [InterconnectPort, fun mzb_protocol:handle/2])
    ]
    }}.

child_spec(WorkerOrSupervisor, N, I, Restart, Args) ->
    {N, {I, start_link, Args}, Restart, 5000, WorkerOrSupervisor, [I]}.
