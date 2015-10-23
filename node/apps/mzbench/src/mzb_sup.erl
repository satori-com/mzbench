-module(mzb_sup).

-export([start_link/0, stop_bench/0]).

-behaviour(supervisor).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop_bench() ->
    ok = supervisor:terminate_child(?MODULE, bench_sup),
    supervisor:restart_child(?MODULE, bench_sup).

init([]) ->
    {ok, LogPort} = application:get_env(mzbench, node_log_port),
    {ok, ManagementPort} = application:get_env(mzbench, node_management_port),
    {ok, {{one_for_one, 5, 60}, [
        ranch:child_spec(management_tcp_server, 10, ranch_tcp, [{port, ManagementPort}], mzb_management_tcp_protocol, []),
        ranch:child_spec(lager_tcp_server, 10, ranch_tcp, [{port, LogPort}], mzb_lager_tcp_protocol, []),

        child_spec(supervisor, metrics_event_manager, gen_event, permanent, [{local, metrics_event_manager}]),
        child_spec(worker, system_load_monitor, mzb_system_load_monitor, permanent, []),
        child_spec(supervisor, bench_sup, mzb_bench_sup, permanent, []),
        child_spec(worker, garbage_cleaner, mzb_gc, permanent, [])
    ]
    }}.

child_spec(WorkerOrSupervisor, N, I, Restart, Args) ->
    {N, {I, start_link, Args}, Restart, 5000, WorkerOrSupervisor, [I]}.
