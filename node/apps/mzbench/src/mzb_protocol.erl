-module(mzb_protocol).

-export([handle/1]).

handle(get_system_log_port) ->
    ranch:get_port(lager_tcp_server);

handle(get_user_log_port) ->
    ranch:get_port(lager_tcp_server_user);

handle({set_signaler_nodes, Nodes}) ->
    mzb_signaler:set_nodes(Nodes);

handle({create_histogram, Name}) ->
    mzb_histigram:create(Name);

handle({mzb_watchdog, activate}) ->
    mzb_watchdog:activate();

handle({start_pool, Pool, Env, NumNodes, Offset}) ->
    mzb_bench_sup:start_pool(Pool, Env, NumNodes, Offset);

handle({stop_pool, Pool}) ->
    mzb_pool:stop(Pool);

handle({pool_report, PoolPid, Info, IsFinal}) ->
    mzb_director:pool_report(PoolPid, Info, IsFinal);

handle({compile_env, Script, Env}) ->
    mzb_director:compile_and_load(Script, Env);

handle({get_local_metrics_values, Metrics}) ->
    mzb_metrics:get_local_values(Metrics);

handle(get_all_signals) ->
    mzb_signaler:get_all_signals();

handle({add_signal, Name, Count}) ->
    mzb_signaler:add_local_signal(Name, Count);

handle(is_director_alive) ->
    mzb_director:is_alive();

handle(Unhandled) ->
    system_log:error("Unhandled node message: ~p", [Unhandled]),
    erlang:error({unknown_message, Unhandled}).
