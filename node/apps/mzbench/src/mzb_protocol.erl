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

handle({get_local_metrics_values, Metrics}, _) ->
    {reply, mzb_metrics:get_local_values(Metrics)};

handle(get_all_signals, _) ->
    {reply, mzb_signaler:get_all_signals()};

handle({add_signal, Name, Count}, _) ->
    {reply, mzb_signaler:add_local_signal(Name, Count)};

handle(is_director_alive, _) ->
    {reply, mzb_director:is_alive()};

handle(update_time_offset, ReplyFun) ->
    _ = spawn(fun () ->
        ReplyFun(mzb_time:update_time_offset())
    end),
    noreply;

handle(get_local_timestamp, _) ->
    {reply, os:timestamp()};

handle(Unhandled, _) ->
    system_log:error("Unhandled node message: ~p", [Unhandled]),
    erlang:error({unknown_message, Unhandled}).
