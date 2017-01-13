-module(mzb_api_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0, start_benchmarks_sup/0]).

%% supervisor.
-export([init/1]).

%% API.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [main]).

start_benchmarks_sup() ->
    supervisor:start_link({local, benchmarks_sup}, ?MODULE, [benchmarks]).

%% supervisor.

init([main]) ->
    {ok, GCSleep} = application:get_env(mzbench_api, gc_sleep),
    Procs = [
        {firehose, {gen_event, start_link, [{local, mzb_api_firehose}]}, permanent, 10000, supervisor, []},
        {cloud_plugins, {mzb_api_cloud, start_link, []}, permanent, 10000, worker, [mzb_api_cloud]},
        {server, {mzb_api_server, start_link, []}, permanent, 10000, worker, [mzb_api_server]},
        {benchmarks, {?MODULE, start_benchmarks_sup, []}, permanent, infinity, supervisor, [?MODULE]},
        {gc, {mzb_gc, start_link, [GCSleep]}, permanent, 10000, worker, [mzb_gc]},
        {auth, {mzb_api_auth, start_link, []}, permanent, 10000, worker, [mzb_api_auth]},
        {exclusive, {mzb_api_exclusive, start_link, []}, permanent, 10000, worker, [mzb_api_exclusive]}
    ],
    {ok, {{one_for_all, 10, 10}, Procs}};

init([benchmarks]) ->
    {ok, {{simple_one_for_one, 10, 10}, [{bench, {mzb_api_bench, start_link, []}, temporary, 10000, worker, [mzb_api_bench]}]}}.
