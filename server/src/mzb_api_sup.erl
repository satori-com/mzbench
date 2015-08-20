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
    Procs = [
        {server, {mzb_api_server, start_link, []}, permanent, 10000, worker, [mzb_api_server]},
        {firehose, {gen_event, start_link, [{local, mzb_api_firehose}]}, permanent, 10000, supervisor, []},
        {benchmarks, {?MODULE, start_benchmarks_sup, []}, permanent, infinity, supervisor, [?MODULE]}
    ],
    {ok, {{one_for_all, 10, 10}, Procs}};

init([benchmarks]) ->
    {ok, {{simple_one_for_one, 10, 10}, [{bench, {mzb_api_bench, start_link, []}, temporary, 10000, worker, [mzb_api_bench]}]}}.
