-module(mzb_api_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0, start_benchmarks_sup/0, start_deallocator_sup/0, start_report_sup/0]).

%% supervisor.
-export([init/1]).

%% API.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [main]).

start_benchmarks_sup() ->
    supervisor:start_link({local, benchmarks_sup}, ?MODULE, [benchmarks]).

start_deallocator_sup() ->
    supervisor:start_link({local, deallocators_sup}, ?MODULE, [deallocators]).

start_report_sup() ->
    supervisor:start_link({local, reports_sup}, ?MODULE, [reports]).

%% supervisor.

init([main]) ->
    Procs = [
        {server, {mzb_api_server, start_link, []}, permanent, 10000, worker, [mzb_api_server]},
        {benchmarks, {?MODULE, start_benchmarks_sup, []}, permanent, infinity, supervisor, [?MODULE]},
        {deallocators, {?MODULE, start_deallocator_sup, []}, permanent, infinity, supervisor, [?MODULE]},
        {reports, {?MODULE, start_report_sup, []}, permanent, infinity, supervisor, [?MODULE]}
        ],
    {ok, {{one_for_all, 10, 10}, Procs}};

init([benchmarks]) ->
    {ok, {{simple_one_for_one, 10, 10}, [{bench, {mzb_api_bench, start_link, []}, temporary, 10000, worker, [mzb_api_bench]}]}};

init([deallocators]) ->
    {ok, {{simple_one_for_one, 100, 10}, [{deallocator, {mzb_api_bench, start_deallocator, []}, transient, infinity, worker, []}]}};

init([reports]) ->
    {ok, {{simple_one_for_one, 100, 10}, [{report, {mzb_api_bench, start_report_sender, []}, temporary, infinity, worker, []}]}}.

