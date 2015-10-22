-module(mzb_api_paths).

-export([
    node_deployment_path/0,
    worker_deployment_path/0,
    plugins_dir/0,
    bench_data_dir/0,
    tgz_packages_dir/0]).

node_deployment_path() ->
    {ok, Result} = application:get_env(mzbench_api, node_deployment_path),
    Result.

worker_deployment_path() ->
    {ok, Result} = application:get_env(mzbench_api, worker_deployment_path),
    Result.

plugins_dir() ->
    {ok, Result} = application:get_env(mzbench_api, plugins_dir),
    mzb_file:expand_filename(Result).

bench_data_dir() ->
    {ok, Result} = application:get_env(mzbench_api, bench_data_dir),
    mzb_file:expand_filename(Result).

tgz_packages_dir() ->
    {ok, Result} = application:get_env(mzbench_api, tgz_packages_dir),
    mzb_file:expand_filename(Result).