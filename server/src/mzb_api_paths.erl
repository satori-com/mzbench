-module(mzb_api_paths).

-export([
    node_deployment_path/0,
    worker_deployment_path/0]).

node_deployment_path() ->
    {ok, Result} = application:get_env(mzbench_api, node_deployment_path),
    Result.

worker_deployment_path() ->
    {ok, Result} = application:get_env(mzbench_api, worker_deployment_path),
    Result.