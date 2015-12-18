-module(mzb_api_metrics).

-export([get_metrics/5,
         extract_metric_names/1]).

get_metrics(UserName, DirNode, Host, RemoteScriptPath, RemoteEnvPath) ->
    [Res] = mzb_subprocess:remote_cmd(
              UserName,
              [Host],
              io_lib:format("~s/mzbench/bin/metric_names.escript", [mzb_api_paths:node_deployment_path()]),
              [DirNode, RemoteScriptPath, RemoteEnvPath], mzb_api_app:default_logger(), []),
    try
        jiffy:decode(Res, [return_maps])
    catch
        C:E ->
            ST = erlang:get_stacktrace(),
            lager:error("Failed to parse metrics names cause of ~p~nOutput: ~p~nStacktrace: ~p", [E, Res, ST]),
            erlang:raise(C,E,ST)
    end.

extract_metric_names(#{groups:= Groups} = _Metrics) ->
    [Name || #{graphs:= Graphs} <- Groups, #{metrics:= Metrics} <- Graphs, #{name:= Name} <- Metrics];
extract_metric_names(#{}) ->
    [].


