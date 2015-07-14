-module(mzb_api_metrics).

-export([get_graphite_image_links/2, get_metrics/5]).

-define(HISTOGRAM_DATAPOINTS, ["max", "min", "mean", "50", "75", "90", "95", "99", "999"]).
-define(DEVIATION_DATAPOINTS, ["50", "75", "90", "95", "99", "999"]).

get_metrics(UserName, DirNode, Host, RemoteScriptPath, RemoteEnvPath) ->
    [Res] = mzb_subprocess:remote_cmd(
              UserName,
              [Host],
              "~/mz/mz_bench/bin/metric_names.escript",
              [DirNode, RemoteScriptPath, RemoteEnvPath], mzb_api_app:default_logger(), []),
    try
        jiffy:decode(Res, [return_maps])
    catch
        C:E ->
            ST = erlang:get_stacktrace(),
            lager:error("Failed to parse metrics names cause of ~p~nOutput: ~p~nStacktrace: ~p", [E, Res, ST]),
            erlang:raise(C,E,ST)
    end.

get_graphite_image_links(MetricsMap, BenchTime) ->
    maps:fold(fun (H, MetricLists, Acc) ->
                      [make_graphite_link(H, BenchTime, M) || M <- MetricLists] ++ Acc
              end, [], MetricsMap).

make_graphite_link(Host, BenchTime, Group) ->
    {From, To} = get_graphite_from_and_to(BenchTime),
    Targets = ["target=" ++ binary_to_list(M) || M <- Group],
    lists:flatten(io_lib:format(
        "~s/render?width=800&height=500&from=~s&until=~s&~s",
        [Host, From, To, string:join(Targets, "&")])).

get_graphite_from_and_to(Seconds) when Seconds < 60 ->
    get_graphite_from_and_to(60);
get_graphite_from_and_to(Seconds) ->
    {N1, N2, N3} = os:timestamp(),
    NowTo = {N1, N2 + 60, N3},
    NowFrom =
        case (N2 - Seconds) < 0 of
            true  -> {N1 - 1, 1000000 + N2 - Seconds, N3};
            false -> {N1, N2 - Seconds, N3}
        end,
    {format_graphite_datatime(NowFrom), format_graphite_datatime(NowTo)}.

format_graphite_datatime(Now) ->
    {{Y,M,D}, {H,Mi,_}} = calendar:now_to_universal_time(Now),
    io_lib:format("~2.10.0B:~2.10.0B_~4.10.0B~2.10.0B~2.10.0B", [H, Mi, Y, M, D]).
