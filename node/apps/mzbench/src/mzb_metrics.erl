-module(mzb_metrics).

-export([start_link/5,
         notify/2,
         get_graphite_host_and_port/1,
         get_graphite_url/1,
         get_local_values/0,
         final_trigger/0,
         get_metric_value/1,
         get_failed_asserts/0,
         build_metric_groups/1,
         extract_exometer_metrics/1,
         datapoint2str/1,
         datapoints/1]).

-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(s, {
    prefix = "undefined" :: string(),
    nodes = [],
    director_pid = undefined :: pid(),
    graphite_reporter_ref = undefined :: reference(),
    last_tick_time = undefined,
    start_time = undefined,
    stop_time = undefined,
    previous_counter_values = [],
    last_rps_calculation_time = undefined,
    asserts = [],
    active = true :: true | false
}).

-define(INTERVAL, 10000). % in ms
-define(ASSERT_ACCURACY, round(?INTERVAL * 1.5)). % in ms
%% graphite stores data-points with 10-secs resolution
%% report metrics with 5-secs interval to avoid gaps on graphs due to interval's trigger inaccuracy
-define(GRAPHITE_INTERVAL, 5000).
-define(LOCALPREFIX, "local").
-define(GLOBALPREFIX, "mzb").
-define(INTERVALNAME, report_interval).

%%%===================================================================
%%% API
%%%===================================================================

start_link(MetricsPrefix, Env, MetricGroups, Nodes, DirPid) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [MetricsPrefix, Env, MetricGroups, Nodes, DirPid], [{spawn_opt, [{priority, high}]}]).

notify({Name, counter}, Value) ->
    exometer:update_or_create([?LOCALPREFIX, Name], Value, counter, []);
notify({Name, gauge}, Value) ->
    exometer:update_or_create([?LOCALPREFIX, Name], Value, gauge, []);
notify({Name, histogram}, Value) ->
    mz_histogram:notify(Name, Value);
notify(Name, Value) ->
    notify({Name, counter}, Value).

final_trigger() ->
    gen_server:call(?MODULE, final_trigger, infinity),
    timer:sleep(?INTERVAL). % let reporters report last metric data

-spec get_failed_asserts() -> [term()].
get_failed_asserts() ->
    gen_server:call(?MODULE, get_failed_asserts).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([MetricsPrefix, Env, MetricGroups, Nodes, DirPid]) ->
    {Host, Port} = get_graphite_host_and_port(Env),
    ApiKey = proplists:get_value("graphite_api_key", Env, []),
    Asserts = mzb_asserts:init(proplists:get_value(asserts, Env, undefined)),
    {ok, GraphiteReporterRef} = init_exometer(Host, Port, ApiKey, MetricsPrefix, MetricGroups),
    erlang:send_after(?INTERVAL, self(), trigger),
    _ = [ok = mz_histogram:create(Nodes, Name) || {Name, histogram} <- extract_metrics(MetricGroups)],
    StartTime = os:timestamp(),
    _ = random:seed(StartTime),
    {ok, #s{
        prefix = MetricsPrefix,
        nodes = Nodes,
        director_pid = DirPid,
        graphite_reporter_ref = GraphiteReporterRef,
        last_tick_time = StartTime,
        start_time = StartTime,
        previous_counter_values = [],
        last_rps_calculation_time = StartTime,
        asserts = Asserts,
        active = true
        }}.

handle_call(final_trigger, _From, State) ->
    {reply, ok, tick(State#s{active = false, stop_time = os:timestamp()})};

handle_call(get_failed_asserts, _From, #s{asserts = Asserts} = State) ->
    {reply, mzb_asserts:get_failed(_Finished = true, ?ASSERT_ACCURACY, Asserts), State};

handle_call(Req, _From, State) ->
    lager:error("Unhandled call: ~p", [Req]),
    {stop, {unhandled_call, Req}, State}.

handle_cast(Msg, State) ->
    lager:error("Unhandled cast: ~p", [Msg]),
    {stop, {unhandled_cast, Msg}, State}.

handle_info(trigger, State = #s{active = false}) ->
    erlang:send_after(?INTERVAL, self(), trigger),
    {noreply, State};
handle_info(trigger, State = #s{active = true}) ->
    NewState = tick(State),
    erlang:send_after(?INTERVAL, self(), trigger),
    {noreply, NewState};
handle_info({'DOWN', GraphiteReporterRef, _, _, Reason}, 
            #s{graphite_reporter_ref = GraphiteReporterRef} = State) ->
    lager:error("[ mzb_metrics ] Graphite reporter at ~p has crashed! Reason: ~p", 
                    [GraphiteReporterRef, Reason]),
    {stop, graphite_reporter_died, State};
handle_info(Info, State) ->
    lager:error("Unhandled info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, State) ->
    _ = tick(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

tick(#s{last_tick_time = LastTick} = State) ->
    Now = os:timestamp(),
    TimeSinceTick = timer:now_diff(Now, LastTick),
    State1 = aggregate_metrics(State),
    State2 = check_assertions(TimeSinceTick, State1),
    State3 = check_signals(State2),
    State3#s{last_tick_time = Now}.

aggregate_metrics(#s{nodes = Nodes} = State) ->
    lager:info("[ metrics ] METRIC AGGREGATION:"),

    StartTime = os:timestamp(),

    Values = mzb_lists:pmap(
        fun (N) ->
            lager:info("[ metrics ] Waiting for metrics from ~p...", [N]),
            case rpc:call(N, mzb_metrics, get_local_values, []) of
                {badrpc, Reason} ->
                    lager:error("[ metrics ] Failed to request metrics from node ~p (~p)", [N, Reason]),
                    erlang:error({request_metrics_failed, N, Reason});
                Res ->
                    lager:info("[ metrics ] Received metrics from ~p", [N]),
                    Res
            end
        end, lists:usort([erlang:node()] ++ Nodes)),

    Aggregated = merge_metrics_data(Values),

    lager:info("[ metrics ] Updating metric values in exometer..."),
    lists:foreach(
        fun ({N, V, counter}) ->
                exometer:update_or_create([?GLOBALPREFIX, N], V, counter, []);
            ({N, V, gauge}) ->
                exometer:update_or_create([?GLOBALPREFIX, N], V, gauge, [])
        end, Aggregated),

    lager:info("[ metrics ] Evaluating rates..."),
    NewState = eval_rps(State),

    FinishTime = os:timestamp(),
    MergingTime = timer:now_diff(FinishTime, StartTime) / 1000,

    ok = exometer:update_or_create(
        [?GLOBALPREFIX, "metric_merging_time"],
        MergingTime,
        gauge,
        []),

    lager:info("[ metrics ] Current metrics values:~n~s", [format_global_metrics()]),
    NewState.

check_assertions(TimePeriod, #s{director_pid = DirPid, asserts = Asserts} = State) ->
    lager:info("[ metrics ] CHECK ASSERTIONS:"),
    NewAsserts = mzb_asserts:update_state(TimePeriod, Asserts),
    lager:info("Current assertions:~n~s", [mzb_asserts:format_state(NewAsserts)]),

    FailedAsserts = mzb_asserts:get_failed(_Finished = false, ?ASSERT_ACCURACY, NewAsserts),
    case FailedAsserts of
        [] -> ok;
        _  ->
            lager:error("Interrupting benchmark because of failed asserts:~n~s", [string:join([Str|| {_, Str} <- FailedAsserts], "\n")]),
            mzb_director:stop_benchmark(DirPid, {assertions_failed, FailedAsserts})
    end,
    State#s{asserts = NewAsserts}.

check_signals(#s{nodes = Nodes} = State) ->
    lager:info("[ metrics ] CHECK SIGNALS:"),
    RawSignals = mzb_lists:pmap(
        fun (N) ->
            lager:info("[ metrics ] Reading signals from ~p...", [N]),
            case rpc:call(N, mzb_signaler, get_all_signals, []) of
                {badrpc, Reason} ->
                    lager:error("[ metrics ] Failed to request signals from node ~p (~p)", [N, Reason]),
                    [];
                Res ->
                    lager:info("[ metrics ] Received signals from ~p", [N]),
                    Res
            end
        end, lists:usort([erlang:node()] ++ Nodes)),
    GroupedSignals = groupby(lists:flatten(RawSignals)),
    Signals = [{N, lists:max(Counts)} || {N, Counts} <- GroupedSignals],
    lager:info("List of currently registered signals:~n~s", [format_signals_count(Signals)]),
    State.

format_global_metrics() ->
    Metrics = exometer:find_entries([?GLOBALPREFIX]),
    Lines = lists:map(
        fun({[?GLOBALPREFIX, Name], _Type, _Status}) ->
            {ok, [{value, Value}]} = exometer:get_value([?GLOBALPREFIX, Name], value),
            io_lib:format("~s = ~p", [Name, Value])
        end,
        Metrics),
    string:join(Lines, "\n").

format_signals_count(Signals) ->
    Lines = lists:map(
        fun({Name, Count}) ->
            io_lib:format("~s = ~b", [Name, Count])
        end,
        Signals),
    string:join(Lines, "\n").

get_metric_value(Metric) ->
    Suffix = metric_name_suffix(Metric),
    DataPoint =
        try
            erlang:list_to_integer(Suffix)
        catch
            _:_ -> erlang:list_to_atom(Suffix)
        end,
    case exometer:get_value([?GLOBALPREFIX, drop_metric_suffix(Metric)], DataPoint) of
        {ok, [{_, Value}]} -> {ok, Value};
        _ -> {error, not_found}
    end.

eval_rps(#s{previous_counter_values = PreviousData, last_rps_calculation_time = LastRPSCalculationTime} = State) ->
    Now = os:timestamp(),
    TimeInterval = timer:now_diff(Now, LastRPSCalculationTime),
    case TimeInterval > 1000000 of
        false -> State;
        true ->
            Counters = [N || {[_, N], counter, _} <- exometer:find_entries([?GLOBALPREFIX])],
            NewData = lists:foldl(
                fun (Metric, Acc) ->
                    NewMetric = Metric ++ ".rps",
                    Old = proplists:get_value(NewMetric, Acc, 0),
                    {ok, [{value, New}]} = exometer:get_value([?GLOBALPREFIX, Metric], value),
                    HitsPerSecond = ((New - Old) * 1000000) / TimeInterval,
                    ok = exometer:update_or_create([?GLOBALPREFIX, NewMetric], HitsPerSecond, gauge, []),
                    lists:keystore(NewMetric, 1, Acc, {NewMetric, New})
                end, PreviousData, Counters),
            State#s{previous_counter_values = NewData, last_rps_calculation_time = Now}
    end.

merge_metrics_data(Metrics) ->
    lists:foldl(
        fun ({{Name, counter}, Vs}, Acc) ->
                [{Name, lists:sum(Vs), counter}|Acc];
            ({{Name, gauge}, [V|_]}, Acc) ->
                [{Name, V, gauge}|Acc];
            ({{Name, histogram}, DataList}, Acc) ->
                Datapoints = datapoints(histogram),
                Stats = mz_histogram:merge_histograms(DataList, Datapoints),
                [{Name ++ "." ++ datapoint2str(DP), V, gauge} || {DP, V} <- Stats] ++ Acc
        end, [], groupby([{{N,T}, V} || {N, V, T} <- lists:append(Metrics)])).

datapoint2str(DP) when is_atom(DP) -> erlang:atom_to_list(DP);
datapoint2str(DP) when is_integer(DP) -> erlang:integer_to_list(DP).

groupby(L) -> groupby(L, []).
groupby([], Res) -> Res;
groupby([{H, _}|_] = L, Res) ->
    Values = proplists:get_all_values(H, L),
    groupby(proplists:delete(H, L), [{H, Values}|Res]).

metric_name_suffix(M) ->
    lists:last(string:tokens(M, ".")).

drop_metric_suffix(M) ->
    string:join(lists:droplast(string:tokens(M, ".")), ".").

get_local_values() ->
    lager:info("[ local_metrics ] Getting local metric values on ~p... ", [node()]),
    Metrics = exometer:find_entries([?LOCALPREFIX]),
    CountersAndGauges = lists:map(
        fun({[?LOCALPREFIX, Name], Type, _Status}) ->
            {ok, [{value, Value}]} = exometer:get_value([?LOCALPREFIX, Name], value),
            {Name, Value, Type}
        end,
        Metrics),
    _ = [ ok = exometer:reset(N) || {N, counter, _} <- Metrics ],
    Histograms = [{Name, Data, histogram} || {Name, Data} <- mz_histogram:get_and_remove_raw_data()],
    lager:info("[ local_metrics ] Got ~p metrics on ~p", [erlang:length(CountersAndGauges) + erlang:length(Histograms), node()]),
    CountersAndGauges ++ Histograms.

get_graphite_host_and_port(Env) ->
    URL = proplists:get_value("graphite", Env, undefined),
    case URL of
        undefined -> {undefined, undefined};
        U -> case string:tokens(U, ":") of
                [Host] -> {Host, 2003};
                [Host, Port] -> {Host, Port}
            end
    end.

get_graphite_url(Env) ->
    URL = proplists:get_value("graphite_url", Env, undefined),
    case URL of
        undefined -> {H, _} = get_graphite_host_and_port(Env),
                     case H of
                        undefined -> undefined;
                        _ -> mzb_string:format("http://~s", [H])
                     end;
        _ -> URL
    end.

extract_metrics(Groups) ->
    [{Name, Type} || {group, _GroupName, Graphs} <- Groups,
                     {graph, GraphOpts}          <- Graphs,
                     {Name, Type, _Opts}         <- maps:get(metrics, GraphOpts, [])].

extract_exometer_metrics(Groups) ->
    MetricGroups = build_metric_groups(Groups),
    Metrics = extract_metrics(MetricGroups),
    Names = [Name || {Name, _Type} <- Metrics],
    UniqueNames = lists:usort(Names),
    case erlang:length(Names) > erlang:length(UniqueNames) of
        true -> erlang:error({doubling_metric_names, Names -- UniqueNames});
        false -> ok
    end,
    Metrics.

get_exometer_metrics({Name, counter, Opts}) ->
    RateOpts = Opts#{rps => true},
    [[{Name, counter, Opts}], [{Name ++ ".rps", gauge, RateOpts}]];

get_exometer_metrics(Metric = {_, gauge, _}) ->
    [[Metric]];
get_exometer_metrics({Name, histogram, Opts}) ->
    DPs = [datapoint2str(DP) || DP <- datapoints(histogram)],
    Suffixes = ["." ++ DP || DP <- DPs],
    [[{Name ++ S, gauge, Opts} || S <- Suffixes]].

build_metric_groups(Groups) ->
    lists:map(fun ({group, Name, Graphs}) ->
        NewGraphs = lists:flatmap(fun ({graph, Opts}) ->
            Metrics = maps:get(metrics, Opts, []),
            MetricsGroups = build_metric_graphs(Metrics),
            [{graph, Opts#{metrics => MG}} || MG <- MetricsGroups]
        end, Graphs),
        {group, Name, NewGraphs}
    end, Groups).

build_metric_graphs(Group) ->
    NotCountersAndGauges = [M || M = {_Name, T, _Opts} <- Group, T /= counter, T /= gauge],
    Policy = case NotCountersAndGauges of
        [] -> counters_and_gauges;
        _  -> all_in_one_group
    end,
    build_metric_graphs(Group, Policy).

build_metric_graphs(Group, counters_and_gauges) ->
    Counters = [get_exometer_metrics(M) || M = {_Name, counter, _Opts} <- Group],
    CounterGroups = lists:foldl(fun (Cnt, []) -> Cnt;
                                    (Cnt, Acc) -> [A ++ B || {A, B} <- lists:zip(Acc, Cnt)]
                                end, [], Counters),

    Gauges = flatten_exometer_metrics([M || M = {_Name, gauge, _Opts} <- Group]),

    case CounterGroups of
        [] -> [ Gauges ];
        _  -> [ Gauges ++ CntGr || CntGr <- CounterGroups]
    end;

build_metric_graphs(Group, all_in_one_group) ->
    [flatten_exometer_metrics(Group)].

flatten_exometer_metrics(BenchMetrics) ->
    FlattenMetrics = lists:flatten(BenchMetrics),
    lists:flatten([get_exometer_metrics(M) || M <- FlattenMetrics]).

init_exometer(GraphiteHost, GraphitePort, GraphiteApiKey, Prefix, Metrics) ->
    ExometerMetrics = extract_exometer_metrics(Metrics),

    _ = lists:map(fun({Metric, Type}) ->
            exometer:new([?GLOBALPREFIX, Metric], Type)
        end, ExometerMetrics),

    GraphiteReporterMonitorRef = case GraphiteHost of
        undefined -> undefined;
        _ ->
            Opts = [{connect_timeout, 5000},
                    {prefix, [Prefix]},
                    {host, GraphiteHost},
                    {port, GraphitePort},
                    {intervals, [{?INTERVALNAME, ?GRAPHITE_INTERVAL}]},
                    {api_key, GraphiteApiKey}],
            init_and_monitor_exometer_reporter(exometer_report_graphite, Opts, ExometerMetrics)
    end,
    Interval = [{intervals, [{?INTERVALNAME, ?INTERVAL}]}],
    ok = init_exometer_reporter(mzb_exometer_report_apiserver, Interval, ExometerMetrics),
    {ok, GraphiteReporterMonitorRef}.

init_exometer_reporter(Name, Opts, Metrics) ->
    ok = case exometer_report:add_reporter(Name, Opts) of
        ok -> ok;
        {error, already_running} -> ok;
        E -> E
    end,
    ok = subscribe_exometer(Name, Metrics),
    ok.

init_and_monitor_exometer_reporter(Name, Opts, Metrics) ->
    ok = init_exometer_reporter(Name, Opts, Metrics),
    ReporterPid = proplists:get_value(Name, exometer_report:list_reporters()),
    erlang:monitor(process, ReporterPid).

datapoints(histogram) -> [min, max, mean, 50, 75, 90, 95, 99, 999];
datapoints(counter)   -> [value];
datapoints(gauge)     -> [value].

subscribe_exometer(Reporter, Metrics) ->
    lager:info("Subscribing reporter ~p to ~p.", [Reporter, Metrics]),
    lists:foreach(fun ({Metric, Type}) ->
        exometer_report:subscribe(Reporter, [?GLOBALPREFIX, Metric], datapoints(Type), ?INTERVALNAME, [])
    end, Metrics),
    ok.
