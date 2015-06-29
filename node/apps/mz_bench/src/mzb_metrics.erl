-module(mzb_metrics).

-export([start_link/5,
         notify/2,
         get_graphite_host_and_port/1,
         get_graphite_url/1,
         get_local_values/0,
         final_trigger/0,
         get_metric_value/1,
         get_failed_asserts/0,
         build_graphite_groups/2,
         build_metric_groups/1]).

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
    supervisor_pid = undefined :: pid(),
    graphite_reporter_ref = undefined :: reference(),
    last_tick_time = undefined,
    start_time = undefined,
    stop_time = undefined,
    previous_counter_values = [],
    asserts = [],
    active = true :: true | false
}).

-define(INTERVAL, 10000).
%% graphite stores data-points with 10-secs resolution
%% report metrics with 5-secs interval to avoid gaps on graphs due to interval's trigger inaccuracy
-define(GRAPHITE_INTERVAL, 5000).
-define(LOCALPREFIX, "local").
-define(GLOBALPREFIX, "mzb").
-define(INTERVALNAME, report_interval).

%%%===================================================================
%%% API
%%%===================================================================

start_link(MetricsPrefix, Env, Metrics, Nodes, SuperPid) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [MetricsPrefix, Env, Metrics, Nodes, SuperPid], [{spawn_opt, [{priority, high}]}]).

notify({Name, counter}, Value) ->
    exometer:update_or_create([?LOCALPREFIX, Name], Value, counter, []);
notify({Name, gauge}, Value) ->
    exometer:update_or_create([?LOCALPREFIX, Name], Value, gauge, []);
notify({Name, histogram}, Value) ->
    mz_histogram:notify(Name, Value);
% BC code:
notify({Name, fast_histogram, _}, Value) ->
    mz_histogram:notify(Name, Value);
notify({Name, fast_histogram}, Value) ->
    mz_histogram:notify(Name, Value);
% end of BC
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

init([MetricsPrefix, Env, Metrics, Nodes, SuperPid]) ->
    {Host, Port} = get_graphite_host_and_port(Env),
    ApiKey = proplists:get_value("graphite_api_key", Env, []),
    Asserts = mzb_asserts:init(proplists:get_value(asserts, Env, undefined)),
    {ok, GraphiteReporterRef} = init_exometer(Host, Port, ApiKey, MetricsPrefix, Metrics),
    erlang:send_after(?INTERVAL, self(), trigger),
    _ = [ok = mz_histogram:create(Nodes, Name) || {Name, histogram} <- lists:flatten(Metrics)],
    _ = random:seed(os:timestamp()),
    {ok, #s{
        prefix = MetricsPrefix,
        nodes = Nodes,
        supervisor_pid = SuperPid,
        graphite_reporter_ref = GraphiteReporterRef,
        last_tick_time = os:timestamp(),
        start_time = os:timestamp(),
        previous_counter_values = [],
        asserts = Asserts,
        active = true
        }}.

handle_call(final_trigger, _From, State) ->
    {reply, ok, tick(State#s{active = false, stop_time = os:timestamp()})};

handle_call(get_failed_asserts, _From, #s{asserts = Asserts, start_time = StartTime, stop_time = StopTime} = State) ->
    StopTime2 =
        case StopTime of
            undefined -> os:timestamp();
            _ -> StopTime
        end,
    Diff = timer:now_diff(StopTime2, StartTime),
    {reply, mzb_asserts:get_failed(Diff - ?INTERVAL * 1000, Asserts), State};

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

tick(#s{nodes = Nodes, supervisor_pid = _SuperPid, last_tick_time = LastTick, asserts = Asserts} = State) ->

    lager:info("[ metrics ] TICK"),

    Before = os:timestamp(),

    Values = mzbl_utility:pmap(
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

    lager:info("[ metrics ] Metric aggregation..."),
    Aggregated = aggregate_metrics_data(Values),

    lager:info("[ metrics ] Updating values in exometer..."),
    lists:foreach(
        fun ({N, V, counter}) ->
                exometer:update_or_create([?GLOBALPREFIX, N], V, counter, []);
            ({N, V, gauge}) ->
                exometer:update_or_create([?GLOBALPREFIX, N], V, gauge, [])
        end, Aggregated),

    Now = os:timestamp(),
    TimeSinceTick = timer:now_diff(Now, LastTick),

    lager:info("[ metrics ] Evaluating rates..."),
    NewState = eval_rps(State, TimeSinceTick),

    lager:info("[ metrics ] Checking assertions..."),
    NewAsserts = mzb_asserts:update_state(TimeSinceTick, Asserts),
    lager:info("Asserts:~n~p", [NewAsserts]),
    ok = exometer:update_or_create(
        [?GLOBALPREFIX, "metric_merging_time"],
        timer:now_diff(os:timestamp(), Before) / 1000,
        gauge,
        []),
    
    lager:info("[ metrics ] Checking signals..."),
    RawSignals = mzbl_utility:pmap(
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
    
    lager:info("[ metrics ] TICK finished~n~s", [format_global_metrics()]),
    NewState#s{last_tick_time = Now, asserts = NewAsserts}.

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

eval_rps(#s{previous_counter_values = PreviousData} = State, TimeInterval) ->
    Counters = [N || {[_, N], counter, _} <- exometer:find_entries([?GLOBALPREFIX])],
    NewData = lists:foldl(
        fun (Metric, Acc) ->
            NewMetric = Metric ++ ".rps",
            Old = proplists:get_value(NewMetric, Acc, 0),
            {ok, [{value, New}]} = exometer:get_value([?GLOBALPREFIX, Metric], value),
            HitsPerSecond = ((New - Old) * 1000000) / TimeInterval,
            _ = exometer:update_or_create([?GLOBALPREFIX, NewMetric], HitsPerSecond, gauge, []),
            lists:keystore(NewMetric, 1, Acc, {NewMetric, New})
        end, PreviousData, Counters),
    State#s{previous_counter_values = NewData}.

aggregate_metrics_data(Metrics) ->
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
                        _ -> lists:flatten(io_lib:format("http://~s", [H]))
                     end;
        _ -> URL
    end.

flatten_exometer_metrics(BenchMetrics) ->
    FlattenMetrics = lists:flatten(BenchMetrics),
    FlattenExometerMetrics = lists:flatten([get_exometer_metrics(M) || M <- FlattenMetrics]),
    Names = [N || {N, _} <- FlattenExometerMetrics],
    UniqueNames = lists:usort(Names),
    case erlang:length(Names) > erlang:length(UniqueNames) of
        true -> erlang:error({doubling_metric_names, Names -- UniqueNames});
        false -> ok
    end,
    FlattenExometerMetrics.


get_exometer_metrics({Name, counter}) ->
    [[{Name, counter}], [{Name ++ ".rps", gauge}]];
get_exometer_metrics({Name, gauge}) ->
    [[{Name, gauge}]];
get_exometer_metrics({Name, histogram}) ->
    DPs = [datapoint2str(DP) || DP <- datapoints(histogram)],
    Suffixes = ["." ++ DP || DP <- DPs],
    [[{Name ++ S, gauge} || S <- Suffixes]].

build_graphite_groups(Prefix, Metrics) ->
    Groups = lists:append([build_metric_groups(M) || M <- Metrics]),
    lists:map(fun (Group) ->
        lists:flatmap(fun({Name, Type}) ->
            DPs = [datapoint2str(DP) || DP <- datapoints(Type)],
            [Prefix ++ "." ++ Name ++ "."++ S || S <- DPs]
        end, Group)
    end, Groups).

build_metric_groups(Group) when is_tuple(Group) ->
    build_metric_groups([Group]);
build_metric_groups(Group) ->
    NotCountersAndGauges = [M || M = {_Name, T} <- Group, T /= counter, T /= gauge],
    Policy = case NotCountersAndGauges of
        [] -> counters_and_gauges;
        _  -> all_in_one_group
    end,
    build_metric_groups(Group, Policy).

build_metric_groups(Group, counters_and_gauges) ->
    Counters = [get_exometer_metrics(M) || M = {_Name, counter} <- Group],
    CounterGroups = lists:foldl(fun (Cnt, []) -> Cnt;
                                    (Cnt, Acc) -> [A ++ B || {A, B} <- lists:zip(Acc, Cnt)]
                                end, [], Counters),
    Gauges = flatten_exometer_metrics([M || M = {_Name, gauge} <- Group]),
    case CounterGroups of
        [] -> [ Gauges ];
        _  -> [ Gauges ++ CntGr || CntGr <- CounterGroups]
    end;
build_metric_groups(Group, all_in_one_group) ->
    [flatten_exometer_metrics(Group)].

init_exometer(GraphiteHost, GraphitePort, GraphiteApiKey, Prefix, Metrics) ->
    ExometerMetrics = flatten_exometer_metrics(Metrics),

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
