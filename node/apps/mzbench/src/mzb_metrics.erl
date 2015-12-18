-module(mzb_metrics).

-export([start_link/3,
         notify/2,
         get_value/1,
         get_local_values/1,
         final_trigger/0,
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

-type metric_type() :: counter | histogram | gauge.

-record(s, {
    nodes = [] :: [node()],
    last_tick_time = undefined :: erlang:timestamp(),
    start_time = undefined :: erlang:timestamp(),
    stop_time = undefined :: erlang:timestamp(),
    previous_counter_values = [] :: [{string(), non_neg_integer()}],
    last_rps_calculation_time = undefined :: erlang:timestamp(),
    asserts = [] :: [map()],
    active = true :: true | false,
    metrics = [] :: [{string(), metric_type(), term()}]
}).

-define(INTERVAL, 5000). % in ms
-define(ASSERT_ACCURACY, round(?INTERVAL * 1.5)). % in ms
-define(LOCALPREFIX, "local").
-define(INTERVALNAME, report_interval).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Env, MetricGroups, Nodes) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Env, MetricGroups, Nodes], [{spawn_opt, [{priority, high}]}]).

notify({Name, counter}, Value) ->
    exometer:update_or_create([?LOCALPREFIX, Name], Value, counter, []);
notify({Name, gauge}, Value) ->
    exometer:update_or_create([?LOCALPREFIX, Name], Value, gauge, []);
notify({Name, histogram}, Value) ->
    mz_histogram:notify(Name, Value);
notify(Name, Value) ->
    notify({Name, counter}, Value).

get_value(Metric) ->
    case exometer:get_value([Metric], value) of
        {ok, [{value, Value}]} -> Value;
        {error, Reason} -> erlang:error({badarg, Metric, Reason})
    end.

final_trigger() ->
    gen_server:call(?MODULE, final_trigger, infinity).

-spec get_failed_asserts() -> [term()].
get_failed_asserts() ->
    gen_server:call(?MODULE, get_failed_asserts).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Env, MetricGroups, Nodes]) ->
    Asserts = mzb_asserts:init(proplists:get_value(asserts, Env, undefined)),
    ok = init_exometer(MetricGroups),
    erlang:send_after(?INTERVAL, self(), trigger),
    _ = [ok = mz_histogram:create(Nodes, Name) || {Name, histogram, _} <- extract_metrics(MetricGroups)],
    StartTime = os:timestamp(),
    _ = random:seed(StartTime),
    {ok, #s{
        nodes = Nodes,
        last_tick_time = StartTime,
        start_time = StartTime,
        previous_counter_values = [],
        last_rps_calculation_time = StartTime,
        asserts = Asserts,
        active = true,
        metrics = extract_metrics(MetricGroups)
        }}.

handle_call(final_trigger, _From, State) ->
    NewState = tick(State#s{active = false, stop_time = os:timestamp()}),
    exometer_report:trigger_interval(mzb_exometer_report_apiserver, ?INTERVALNAME),
    timer:sleep(3000),  % Let exometer the time to finish reporting
    
    {reply, ok, NewState};

handle_call(get_failed_asserts, _From, #s{asserts = Asserts} = State) ->
    {reply, mzb_asserts:get_failed(_Finished = true, ?ASSERT_ACCURACY, Asserts), State};

handle_call(Req, _From, State) ->
    system_log:error("Unhandled call: ~p", [Req]),
    {stop, {unhandled_call, Req}, State}.

handle_cast(Msg, State) ->
    system_log:error("Unhandled cast: ~p", [Msg]),
    {stop, {unhandled_cast, Msg}, State}.

handle_info(trigger, State = #s{active = false}) ->
    erlang:send_after(?INTERVAL, self(), trigger),
    {noreply, State};
handle_info(trigger, State = #s{active = true}) ->
    NewState = tick(State),
    erlang:send_after(?INTERVAL, self(), trigger),
    {noreply, NewState};
handle_info(Info, State) ->
    system_log:error("Unhandled info: ~p", [Info]),
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
    State2 = evaluate_derived_metrics(State1),
    State3 = check_assertions(TimeSinceTick, State2),
    State4 = check_signals(State3),
    State4#s{last_tick_time = Now}.

aggregate_metrics(#s{nodes = Nodes, metrics = Metrics} = State) ->
    system_log:info("[ metrics ] METRIC AGGREGATION:"),
    StartTime = os:timestamp(),

    Values = mzb_lists:pmap(
        fun (N) ->
            system_log:info("[ metrics ] Waiting for metrics from ~p...", [N]),
            case rpc:call(N, mzb_metrics, get_local_values, [Metrics]) of
                {badrpc, Reason} ->
                    system_log:error("[ metrics ] Failed to request metrics from node ~p (~p)", [N, Reason]),
                    erlang:error({request_metrics_failed, N, Reason});
                Res ->
                    system_log:info("[ metrics ] Received metrics from ~p", [N]),
                    Res
            end
        end, lists:usort([erlang:node()] ++ Nodes)),

    Aggregated = merge_metrics_data(Values),

    system_log:info("[ metrics ] Updating metric values in exometer..."),
    lists:foreach(
        fun ({N, V, counter}) ->
                exometer:update_or_create([N], V, counter, []);
            ({N, V, gauge}) ->
                exometer:update_or_create([N], V, gauge, [])
        end, Aggregated),

    FinishTime = os:timestamp(),
    MergingTime = timer:now_diff(FinishTime, StartTime) / 1000,

    ok = exometer:update_or_create(
        ["metric_merging_time"],
        MergingTime,
        gauge,
        []),

    State.

evaluate_derived_metrics(#s{metrics = Metrics} = State) ->
    system_log:info("[ metrics ] Evaluating rates..."),
    NewState = eval_rps(State),

    system_log:info("[ metrics ] Evaluating derived metrics..."),
    DerivedMetrics = lists:filter(fun is_derived_metric/1, Metrics),
    lists:foreach(fun ({Name, derived, #{resolver:= Resolver, worker:= {Provider, Worker}}}) ->
        try Provider:apply(Resolver, [], Worker) of
            Val -> exometer:update_or_create([Name], Val, gauge, [])
        catch
            _:Reason -> system_log:error("Failed to evaluate derived metrics:~nWorker: ~p~nFunction: ~p~nReason: ~p~nStacktrace: ~p~n", [Worker, Resolver, Reason, erlang:get_stacktrace()])
        end
    end, DerivedMetrics),
    system_log:info("[ metrics ] Current metrics values:~n~s", [format_global_metrics()]),
    NewState.

check_assertions(TimePeriod, #s{asserts = Asserts} = State) ->
    system_log:info("[ metrics ] CHECK ASSERTIONS:"),
    NewAsserts = mzb_asserts:update_state(TimePeriod, Asserts),
    system_log:info("Current assertions:~n~s", [mzb_asserts:format_state(NewAsserts)]),

    FailedAsserts = mzb_asserts:get_failed(_Finished = false, ?ASSERT_ACCURACY, NewAsserts),
    case FailedAsserts of
        [] -> ok;
        _  ->
            system_log:error("Interrupting benchmark because of failed asserts:~n~s", [string:join([Str|| {_, Str} <- FailedAsserts], "\n")]),
            mzb_director:stop_benchmark({assertions_failed, FailedAsserts})
    end,
    State#s{asserts = NewAsserts}.

check_signals(#s{nodes = Nodes} = State) ->
    system_log:info("[ metrics ] CHECK SIGNALS:"),
    RawSignals = mzb_lists:pmap(
        fun (N) ->
            system_log:info("[ metrics ] Reading signals from ~p...", [N]),
            case rpc:call(N, mzb_signaler, get_all_signals, []) of
                {badrpc, Reason} ->
                    system_log:error("[ metrics ] Failed to request signals from node ~p (~p)", [N, Reason]),
                    [];
                Res ->
                    system_log:info("[ metrics ] Received signals from ~p", [N]),
                    Res
            end
        end, lists:usort([erlang:node()] ++ Nodes)),
    GroupedSignals = groupby(lists:flatten(RawSignals)),
    Signals = [{N, lists:max(Counts)} || {N, Counts} <- GroupedSignals],
    system_log:info("List of currently registered signals:~n~s", [format_signals_count(Signals)]),
    State.

format_global_metrics() ->
    Metrics = global_metrics(),
    Lines = lists:map(
        fun({[Name], _Type, _Status}) ->
            {ok, [{value, Value}]} = exometer:get_value([Name], value),
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

eval_rps(#s{previous_counter_values = PreviousData, last_rps_calculation_time = LastRPSCalculationTime} = State) ->
    Now = os:timestamp(),
    TimeInterval = timer:now_diff(Now, LastRPSCalculationTime),
    case TimeInterval > 1000000 of
        false -> State;
        true ->
            Counters = [N || {[N], counter, _} <- global_metrics()],
            NewData = lists:foldl(
                fun (Metric, Acc) ->
                    NewMetric = Metric ++ ".rps",
                    Old = proplists:get_value(NewMetric, Acc, 0),
                    {ok, [{value, New}]} = exometer:get_value([Metric], value),
                    HitsPerSecond = ((New - Old) * 1000000) / TimeInterval,
                    ok = exometer:update_or_create([NewMetric], HitsPerSecond, gauge, []),
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


get_local_values(Metrics) ->
    system_log:info("[ local_metrics ] Getting local metric values on ~p...", [node()]),
    CountersAndGauges = lists:map(
        fun ({Name, Type, _}) ->
            case exometer:get_value([?LOCALPREFIX, Name], value) of
                {ok, [{value, Value}]} when Type == counter ->
                    notify({Name, counter}, -Value),
                    {Name, Value, Type};
                {ok, [{value, Value}]}  ->
                    {Name, Value, Type};
                {error, _} -> ok
            end
        end,
        [M || {_, T, _} = M <- Metrics, (T == gauge) or (T == counter)]),
    Histograms = [{Name, Data, histogram} || {Name, Data} <- mz_histogram:get_and_remove_raw_data([N || {N, histogram, _} <- Metrics])],
    system_log:info("[ local_metrics ] Got ~p metrics on ~p", [erlang:length(CountersAndGauges) + erlang:length(Histograms), node()]),
    CountersAndGauges ++ Histograms.

extract_metrics(Groups) ->
    [{Name, Type, Opts} || {group, _GroupName, Graphs} <- Groups,
                           {graph, GraphOpts}          <- Graphs,
                           {Name, Type, Opts}          <- mzb_bc:maps_get(metrics, GraphOpts, [])].

is_derived_metric({_Name, derived, _}) -> true;
is_derived_metric({_Name, _Type,   _}) -> false.

extract_exometer_metrics(Groups) ->
    MetricGroups = build_metric_groups(Groups),
    Metrics = extract_metrics(MetricGroups),
    Names = [Name || {Name, _Type, _} <- Metrics],
    UniqueNames = lists:usort(Names),
    case erlang:length(Names) > erlang:length(UniqueNames) of
        true -> erlang:error({doubling_metric_names, Names -- UniqueNames});
        false -> ok
    end,
    Metrics.

get_exometer_metrics({Name, counter, Opts}) ->
    NewOpts =
        case maps:find(rps_visibility, Opts) of
            {ok, Val} -> maps:put(visibility, Val, maps:remove(rps_visibility, Opts));
            error -> Opts
        end,
    RateOpts = NewOpts#{rps => true},
    [[{Name, counter, Opts}], [{Name ++ ".rps", gauge, RateOpts}]];
get_exometer_metrics({Name, gauge, Opts}) ->
    [[{Name, gauge, Opts}]];
get_exometer_metrics({Name, derived, Opts}) ->
    [[{Name, gauge, Opts}]];
get_exometer_metrics({Name, histogram, Opts}) ->
    DPs = [datapoint2str(DP) || DP <- datapoints(histogram)],
    Suffixes = ["." ++ DP || DP <- DPs],
    [[{Name ++ S, gauge, Opts} || S <- Suffixes]].

build_metric_groups(Groups) ->
    lists:map(fun ({group, Name, Graphs}) ->
        NewGraphs = lists:flatmap(fun ({graph, Opts}) ->
            Metrics = mzb_bc:maps_get(metrics, Opts, []),
            MetricsGroups = build_metric_graphs(Metrics),
            [{graph, Opts#{metrics => MG}} || MG <- MetricsGroups]
        end, Graphs),
        {group, Name, NewGraphs}
    end, Groups).

build_metric_graphs(Group) ->
    NotCountersAndGauges = [M || M = {_Name, T, _Opts} <- Group, T /= counter, T /= gauge, T /= derived],
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

    Gauges = flatten_exometer_metrics([M || M = {_Name, T, _Opts} <- Group, T == gauge orelse T == derived]),

    case CounterGroups of
        [] -> [ Gauges ];
        _  -> [ Gauges ++ CntGr || CntGr <- CounterGroups]
    end;

build_metric_graphs(Group, all_in_one_group) ->
    [flatten_exometer_metrics(Group)].

flatten_exometer_metrics(BenchMetrics) ->
    FlattenMetrics = lists:flatten(BenchMetrics),
    lists:flatten([get_exometer_metrics(M) || M <- FlattenMetrics]).

init_exometer(Metrics) ->
    ExometerMetrics = extract_exometer_metrics(Metrics),

    _ = lists:map(
        fun ({Metric, Type, _Opts}) ->
            exometer:new([Metric], Type)
        end, ExometerMetrics),

    Interval = [{intervals, [{?INTERVALNAME, ?INTERVAL}]}],
    init_exometer_reporter(mzb_exometer_report_apiserver, Interval, ExometerMetrics).

init_exometer_reporter(Name, Opts, Metrics) ->
    ok = case exometer_report:add_reporter(Name, Opts) of
        ok -> ok;
        {error, already_running} -> ok;
        E -> E
    end,
    ok = subscribe_exometer(Name, Metrics),
    ok.

datapoints(histogram) -> [min, max, mean, 50, 75, 90, 95, 99, 999];
datapoints(counter)   -> [value];
datapoints(gauge)     -> [value].

subscribe_exometer(Reporter, Metrics) ->
    system_log:info("Subscribing reporter ~p to ~p.", [Reporter, Metrics]),
    lists:foreach(fun ({Metric, Type, _}) ->
        exometer_report:subscribe(Reporter, [Metric], datapoints(Type), ?INTERVALNAME, [])
    end, Metrics),
    ok.

global_metrics() ->
     [M || {[_], _, _} = M <- exometer:find_entries([])].

