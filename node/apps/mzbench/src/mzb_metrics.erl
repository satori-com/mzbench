-module(mzb_metrics).

-export([start_link/3,
         declare_metric/5,
         declare_metrics/1,
         local_declare_metrics/1,
         notify/2,
         get_value/1,
         get_by_wildcard/1,
         get_local_values/1,
         final_trigger/0,
         get_failed_asserts/0,
         build_metric_groups/1,
         extract_exometer_metrics/1,
         datapoint2str/1,
         datapoints/1,
         get_metrics/0,
         get_histogram_data/0]).

-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-record(s, {
    nodes = [] :: [node()],
    last_tick_time = undefined :: erlang:timestamp(),
    start_time = undefined :: erlang:timestamp(),
    stop_time = undefined :: erlang:timestamp(),
    previous_counter_values = [] :: [{string(), non_neg_integer()}],
    last_rps_calculation_time = undefined :: erlang:timestamp(),
    asserts = [] :: [map()],
    loop_assert_metrics = [],
    active = true :: true | false,
    metric_groups = [],
    update_interval_ms :: undefined | integer(),
    assert_accuracy_ms :: undefined | integer(),
    histograms = []
}).


%%%===================================================================
%%% API
%%%===================================================================

start_link(Asserts, LoopAssertMetrics, Nodes) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Asserts, LoopAssertMetrics, Nodes], [{spawn_opt, [{priority, high}]}]).

notify({Name, counter}, Value) ->
    mz_counter:notify(Name, Value);
notify({Name, gauge}, Value) ->
    mzb_gauge:notify(Name, Value);
notify({Name, histogram}, Value) ->
    mz_histogram:notify(Name, Value);
notify(Name, Value) ->
    notify({Name, counter}, Value).

declare_metric(Group, Title, Name, Type, Opts) ->
    declare_metrics([
            {group, Group, [
                {graph, Opts#{title => Title, metrics => [{Name, Type}]}}
            ]}
        ]).

declare_metrics(Groups) ->
    case mzb_metrics_cache:check_cached_declare(Groups) of
        true -> ok;
        false -> mzb_interconnect:call_director({declare_metrics, Groups})
    end.

local_declare_metrics(Groups) ->
    gen_server:call(?MODULE, {declare_metrics, Groups}).

get_value(Metric) ->
    try global_get(Metric)
    catch
        _:Error -> erlang:error({badarg, Metric, Error})
    end.

get_by_wildcard(Wildcard) ->
    Regexp = mzb_string:wildcard_to_regexp(Wildcard),
    ets:foldl(fun({Name, _, Value}, A) -> 
        case re:run(Name, Regexp) of
            nomatch -> A;
            _ -> [Value | A]
        end end, [], ?MODULE).

final_trigger() ->
    gen_server:call(?MODULE, final_trigger, infinity).

-spec get_failed_asserts() -> [term()].
get_failed_asserts() ->
    gen_server:call(?MODULE, get_failed_asserts).

get_metrics() ->
    gen_server:call(?MODULE, get_metrics).

get_histogram_data() ->
    gen_server:call(?MODULE, get_histogram_data).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Asserts, LoopAssertMetrics, Nodes]) ->
    {ok, UpdateIntervalMs} = application:get_env(mzbench, metric_update_interval_ms),
    _ = ets:new(?MODULE, [set, protected, named_table]),
    erlang:send_after(UpdateIntervalMs, self(), trigger),
    StartTime = os:timestamp(),
    _ = random:seed(StartTime),
    {ok, #s{
        nodes = Nodes,
        last_tick_time = StartTime,
        start_time = StartTime,
        previous_counter_values = [],
        last_rps_calculation_time = StartTime,
        asserts = mzbl_asserts:init(Asserts),
        loop_assert_metrics = lists:map(fun mzb_string:wildcard_to_regexp/1, LoopAssertMetrics),
        active = true,
        metric_groups = [],
        update_interval_ms = UpdateIntervalMs,
        assert_accuracy_ms = round(UpdateIntervalMs * 1.5)
        }}.

handle_call({declare_metrics, Groups}, _From, #s{metric_groups = OldGroups} = State) ->
    try mzb_script_metrics:normalize(Groups ++ OldGroups) of
        NewGroups ->
            mzb_metric_reporter:new_metrics(NewGroups),
            NewCounters = [N || {N, counter, _} <- extract_metrics(NewGroups)] -- [N || {N, counter, _} <- extract_metrics(OldGroups)],
            [ mzb_metrics:notify({N, counter}, 0) || N <- NewCounters],
            {reply, ok, State#s{metric_groups = NewGroups}}
    catch
        error:Error ->
            system_log:error("Metrics declaration error: ~s", [mzb_script_metrics:format_error(Error)]),
            {reply, {error, Error}, State}
    end;

handle_call(final_trigger, _From, State) ->
    NewState = tick(State#s{active = false, stop_time = os:timestamp()}),
    {reply, ok, NewState};

handle_call(get_failed_asserts, _From, #s{asserts = Asserts, assert_accuracy_ms = AccuracyMs} = State) ->
    {reply, mzbl_asserts:get_failed(_Finished = true, AccuracyMs, Asserts), State};

handle_call(get_metrics, _From, #s{metric_groups = Groups} = State) ->
    {reply, Groups, State};

handle_call(get_histogram_data, _From, #s{histograms = Histograms} = State) ->
    {reply, [{N, Bin} || {N, Ref} <- Histograms, {ok, Bin} <- [mz_histogram:export(Ref)]], State};

handle_call(Req, _From, State) ->
    system_log:error("Unhandled call: ~p", [Req]),
    {stop, {unhandled_call, Req}, State}.

handle_cast(Msg, State) ->
    system_log:error("Unhandled cast: ~p", [Msg]),
    {stop, {unhandled_cast, Msg}, State}.

handle_info(trigger, State = #s{active = false, update_interval_ms = IntervalMs}) ->
    erlang:send_after(IntervalMs, self(), trigger),
    {noreply, State};
handle_info(trigger, State = #s{active = true, update_interval_ms = IntervalMs}) ->
    NewState = tick(State),
    erlang:send_after(IntervalMs, self(), trigger),
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
    case TimeSinceTick of
        0 ->
            system_log:info("[ metrics ] Tick dropped because its timestamp is equal to the previous one.", []),
            State;
        _ ->
            State1 = aggregate_metrics(State),
            State2 = evaluate_derived_metrics(State1),
            State3 = check_assertions(TimeSinceTick, State2),
            State4 = check_signals(State3),
            State5 = check_dynamic_deadlock(State4),
            ok = report_metrics(State5),
            State5#s{last_tick_time = Now}
    end.

aggregate_metrics(#s{nodes = Nodes, metric_groups = MetricGroups, histograms = Histograms} = State) ->
    StartTime = os:timestamp(),

    Values = mzb_lists:pmap(
        fun (N) ->
            case mzb_interconnect:call(N, {get_local_metrics_values, extract_metrics(MetricGroups)}) of
                {badrpc, Reason} ->
                    system_log:error("[ metrics ] Failed to request metrics from node ~p (~p)", [N, Reason]),
                    erlang:error({request_metrics_failed, N, Reason});
                Res ->
                    Res
            end
        end, lists:usort([erlang:node()|Nodes])),

    Aggregated = merge_metrics_data(Values),

    lists:foreach(
        fun ({N, V, counter}) -> global_inc(N, counter, V);
            ({N, V, gauge})   -> global_set(N, gauge, V)
        end, Aggregated),

    NewHistograms = lists:foldl(
        fun ({{Name, histogram}, DataList}, Acc) ->
                Ref = proplists:get_value(Name, Acc),
                NewRef = mz_histogram:merge_to(Ref, DataList),
                lists:keystore(Name, 1, Acc, {Name, NewRef});
            (_, Acc) -> Acc
        end, Histograms, groupby([{{N,T}, V} || {N, V, T} <- lists:append(Values)])),

    FinishTime = os:timestamp(),
    MergingTime = timer:now_diff(FinishTime, StartTime) / 1000,
    global_set("metric_merging_time", gauge, MergingTime),

    State#s{histograms = NewHistograms}.

evaluate_derived_metrics(#s{metric_groups = MetricGroups} = State) ->
    NewState = eval_rps(State),

    DerivedMetrics = lists:filter(fun is_derived_metric/1, extract_metrics(MetricGroups)),
    lists:foreach(fun ({Name, derived, #{resolver:= Resolver, worker:= {Provider, Worker}} = Opts}) ->
        Args = mzb_bc:maps_get(resolver_args, Opts, []),
        try Provider:apply(Resolver, Args, Worker) of
            Val -> global_set(Name, gauge, Val)
        catch
            _:Reason -> system_log:error("Failed to evaluate derived metrics:~nWorker: ~p~nFunction: ~p~nReason: ~p~nStacktrace: ~p~n", [Worker, Resolver, Reason, erlang:get_stacktrace()])
        end
    end, DerivedMetrics),
    system_log:debug("[ metrics ] Current metrics values:~n~s", [format_global_metrics()]),
    NewState.

check_dynamic_deadlock(#s{} = State) ->
    Blocked = global_get("blocked.workers"),
    if Blocked == 0 -> State;
        true ->
            WorkerMetrics = [{lists:reverse(N), V} || {"workers.pool" ++ _ = N, counter, V} <- global_metrics()],
            Started = lists:sum([V || {"detrats" ++ _, V} <- WorkerMetrics]), % "started" reversed
            Ended = lists:sum([V || {"dedne" ++ _, V} <- WorkerMetrics]), % "ended" reversed
            if Blocked >= Started - Ended -> mzb_director:notify({assertions_failed, dynamic_deadlock});
                true -> ok
            end,
            State
    end.

check_assertions(TimePeriod, #s{asserts = Asserts, assert_accuracy_ms = AccuracyMs} = State) ->
    system_log:info("[ metrics ] CHECK ASSERTIONS:"),
    NewAsserts = mzbl_asserts:update_state(TimePeriod, Asserts),
    system_log:info("Current assertions:~n~s", [mzbl_asserts:format_state(NewAsserts)]),

    FailedAsserts = mzbl_asserts:get_failed(_Finished = false, AccuracyMs, NewAsserts),
    case FailedAsserts of
        [] -> ok;
        _  ->
            system_log:error("Interrupting benchmark because of failed asserts:~n~s", [string:join([Str|| {_, Str} <- FailedAsserts], "\n")]),
            mzb_director:notify({assertions_failed, FailedAsserts})
    end,
    State#s{asserts = NewAsserts}.

check_signals(#s{nodes = Nodes} = State) ->
    system_log:info("[ metrics ] CHECK SIGNALS:"),
    RawSignals = mzb_lists:pmap(
        fun (N) ->
            case mzb_interconnect:call(N, get_all_signals) of
                {badrpc, Reason} ->
                    system_log:error("[ metrics ] Failed to request signals from node ~p (~p)", [N, Reason]),
                    [];
                Res ->
                    Res
            end
        end, lists:usort([erlang:node()] ++ Nodes)),
    GroupedSignals = groupby(lists:flatten(RawSignals)),
    Signals = [{N, lists:max(Counts)} || {N, Counts} <- GroupedSignals],
    _ = [signal_to_metric(N, Value) || {N, Value} <- Signals],
    system_log:info("List of currently registered signals:~n~s", [format_signals_count(Signals)]),
    State.

signal_to_metric(Name, Value) when is_atom(Name) ->
    signal_to_metric(atom_to_list(Name), Value);
signal_to_metric(Name, Value) -> global_set(Name, gauge, Value).

format_global_metrics() ->
    Metrics = global_metrics(),
    Lines = lists:map(
        fun({Name, _Type, Value}) ->
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
            Counters = [N || {N, counter, _} <- global_metrics()],
            NewData = lists:foldl(
                fun (Metric, Acc) ->
                    NewMetric = Metric ++ ".rps",
                    Old = proplists:get_value(NewMetric, Acc, 0),
                    New = global_get(Metric),
                    HitsPerSecond = ((New - Old) * 1000000) / TimeInterval,
                    global_set(NewMetric, gauge, HitsPerSecond),
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
    MetricsData = lists:filtermap(
        fun ({Name, counter, _}) ->
                try
                    V = mz_counter:get_value(Name),
                    mz_counter:notify(Name, -V),
                    {true, {Name, V, counter}}
                catch
                    _:not_found -> false
                end;
            ({Name, gauge, _}) ->
                try
                    {true, {Name, mzb_gauge:take_value(Name), gauge}}
                catch
                    _:not_found -> false
                end;
            ({Name, histogram, _}) ->
                case mz_histogram:get_and_remove_raw_data([Name]) of
                    [{_, Data}] -> {true, {Name, Data, histogram}};
                    [] -> false
                end;
            ({_, _, _}) ->
                false
        end,
        Metrics),
    MetricsData.

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
            {MetaType, MetricsGroups} = build_metric_graphs(Metrics),
            [{graph, Opts#{metrics => MG, metatype => MetaType}} || MG <- MetricsGroups]
        end, Graphs),
        {group, Name, NewGraphs}
    end, Groups).

build_metric_graphs(Group) ->
    NotCountersAndGauges = [M || M = {_Name, T, _Opts} <- Group, T /= counter, T /= gauge, T /= derived],
    NotHistograms = [M || M = {_Name, T, _Opts} <- Group, T /= histogram],
    MetaType = case {NotCountersAndGauges, NotHistograms} of
        {[], _} -> counters_and_gauges;
        {_, []} -> histograms;
        _  -> mixed
    end,
    {MetaType, build_metric_graphs(Group, MetaType)}.

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

build_metric_graphs(Group, histograms) -> build_metric_graphs(Group, mixed);
build_metric_graphs(Group, mixed) ->
    [flatten_exometer_metrics(Group)].

flatten_exometer_metrics(BenchMetrics) ->
    FlattenMetrics = lists:flatten(BenchMetrics),
    lists:flatten([get_exometer_metrics(M) || M <- FlattenMetrics]).

report_metrics(#s{loop_assert_metrics = MetricRegexpList, nodes = Nodes}) ->
    GlobalMetrics = global_metrics(),
    GlobalFiltered = lists:filter(fun ({Nm, _, _}) -> lists:any(
        fun (X) -> case re:run(Nm, X) of
            nomatch -> false;
            _ -> true
        end end, MetricRegexpList) end, GlobalMetrics),
    [mzb_interconnect:abcast(Nodes, {cache_metric, Name, Value}) || {Name, _, Value} <- GlobalFiltered, Value /= undefined],
    [mzb_metric_reporter:report(Name, Value) || {Name, _, Value} <- GlobalMetrics, Value /= undefined],
    ok.

datapoints(histogram) -> [min, max, mean, 50, 75, 90, 95, 99, 999];
datapoints(counter)   -> [value];
datapoints(gauge)     -> [value].

global_metrics() ->
    ets:tab2list(?MODULE).

global_get(Name) ->
    case ets:lookup(?MODULE, Name) of
        [{Name, _, Value}] -> Value;
        [] -> erlang:error(not_found)
    end.

global_inc(Name, Type, Value) ->
    case ets:lookup(?MODULE, Name) of
        [{Name, Type, OldValue}] -> global_set(Name, Type, OldValue + Value);
        [] -> global_set(Name, Type, Value)
    end.

global_set(Name, Type, Value) ->
    ets:insert(?MODULE, {Name, Type, Value}).
