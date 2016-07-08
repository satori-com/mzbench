-module(mzb_api_ws_handler).

-export([init/2,
         terminate/3,
         websocket_handle/3,
         websocket_info/3]).

% export for tests
-export([normalize/1,
         apply_filter/2,
         apply_pagination/2 ]).

-record(state, {
          ref = undefined :: undefined | reference(),
          timeline_opts = undefined :: undefined | map(),
          timeline_bounds = {undefined, undefined} :: {undefined | non_neg_integer(), undefined | non_neg_integer()},
          log_streams = #{} :: #{},
          metric_streams = #{} :: #{},
          timeline_items = [] :: [integer()],
          dashboard_items = [] :: [integer()],
          dashboard_query = undefined :: undefined | map(),
          tags = [] :: [string()]
       }).

-record(stream_parameters, {
    subsampling_interval = 0 :: non_neg_integer(),
    time_window = undefined :: undefined | non_neg_integer(),
    begin_time = undefined :: undefined | non_neg_integer(),
    end_time = undefined :: undefined | non_neg_integer(),
    stream_after_eof = true :: boolean(),
    metric_report_interval_sec = undefined :: undefined | non_neg_integer()
}).

init(Req, _Opts) ->
    Ref = erlang:make_ref(),
    ok = gen_event:add_handler(mzb_api_firehose, {mzb_api_firehose, Ref}, [self()]),
    {cowboy_websocket, Req, #state{ref = Ref}}.

terminate(_Reason, _Req, #state{ref = Ref}) ->
    gen_event:delete_handler(mzb_api_firehose, {mzb_api_firehose, Ref}, [self()]),
    ok.

websocket_handle({text, Msg}, Req, State) ->
    case dispatch_request(jiffy:decode(Msg, [return_maps]), State) of
        {reply, Reply, NewState} ->
            JsonReply = jiffy:encode(mzb_string:str_to_bstr(Reply), [force_utf8]),
            {reply, {text, JsonReply}, Req, NewState};
        {ok, NewState} ->
            {ok, Req, NewState}
    end;

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info(Message, Req, State) ->
    case dispatch_info(Message, State) of
        {reply, Reply, NewState} ->
            JsonReply = jiffy:encode(mzb_string:str_to_bstr(Reply), [force_utf8]),
            {reply, {text, JsonReply}, Req, NewState};
        {ok, NewState} ->
            {ok, Req, NewState}
    end.

dispatch_info({update_bench, _BenchInfo}, State = #state{timeline_opts = undefined}) ->
    {ok, State};

dispatch_info({update_bench, BenchInfo = #{id:= Id}}, State = #state{
                                                    timeline_items  = TimelineIds,
                                                    timeline_opts   = TimelineOpts,
                                                    timeline_bounds = TimelineBounds}) ->
    BenchInfos1  = normalize([{Id, BenchInfo}]),
    Query = mzb_bc:maps_get(<<"q">>, TimelineOpts, undefined),
    BenchInfos2  = apply_filter(Query, BenchInfos1),
    TimelineItems = apply_boundaries(TimelineBounds, BenchInfos2, fun(A, B) -> A =< B end),
    [Bench] = BenchInfos1,

    case lists:member(Id, TimelineIds) of
        true  ->
            Event = #{type => "UPDATE_BENCH_INFO", data => Bench},
            {reply, Event, State};
        false ->
            case TimelineItems of
                [_] ->
                    Event = #{type => "UPDATE_BENCH_INFO", data => Bench},
                    {reply, Event, State#state{timeline_items = [Id|TimelineIds]}};
                [] ->
                    {ok, State}
            end
    end;

dispatch_info({metric_batch_end, StreamId}, State = #state{}) ->
    {reply, #{type => "METRIC_BATCH_END", stream_id => StreamId}, State};

dispatch_info({metric_value, StreamId, Values}, State) ->
    Event = #{
              type => "METRIC_DATA",
              stream_id => StreamId,
              data => erlang:list_to_binary(Values)
             },
    {reply, Event, State};

dispatch_info({system_log, data, StreamId, Chunk}, State) ->
    Event = #{
              type => "LOG_DATA",
              stream_id => StreamId,
              data => Chunk
             },
    {reply, Event, State};

dispatch_info({user_log, data, StreamId, Chunk}, State) ->
    Event = #{
              type => "LOG_USER_DATA",
              stream_id => StreamId,
              data => Chunk
             },
    {reply, Event, State};

dispatch_info({system_log, batch_end, StreamId}, State) ->
    {reply, #{type => "LOG_BATCH_END",stream_id => StreamId}, State};

dispatch_info({user_log, batch_end, StreamId}, State) ->
    {reply, #{type => "LOG_USER_BATCH_END", stream_id => StreamId}, State};

dispatch_info({system_log, overflow, StreamId, MaxSize}, State) ->
    Event = #{
              type => "LOG_OVERFLOW",
              stream_id => StreamId,
              max_size => MaxSize
             },
    {reply, Event, State};

dispatch_info({user_log, overflow, StreamId, MaxSize}, State) ->
    Event = #{
              type => "LOG_USER_OVERFLOW",
              stream_id => StreamId,
              max_size => MaxSize
             },
    {reply, Event, State};

dispatch_info({notify, Severity, Msg}, State) ->
    Event = #{type => "NOTIFY",
              severity => atom_to_list(Severity),
              message => Msg},
    {reply, Event, State};

dispatch_info({'DOWN', MonRef, process, MonPid, Reason},
        #state{metric_streams = Streams, log_streams = LStreams} = State) ->
    CheckStream = fun(Map, Kind) ->
        case lists:keyfind({MonPid, MonRef}, 2, maps:to_list(Map)) of
            {StreamId, _} ->
                case Reason of
                    normal -> ok;
                    aborted -> ok;
                    _ -> lager:error("~p stream with stream_id = ~p crashed with reason: ~p", [Kind, StreamId, Reason])
                end,
                StreamId;
            false -> false
        end
    end,
    case CheckStream(Streams, "Metric") of
        false -> case CheckStream(LStreams, "Log") of
                    false ->
                        lager:error("Can't find streamer by {~p,~p}", [MonPid, MonRef]),
                        {ok, State};
                    StreamId -> {ok, State#state{log_streams = maps:remove(StreamId, LStreams)}}
                 end;
        StreamId -> {ok, State#state{metric_streams = maps:remove(StreamId, Streams)}}
    end;

dispatch_info({'DOWN', _, process, _, _}, State) ->
    {ok, State};

dispatch_info(Info, State) ->
    lager:warning("~p has received unexpected info: ~p", [?MODULE, Info]),
    {ok, State}.

dispatch_request(#{<<"cmd">> := <<"ping">>}, State) ->
    {reply, <<"pong">>, State};

dispatch_request(#{<<"cmd">> := <<"get_server_info">>}, State) ->
    Tags = get_all_tags(),
    Data = #{clouds => mzb_api_cloud:list_clouds(), tags => Tags},
    {reply, #{type => "SERVER_INFO", data => Data}, State#state{tags = Tags}};

dispatch_request(#{<<"cmd">> := <<"create_dashboard">>, <<"data">> := Data}, State) ->
    NewId = dets:foldl(fun ({Id, _}, Acc) -> max(Acc, Id) end, 0, dashboards) + 1,
    dets:insert(dashboards, {NewId, Data}),
    dets:sync(dashboards),
    Event = #{
               type => "DASHBOARD_CREATED",
               data => NewId
             },
    {reply, Event, State};

dispatch_request(#{<<"cmd">> := <<"update_dashboard">>, <<"data">> := Data}, State) ->
    Id = maps:get(<<"id">>, Data),
    dets:insert(dashboards, {Id, maps:remove(<<"id">>, Data)}),
    dets:sync(dashboards),
    Event = #{
               type => "NOTIFY",
               message => "Dashboard has been saved",
               severity => "success"
             },
    {reply, Event, State};

dispatch_request(#{<<"cmd">> := <<"get_dashboards">>} = Cmd, State) ->
    Boards = dets:foldl(fun ({Id, Data}, Acc) -> [maps:put(id, Id, Data)|Acc] end, [], dashboards),
    Sorted = lists:sort(fun (#{id := IdA}, #{id := IdB}) ->
                                IdA >= IdB
                        end, Boards),
    Query = mzb_bc:maps_get(<<"q">>, Cmd, undefined),
    Filtered = filter_dashboards(Sorted, Query),
    {TimelineItems, {MinId, MaxId}} = apply_pagination(Cmd, Filtered),

    KV = [{next, MinId}, {prev, MaxId}],
    Pager = maps:from_list([T || T = {_K,V} <- KV, V /= undefined]),

    Event = #{
               type => "DASHBOARDS",
               data => TimelineItems,
               pager => Pager
             },

    TimelineIds = [Id || #{id:= Id} <- TimelineItems],

    {reply, Event, State#state{dashboard_items  = TimelineIds,
                               dashboard_query = Cmd}};

dispatch_request(#{<<"cmd">> := <<"get_benchset">>} = Cmd, State) ->
    BenchInfos0 = mzb_api_server:get_info(),
    BenchInfos1 = normalize(BenchInfos0),
    Query = mzb_bc:maps_get(<<"criteria">>, Cmd, undefined),
    BenchsetId = mzb_bc:maps_get(<<"benchset_id">>, Cmd, 0),
    BenchInfos2 = apply_filter(Query, BenchInfos1),

    Sets = lists:map(fun(Chart) ->
              Kind = binary_to_list(mzb_bc:maps_get(<<"kind">>, Chart, undefined)),
              Metric = binary_to_list(mzb_bc:maps_get(<<"metric">>, Chart, undefined)),
              Size = list_to_integer(binary_to_list(mzb_bc:maps_get(<<"size">>, Chart, <<"0">>))),
              GroupEnv = binary_to_list(mzb_bc:maps_get(<<"group_env">>, Chart, undefined)),
              XEnv = binary_to_list(mzb_bc:maps_get(<<"x_env">>, Chart, undefined)),
              benchset(BenchInfos2, Metric, Kind, Size, GroupEnv, XEnv)
             end, mzb_bc:maps_get(<<"charts">>, Cmd, [])),

    Event = #{
               type => "BENCHSET",
               data => Sets,
               benchset_id => BenchsetId
             },
    {reply, Event, State};


dispatch_request(#{<<"cmd">> := <<"get_timeline">>} = Cmd, State) ->
    lager:info("Get timeline start"),
    Limit = mzb_bc:maps_get(<<"limit">>, Cmd, 10),
    MaxId = mzb_bc:maps_get(<<"max_id">>, Cmd, undefined),
    MinId = mzb_bc:maps_get(<<"min_id">>, Cmd, undefined),
    BenchId = mzb_bc:maps_get(<<"bench_id">>, Cmd, undefined),
    TimelineUid = mzb_bc:maps_get(<<"timeline_id">>, Cmd, 0),
    Query = mzb_bc:maps_get(<<"q">>, Cmd, undefined),
    Filter = fun (I) -> apply_filter(Query, normalize([I])) end,
    {TimelineItems, NewMinId, NewMaxId} = mzb_api_server:get_info(Filter, MaxId, BenchId, MinId, Limit),

    KV = [{next, NewMinId}, {prev, NewMaxId}],

    Pager = maps:from_list([T || T = {_K,V} <- KV, V /= undefined]),

    Event = #{
               type => "INIT_TIMELINE",
               server_date => mzb_string:iso_8601_fmt(mzb_api_bench:seconds()),
               data => TimelineItems,
               pager => Pager,
               total => length(TimelineItems),
               timeline_id => TimelineUid
             },

    TimelineIds = [Id || #{id:= Id} <- TimelineItems],

    lager:info("Get timeline end"),
    {reply, Event, State#state{timeline_opts   = Cmd,
                               timeline_bounds = {NewMinId, NewMaxId},
                               timeline_items  = TimelineIds}};

dispatch_request(#{<<"cmd">> := <<"get_finals">>} = Cmd, State) ->
    #{
        <<"stream_id">> := StreamId,
        <<"bench_ids">> := BenchIds,
        <<"metric">> := MetricName,
        <<"kind">> := Kind,
        <<"x_env">> := XEnv} = Cmd,
    Self = self(),
    spawn(fun() -> get_finals(Self, StreamId, BenchIds, binary_to_list(MetricName), Kind, XEnv) end),
    {ok, State};

dispatch_request(#{<<"cmd">> := <<"start_streaming_metric">>} = Cmd, State) ->
    #{
        <<"stream_id">> := StreamId, 
        <<"bench">> := BenchId, 
        <<"metric">> := MetricName, 
        <<"subsampling_interval">> := RawSubsamplingInterval,
        <<"time_window">> := RawTimeWindow,
        <<"begin_time">> := RawBeginTime,
        <<"end_time">> := RawEndTime,
        <<"stream_after_eof">> := RawStreamAfterEof} = Cmd,

    SubsamplingInterval = case RawSubsamplingInterval of
        <<"undefined">> -> 0;
        _ when is_integer(RawSubsamplingInterval) -> RawSubsamplingInterval
    end,

    TimeWindow = case RawTimeWindow of
        <<"undefined">> -> undefined;
        _ when is_integer(RawTimeWindow) -> RawTimeWindow
    end,

    BeginTime = case RawBeginTime of
        <<"undefined">> -> undefined;
        _ when is_integer(RawBeginTime) -> RawBeginTime
    end,

    EndTime = case RawEndTime of
        <<"undefined">> -> undefined;
        _ when is_integer(RawEndTime) -> RawEndTime
    end,

    StreamAfterEof = case RawStreamAfterEof of
        <<"true">> -> true;
        <<"false">> -> false
    end,

    {ok, add_stream(StreamId, BenchId, MetricName, 
                    #stream_parameters{
                        subsampling_interval = SubsamplingInterval,
                        time_window = TimeWindow,
                        begin_time = BeginTime,
                        end_time = EndTime,
                        stream_after_eof = StreamAfterEof,
                        metric_report_interval_sec = undefined
                    }, State)};

dispatch_request(#{<<"cmd">> := <<"stop_streaming_metric">>} = Cmd,
                    #state{metric_streams = Streams} = State) ->
    #{<<"stream_id">> := StreamId} = Cmd,
    {ok, State#state{metric_streams = remove_stream(StreamId, Streams)}};

dispatch_request(#{<<"cmd">> := <<"start_streaming_logs">>} = Cmd, State) ->
    #{<<"bench">> := BenchId,
      <<"stream_id">> := StreamId} = Cmd,
    {ok, add_log_stream(BenchId, StreamId, State)};

dispatch_request(#{<<"cmd">> := <<"stop_streaming_logs">>} = Cmd,
                    #state{log_streams = Streams} = State) ->
    #{<<"stream_id">> := StreamId} = Cmd,
    {ok, State#state{log_streams = remove_stream(StreamId, Streams)}};

dispatch_request(#{<<"cmd">> := <<"add_tag">>} = Cmd, #state{} = State) ->
    #{<<"bench">> := BenchId, <<"tag">> := Tag} = Cmd,
    try
        ok = mzb_api_server:add_tags(BenchId, [binary_to_list(Tag)])
    catch
        _:Exception ->
            Str =
                case Exception of
                    {ReasonAtom, ReasonStr} when is_atom(ReasonAtom) -> ReasonStr;
                    _ -> io_lib:format("~p", Exception)
                end,
            mzb_api_firehose:notify(danger, mzb_string:format("Add tag failed: ~s", [Str]))
    end,
    mzb_api_firehose:update_bench(mzb_api_server:status(BenchId)),
    {ok, State};

dispatch_request(#{<<"cmd">> := <<"remove_tag">>} = Cmd, #state{} = State) ->
    #{<<"bench">> := BenchId, <<"tag">> := Tag} = Cmd,
    try
        ok = mzb_api_server:remove_tags(BenchId, [binary_to_list(Tag)])
    catch
        _:Exception ->
            Str =
                case Exception of
                    {ReasonAtom, ReasonStr} when is_atom(ReasonAtom) -> ReasonStr;
                    _ -> io_lib:format("~p", Exception)
                end,
            mzb_api_firehose:notify(danger, mzb_string:format("Add tag failed: ~s", [Str]))
    end,
    mzb_api_firehose:update_bench(mzb_api_server:status(BenchId)),
    {ok, State};

dispatch_request(Cmd, State) ->
    lager:warning("~p has received unexpected info: ~p", [?MODULE, Cmd]),
    {ok, State}.

add_stream(StreamId, BenchId, MetricName, StreamParams, #state{metric_streams = Streams} = State) ->
    #stream_parameters{
        subsampling_interval = SubsamplingInterval, 
        time_window = TimeWindow, 
        begin_time = BeginTime, 
        end_time = EndTime, 
        stream_after_eof = StreamAfterEof
    } = StreamParams,
    lager:debug("Starting streaming metric ~p of the benchmark #~p with stream_id = ~p, subsampling_interval = ~p, 
                    begin_time = ~p, end_time = ~p, time_window = ~p, stream_after_eof = ~p", 
                    [MetricName, BenchId, StreamId, SubsamplingInterval, BeginTime, EndTime, TimeWindow, StreamAfterEof]),
    Self = self(),

    SendMetricsFun = fun ({data, Values}) -> Self ! {metric_value, StreamId, Values};
                         (batch_end)      -> Self ! {metric_batch_end, StreamId}
                     end,
    Ref = erlang:spawn_monitor(fun() -> stream_metric(BenchId, MetricName, StreamParams, SendMetricsFun) end),
    State#state{metric_streams = maps:put(StreamId, Ref, Streams)}.

add_log_stream(BenchId, StreamId, #state{log_streams = Streams} = State) ->
    lager:debug("Starting streaming logs of the benchmark #~p, stream #~p", [BenchId, StreamId]),
    Pid = self(),
    Ref = erlang:spawn_monitor(fun() -> stream_log(BenchId, StreamId, Pid) end),
    State#state{log_streams = maps:put(StreamId, Ref, Streams)}.

remove_stream(StreamId, Streams) ->
    lager:debug("Stoping stream with stream_id = ~p", [StreamId]),
    case maps:find(StreamId, Streams) of
        {ok, Ref} ->
            kill_streamer(Ref),
            maps:remove(StreamId, Streams);
        error ->
            Streams
    end.

kill_streamer(undefined) -> ok;
kill_streamer({Pid, Ref}) ->
    erlang:demonitor(Ref, [flush]),
    erlang:exit(Pid, aborted).

get_all_tags() ->
    BenchInfos = mzb_api_server:get_info(),
    lists:map(fun erlang:list_to_atom/1,
        lists:usort(lists:foldl(fun({_Id, #{config := Config}}, Acc) ->
                Acc ++ mzb_bc:maps_get(tags, Config, []) end, [], BenchInfos))).


benchset(BenchInfos, Metric, Kind, Size, GroupEnv, XEnv) ->
    benchset(BenchInfos, Metric, Kind, Size, GroupEnv, XEnv, []).

benchset(_, Metric, Kind, _, _, _, Acc) when (Metric == undefined) or (Kind == undefined) -> Acc;
benchset(_, _, _, Size, _, _, Acc) when (Size > 0) and (size(Acc) >= Size) -> Acc;
benchset([], _, _, _, _, _, Acc) -> Acc;
benchset([BenchInfo | Rest], Metric, Kind, Size, GroupEnv, XEnv, Acc) ->
    NewAcc = case has_metric(Metric, Kind, BenchInfo) of
                false -> Acc;
                    V -> #{id:= Id, start_time:= StartTime} = BenchInfo,
                        add_to_benchset(Acc, Kind, V,
                          get_bench_env(GroupEnv, BenchInfo), get_bench_env(XEnv, BenchInfo), Id, StartTime)
             end,
    benchset(Rest, Metric, Kind, Size, GroupEnv, XEnv, NewAcc).

has_metric(Metric, "compare", #{metrics:= #{groups:= Groups}}) ->
    lists:any(fun(#{graphs:= Graphs}) ->
        lists:any(fun(#{metrics:= Metrics}) ->
            lists:any(fun(#{name:= Name}) when Name == Metric -> true; (_) -> false end, Metrics)
                end, Graphs) end, Groups);
has_metric(Metric, _, #{results:= Res}) ->
    mzb_bc:maps_get(Metric, Res, false);
has_metric(_, _, _) ->
    false.

get_bench_env(EnvName, #{env:= Env}) ->
    mzb_bc:maps_get(EnvName, Env, []).

add_to_benchset(Acc, "group", Value, Name, X, Id, Time) ->
    add_to_group(Acc, Value, Name, X, Id, Time);
add_to_benchset(Acc, "regression", Value, Name, _X, Id, Time) ->
    add_to_regression(Acc, Value, Name, Id, Time);
add_to_benchset(Acc, _, _, Name, _X, Id, Time) ->
    [#{name => Name, benches => [#{id => Id, time => Time}]} | Acc].

add_to_group([], Value, Name, X, Id, Time) ->
    [#{name => Name, benches => [#{id => Id, time => Time, x => X, final => Value}]}];
add_to_group([#{name := Name, benches := B} | Rest], Value, Name, X, Id, Time) ->
    [#{name => Name, benches => [#{id => Id, time => Time, x => X, final => Value} | B]} | Rest];
add_to_group([C | Rest], Value, Name, X, Id, Time) ->
    [C | add_to_group(Rest, Value, Name, X, Id, Time)].

add_to_regression([], Value, Name, Id, Time) ->
    [#{name => Name, benches => [#{id => Id, time => Time, final => Value}]}];
add_to_regression([#{name := Name, benches := B} | Rest], Value, Name, Id, Time) ->
    [#{name => Name, benches => [#{id => Id, time => Time, final => Value} | B]} | Rest];
add_to_regression([C | Rest], Value, Name, Id, Time) ->
    [C | add_to_regression(Rest, Value, Name, Id, Time)].

get_finals(Pid, StreamId, BenchIds, MetricName, Kind, XEnv) ->
    Data = lists:map(fun(BenchId) ->
                {Timestamp, YVal} = get_last_value(BenchId, MetricName),
                XVal = if Kind == <<"group">> -> get_bench_x_var(BenchId, binary_to_list(XEnv));
                          true -> Timestamp end,
                {XVal, YVal} end, BenchIds),
    Sorted = lists:sort(fun ({A, _}, {B, _}) -> A >= B end, Data),
    Values = lists:foldl(fun({X, Y}, Acc) -> [io_lib:format("~p\t~p\t~p\t~p~n", [X, Y, Y, Y]) |Acc] end, [], Sorted),
    Pid ! {metric_value, StreamId, Values},
    Pid ! {metric_batch_end, StreamId}.

get_bench_x_var(BenchId, EnvName) ->
    #{config:= #{env:= Env}} = mzb_api_server:status(BenchId),
    Val = proplists:get_value(EnvName, Env),
    try
      mzb_string:list_to_number(Val)
    catch _:_ -> 0
    end.


get_last_value(BenchId, MetricName) ->
    Status = mzb_api_server:status(BenchId),
    Time = mzb_bc:maps_get(finish_time, Status, 0),
    {Time, case mzb_bc:maps_get(results, Status, undefined) of
      undefined -> 0;
      Proplist -> proplists:get_value(MetricName, Proplist, 0)
    end}.

%% Normalization

normalize(BenchInfos) ->
    Sorted = lists:sort(fun ({IdA, _}, {IdB, _}) ->
                                IdA >= IdB
                        end, BenchInfos),
    lists:map(fun normalize_bench/1, Sorted).

normalize_bench({Id, Status = #{config:= Config}}) ->
    StatusFields =  mzb_bc:maps_with([status, metrics], Status),

    TimeFields = maps:fold(fun (K, V, AccIn) when is_number(V) ->
                                   maps:put(K, mzb_string:iso_8601_fmt(V), AccIn);
                               (_, _, AccIn) -> AccIn
                           end,
                           #{},
                           mzb_bc:maps_with([finish_time, start_time], Status)),

    #{script:= #{body:= ScriptBody,
                 name:= ScriptName},
      benchmark_name:= BenchName,
      nodes_arg:=      Nodes,
      cloud:=          Cloud,
      vm_args:=        VMArgs,
      env:=            Env} = Config,
    Results = case mzb_bc:maps_get(results, Status, undefined) of
                undefined -> [];
                R -> R end,
    DefaultVMArgs = application:get_env(mzbench_api, vm_args, undefined),
    EnvMap = mzb_bc:maps_without(["mzb_script_name", "nodes_num", "bench_workers_dir", "bench_script_dir", "worker_hosts"], maps:from_list(Env)),
    EnvMap2 = if VMArgs =/= DefaultVMArgs -> maps:put(vm_args, VMArgs, EnvMap);
                  true -> EnvMap end,
    ScriptFields = #{script_body => ScriptBody,
                     script_name => ScriptName,
                     name => BenchName,
                     nodes => Nodes,
                     cloud => Cloud,
                     env => EnvMap2,
                     results => maps:from_list(Results),
                     tags => [erlang:list_to_atom(E) || Tags <- [mzb_bc:maps_get(tags, Config, [])], is_list(Tags), E <- Tags]
                     },

    lists:foldl(fun (Map, Acc) -> maps:merge(Acc, Map) end,
                #{id => Id},
                [StatusFields, TimeFields, ScriptFields]).

%% Filtering

filter_dashboards(List, undefined) -> List;
filter_dashboards(List, <<>>) -> List;
filter_dashboards(List, Query) ->
    QueryString = binary_to_list(Query),
    try
      lists:filter(fun(#{<<"name">> := Name}) ->
        case re:run(Name, QueryString, [caseless]) of
          {match, _} -> true;
                   _ -> false
        end end, List)
    catch _:Error ->
        lager:error("Failed to apply dashboard filter: ~p ~p~n Query: ~p -- List ~p", [Error, erlang:get_stacktrace(), Query, List]),
        []
    end.

apply_filter(Query, BenchInfos) ->
    case Query of
        undefined -> BenchInfos;
        _ ->
            Q = binary_to_list(Query),
            QTokens = string:tokens(Q, " "),
            [Bench || Bench <- BenchInfos, lists:all(fun (Token) -> is_satisfy_filter(Token, Bench) end, QTokens)]
    end.

get_searchable_fields(BenchInfo) ->
    SearchFields = mzb_bc:maps_with([id, status, benchmark_name, script_name, start_time, finish_time], BenchInfo),
    Values = maps:values(SearchFields),
    Tags = [ "#" ++ erlang:atom_to_list(T) || T <- mzb_bc:maps_get(tags, BenchInfo, [])],
    lists:map(fun (X) when is_atom(X) -> atom_to_list(X);
                  (X) when is_integer(X) -> integer_to_list(X);
                  (X) -> X
              end, Values) ++ Tags.

is_satisfy_filter(Query, BenchInfo) ->
    is_satisfy_fields(Query, BenchInfo) orelse is_satisfy_env(Query, BenchInfo).

is_satisfy_env(Query, BenchInfo) ->
    case string:tokens(Query, "=") of
        [Key, Value] ->
            Env = maps:get(env, BenchInfo),
            ActualValue = mzb_bc:maps_get(Key, Env, undefined),
            compare(Value, ActualValue);
        _ -> false
    end.

compare(VStr, V) when is_integer(V) -> (catch list_to_integer(VStr)) == V;
compare(VStr, V) when is_float(V) -> abs(mzb_utility:any_to_num(VStr) - V) =< 0.01;
compare(VStr, V) when is_atom(V) -> VStr == atom_to_list(V);
compare(VStr, V) when is_list(V) -> VStr == V;
compare(_, _) -> false.

is_satisfy_fields(Query, BenchInfo) ->
    try
        SearchFields = get_searchable_fields(BenchInfo),
        lists:any(fun(Field) ->
                      case re:run(Field, Query, [caseless]) of
                          {match, _} -> true;
                          _ -> false
                      end
                  end, SearchFields)
    catch _:Error ->
        lager:error("Failed to apply filter: ~p ~p~n Query: ~p -- BenchInfo ~p", [Error, erlang:get_stacktrace(), Query, BenchInfo]),
        false
    end.

%% Pagination

get_boundary(_, []) -> undefined;
get_boundary([H | _], [H | _]) -> undefined;
get_boundary(_, [ #{id:= Id} | _]) -> Id.

get_page_boundaries(BenchInfos, Paginated) ->
    MaxId = get_boundary(BenchInfos, Paginated),
    MinId = get_boundary(lists:reverse(BenchInfos), lists:reverse(Paginated)),
    {MinId, MaxId}.

index_of(Id, List) -> index_of(Id, List, 1).

index_of(_, [], _)  -> not_found;
index_of(Id, [#{id:= Id}|_], Index) -> Index;
index_of(Id, [_|Tl], Index) -> index_of(Id, Tl, Index+1).

apply_pagination(Pagination, BenchInfos) ->
    Limit = mzb_bc:maps_get(<<"limit">>, Pagination, 10),
    MaxId = mzb_bc:maps_get(<<"max_id">>, Pagination, undefined),
    MinId = mzb_bc:maps_get(<<"min_id">>, Pagination, undefined),
    BenchId = mzb_bc:maps_get(<<"bench_id">>, Pagination, undefined),

    Bounded = apply_boundaries({MinId, MaxId}, BenchInfos, fun(A, B) -> A < B end),
    Limited = apply_limit({BenchId, MinId, MaxId}, Limit, Bounded),

    PageBoundaries = get_page_boundaries(BenchInfos, Limited),

    {Limited, PageBoundaries}.

apply_limit({BenchId, undefined, undefined}, Limit, Bounded) when is_integer(BenchId) ->
    Bounded1 = case index_of(BenchId, Bounded) of
        not_found -> Bounded;
        X when X =< Limit -> Bounded;
        X -> lists:nthtail(X-1, Bounded)
    end,
    lists:sublist(Bounded1, Limit);
apply_limit({undefined, MinId, undefined}, Limit, Bounded) when is_integer(MinId) ->
    lists:reverse(lists:sublist(lists:reverse(Bounded), Limit));
apply_limit(_, Limit, Bounded) ->
    lists:sublist(Bounded, Limit).

apply_boundaries({MinId, MaxId}, BenchInfos, Comparator) ->
    lists:filter(fun(#{id := Id}) ->
                     IsBelowMax = undefined == MaxId orelse Comparator(Id, MaxId),
                     IsAboveMin = undefined == MinId orelse Comparator(MinId, Id),
                     IsBelowMax andalso IsAboveMin
                 end, BenchInfos).

log_file_reader(Filename, ChunkSize, none) ->
    {ok, Fd} = file:open(Filename, [read, raw, binary, read_ahead]),
    fun (close) -> file:close(Fd);
        (read) -> file:read(Fd, ChunkSize)
    end;
log_file_reader(Filename, ChunkSize, deflate) ->
    {ok, Fd} = file:open(Filename, [read, raw, binary, read_ahead]),
    Z = zlib:open(),
    zlib:inflateInit(Z),
    fun (close) -> zlib:inflateEnd(Z), zlib:close(Z), file:close(Fd);
        (read) -> case file:read(Fd, ChunkSize) of
                    {ok, Data} -> {ok, zlib:inflate(Z, Data)};
                    {error, E} -> {error, E};
                    eof -> eof
                  end
    end.

log_file_streamer(Filename, ChunkSize, Compression, Pid, StreamId,
                  MessageType, MaxSize) ->
    Reader = log_file_reader(Filename, ChunkSize, Compression),
    fun (finish) -> Reader(close);
        ({stream, StreamedBytes, overflow}) -> {0, StreamedBytes, overflow};
        ({stream, StreamedBytes, no_overflow}) when StreamedBytes > MaxSize ->
                Pid ! {MessageType, overflow, StreamId, MaxSize},
                Pid ! {MessageType, batch_end, StreamId},
                {0, StreamedBytes, overflow};
        ({stream, StreamedBytes, no_overflow}) ->
            case Reader(read) of
                {ok, Data} ->
                    Pid ! {MessageType, data, StreamId, Data},
                    {1, StreamedBytes + iolist_size(Data), no_overflow};
                eof ->
                    Pid ! {MessageType, batch_end, StreamId},
                    {0, StreamedBytes, no_overflow};
                {error, E} ->
                    lager:error("Error while reading log file: ~p", [E]),
                    {0, StreamedBytes, no_overflow}
            end
    end.

stream_log(BenchId, StreamId, Pid) ->
    #{config:= Config} = mzb_api_server:status(BenchId),
    #{log_compression:= Compression} = Config,
    ReadChunk = application:get_env(mzbench_api, bench_log_read_chunk, undefined),
    PollInterval = application:get_env(mzbench_api, bench_poll_timeout, undefined),
    MaxSize = application:get_env(mzbench_api, bench_log_max_dashboard, undefined),
    SysStreamer = log_file_streamer(mzb_api_bench:log_file(Config), ReadChunk, Compression,
                                Pid, StreamId, system_log, MaxSize),
    UserStreamer = log_file_streamer(mzb_api_bench:log_user_file(Config), ReadChunk, Compression,
                                Pid, StreamId, user_log, MaxSize),
    perform_log_streaming(BenchId, [{SysStreamer,0, no_overflow}, {UserStreamer, 0, no_overflow}],
        PollInterval).

perform_log_streaming(BenchId, Streamers, PollInterval) ->
    StreamResults = lists:map(fun({Streamer, Size, Status}) ->
        Streamer({stream, Size, Status}) end, Streamers),
    Success = lists:foldl(fun({X, _, _}, A) -> X + A end, 0, StreamResults),
    NewStreamers = lists:map(fun({{A, _, _}, {_, B, C}}) -> {A, B, C} end,
        lists:zip(Streamers, StreamResults)),
    if Success == 0 -> case mzb_api_server:is_datastream_ended(BenchId) of
                true  -> _ = lists:map(fun({Streamer, _, _}) -> Streamer(finish) end, Streamers), ok;
                false -> timer:sleep(PollInterval),
                         perform_log_streaming(BenchId, NewStreamers, PollInterval)
             end;
        true -> perform_log_streaming(BenchId, NewStreamers, PollInterval)
    end.

%% Metrics reading process
stream_metric(Id, Metric, StreamParams, SendFun) ->
    #{config:= Config} = mzb_api_server:status(Id),
    ReportIntervalMs =
        case maps:find(metric_update_interval_ms, Config) of
            {ok, Val} -> Val;
            error -> application:get_env(mzbench_api, metric_update_interval_ms, undefined)
        end,
    Filename = mzb_api_bench:metrics_file(Metric, Config),
    FileReader = get_file_reader(Filename),
    try
        PollTimeout = application:get_env(mzbench_api, bench_poll_timeout, undefined),
        perform_streaming(Id, FileReader, SendFun, StreamParams#stream_parameters{metric_report_interval_sec = ReportIntervalMs div 1000}, PollTimeout),
        lager:debug("Streamer for #~b ~s has finished", [Id, Metric])
    after
        FileReader(close)
    end.

perform_streaming(Id, FileReader, SendFun, #stream_parameters{time_window = TimeWindow, stream_after_eof = StreamAfterEof} = StreamParams, Timeout) ->
    StartDate = case TimeWindow of
        undefined -> undefined;
        _ -> mzb_api_bench:seconds() - TimeWindow
    end,

    FilteringSendFun =
        fun({LastSentValueTimestamp, CurrentSumForMean, CurrentNumValuesForMean, CurrentMin, CurrentMax}, {data, Values}) ->
                #stream_parameters{
                    subsampling_interval = SubsamplingInterval,
                    begin_time = BeginTime,
                    end_time = EndTime,
                    metric_report_interval_sec = ReportInterval} = StreamParams,

                TimeFilteredValues = case StartDate of
                    undefined ->
                        case BeginTime of
                            undefined -> Values;
                            _ -> filter_by_time(BeginTime - ReportInterval, EndTime + ReportInterval, Values)
                        end;
                    _ ->
                        CurDate = mzb_api_bench:seconds(),
                        filter_by_time(StartDate - ReportInterval, CurDate + ReportInterval, Values)
                end,

                {NewLastSentValueTimestamp, NewSumForMean, NewNumValuesForMean, NewMin, NewMax, FilteredValues} 
                    = perform_subsampling(SubsamplingInterval, LastSentValueTimestamp, CurrentSumForMean, CurrentNumValuesForMean, 
                                          CurrentMin, CurrentMax, TimeFilteredValues),
                _ = SendFun({data, FilteredValues}),

                {NewLastSentValueTimestamp, NewSumForMean, NewNumValuesForMean, NewMin, NewMax};
            (StoredFilteringData, batch_end) ->
                _ = SendFun(batch_end),
                StoredFilteringData
        end,
    % 1, because if we have no data right now we have to send first bench_end anyway
    perform_streaming(Id, FileReader, FilteringSendFun, {undefined, 0, 0, undefined, undefined}, Timeout, StreamAfterEof, [], 0, 1).

perform_streaming(Id, FileReader, FilteringSendFun, StoredFilteringData, Timeout, StreamAfterEof, Buffer, LinesRead, LinesInBatch) ->
    case FileReader(read_line) of
        {ok, Data} when LinesRead > 2500 ->
            Buf = [Data|Buffer],
            NewStoredFilteringData = FilteringSendFun(StoredFilteringData, {data, lists:reverse(Buf)}),
            perform_streaming(Id, FileReader, FilteringSendFun, NewStoredFilteringData, Timeout, StreamAfterEof, [], 0, LinesInBatch + 1);
        {ok, Data} ->
            perform_streaming(Id, FileReader, FilteringSendFun, StoredFilteringData, Timeout, StreamAfterEof, [Data|Buffer], LinesRead + 1, LinesInBatch + 1);
        eof ->
            NewStoredFilteringData = case Buffer of
                [] -> StoredFilteringData;
                _ -> FilteringSendFun(StoredFilteringData, {data, lists:reverse(Buffer)})
            end,
            NewStoredFilteringData2 = if 
                LinesInBatch > 0 -> FilteringSendFun(NewStoredFilteringData, batch_end);
                true -> NewStoredFilteringData
            end,
            case mzb_api_server:is_datastream_ended(Id) of
                true  -> ok;
                false ->
                    if StreamAfterEof ->
                            timer:sleep(Timeout),
                            perform_streaming(Id, FileReader, FilteringSendFun, NewStoredFilteringData2, Timeout, StreamAfterEof, [], 0, 0);
                        true -> ok
                    end
            end;
        {error, Reason} ->
            _ = case Buffer of
                [] -> ok;
                _ -> FilteringSendFun(StoredFilteringData, {data, lists:reverse(Buffer)})
            end,
            erlang:error(Reason)
    end.

get_file_reader(Filename) ->
    ReadAtOnce = application:get_env(mzbench_api, bench_read_at_once, undefined),
    {ok, H} = file:open(Filename, [raw, read, {read_ahead, ReadAtOnce}]),
    fun (close) -> file:close(H);
        (read_line) -> file:read_line(H)
    end.

% Metrics filtering
filter_by_time(BeginTime, EndTime, Values) ->
    lists:reverse(lists:foldl(fun(ValueString, Acc) ->
            {ValueTimestamp, _} = parse_value(ValueString),

            AfterBeginTime = BeginTime =< ValueTimestamp,
            BeforeEndTime = ValueTimestamp =< EndTime,

            case AfterBeginTime andalso BeforeEndTime of
                true -> [ValueString | Acc];
                false -> Acc
            end
        end, [], Values)).

perform_subsampling(SubsamplingInterval, LastSentValueTimestamp, PreviousSumForMean, PreviousNumValuesForMean, PreviousMin, PreviousMax, Values) ->
    {NewLastSentValueTimestamp, NewSumForMean, NewNumValuesForMean, NewMin, NewMax, NewValuesReversed} = 
        lists:foldl(fun(ValueString, {LastRetainedTime, SumForMean, NumValuesForMean, MinValue, MaxValue, Acc}) ->
            {ValueTimestamp, Value} = parse_value(ValueString),

            NewMinValue = if
                MinValue == undefined -> Value;
                Value < MinValue -> Value;
                true -> MinValue
            end,
            NewMaxValue = if
                MaxValue == undefined -> Value;
                Value > MaxValue -> Value;
                true -> MaxValue
            end,
 
            case LastRetainedTime of
                undefined -> {ValueTimestamp, 0, 0, undefined, undefined, [ 
                        io_lib:format("~p\t~p\t~p\t~p~n", 
                            [ValueTimestamp, Value, NewMinValue, NewMaxValue]) | Acc]};
                Timestamp ->
                    Interval = ValueTimestamp - Timestamp,
                    case Interval < SubsamplingInterval of
                        true -> {LastRetainedTime, SumForMean + Value, NumValuesForMean + 1, NewMinValue, NewMaxValue, Acc};
                        false -> {ValueTimestamp, 0, 0, undefined, undefined, 
                                    [io_lib:format("~p\t~p\t~p\t~p~n", 
                                        [ValueTimestamp, (SumForMean + Value)/(NumValuesForMean + 1), NewMinValue, NewMaxValue]) | Acc]}
                    end
            end
        end, {LastSentValueTimestamp, PreviousSumForMean, PreviousNumValuesForMean, PreviousMin, PreviousMax, []}, Values),
    {NewLastSentValueTimestamp, NewSumForMean, NewNumValuesForMean, NewMin, NewMax, lists:reverse(NewValuesReversed)}.

parse_value(Value) ->
    [TimeString | [ValueString | _]] = string:tokens(Value, "\t\n"),
    {list_to_integer(TimeString), mzb_string:list_to_number(ValueString)}.
