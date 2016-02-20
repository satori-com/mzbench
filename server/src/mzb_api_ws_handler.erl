-module(mzb_api_ws_handler).

-export([init/2,
         terminate/3,
         websocket_handle/3,
         websocket_info/3]).

% export for tests
-export([normalize/1,
         apply_filter/2,
         apply_pagination/2 ]).

-define(MAX_DATA_POINTS_PER_METRIC, 360).

-record(state, {
          ref = undefined :: undefined | reference(),
          timeline_opts = undefined :: undefined | map(),
          timeline_bounds = {undefined, undefined} :: {undefined | non_neg_integer(), undefined | non_neg_integer()},
          metric_streams = #{} :: #{}
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

dispatch_info({update_bench, BenchInfo = #{id:= Id}}, State = #state{timeline_opts   = TimelineOpts,
                                                                     timeline_bounds = TimelineBounds}) ->
    BenchInfos1  = normalize([{Id, BenchInfo}]),
    BenchInfos2  = apply_filter(TimelineOpts, BenchInfos1),
    TimlineItems = apply_boundaries(TimelineBounds, BenchInfos2, fun(A, B) -> A =< B end),

    case TimlineItems of
        [Bench] ->
            Event = #{type => "UPDATE_BENCH_INFO", data => Bench},
            {reply, Event, State};
        [] ->
            {ok, State}
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

dispatch_info({notify, Severity, Msg}, State) ->
    Event = #{type => "NOTIFY",
              severity => atom_to_list(Severity),
              message => Msg},
    {reply, Event, State};

dispatch_info({'DOWN', MonRef, process, MonPid, Reason}, #state{metric_streams = Streams} = State) ->
    case lists:keyfind({MonPid, MonRef}, 2, maps:to_list(Streams)) of
        {StreamId, _} ->
            case Reason of
                normal -> ok;
                aborted -> ok;
                _ -> lager:error("Metric stream with stream_id = ~p crashed with reason: ~p", [StreamId, Reason])
            end,
            {ok, State#state{metric_streams = maps:remove(StreamId, Streams)}};
        false ->
            lager:error("Metric stream terminated with unknown stream_id", []),
            {ok, State}
    end;

dispatch_info({'DOWN', _, process, _, _}, State) ->
    {ok, State};

dispatch_info(Info, State) ->
    lager:warning("~p has received unexpected info: ~p", [?MODULE, Info]),
    {ok, State}.

dispatch_request(#{<<"cmd">> := <<"ping">>}, State) ->
    {reply, <<"pong">>, State};

dispatch_request(#{<<"cmd">> := <<"get_server_info">>}, State) ->
    Data = #{clouds => mzb_api_cloud:list_clouds()},
    {reply, #{type => "SERVER_INFO", data => Data}, State};

dispatch_request(#{<<"cmd">> := <<"get_timeline">>} = Cmd, State) ->
    BenchInfos0 = mzb_api_server:get_info(),
    BenchInfos1 = normalize(BenchInfos0),
    BenchInfos2 = apply_filter(Cmd, BenchInfos1),
    {TimelineItems, {MinId, MaxId}} = apply_pagination(Cmd, BenchInfos2),

    KV = [{next, MinId}, {prev, MaxId}],
    Pager = maps:from_list([T || T = {_K,V} <- KV, V /= undefined]),

    Event = #{
               type => "INIT_TIMELINE",
               server_date => mzb_string:iso_8601_fmt(mzb_api_bench:seconds()),
               data => TimelineItems,
               pager => Pager
             },

    {reply, Event, State#state{timeline_opts   = Cmd,
                               timeline_bounds = {MinId, MaxId}}};

dispatch_request(#{<<"cmd">> := <<"start_streaming_metric">>} = Cmd, State) ->
    #{<<"stream_id">> := StreamId, <<"bench">> := BenchId, <<"metric">> := MetricName} = Cmd,
    {ok, add_stream(StreamId, BenchId, MetricName, State)};

dispatch_request(#{<<"cmd">> := <<"stop_streaming_metric">>} = Cmd, State) ->
    #{<<"stream_id">> := StreamId} = Cmd,
    {ok, remove_stream(StreamId, State)};

dispatch_request(Cmd, State) ->
    lager:warning("~p has received unexpected info: ~p", [?MODULE, Cmd]),
    {ok, State}.

add_stream(StreamId, BenchId, MetricName, #state{metric_streams = Streams} = State) ->
    lager:info("Starting streaming metric ~p of the benchmark #~p with stream_id = ~p", [MetricName, BenchId, StreamId]),
    Self = self(),
    
    SendMetricsFun = fun ({data, Values}) -> Self ! {metric_value, StreamId, Values};
                         (batch_end)      -> Self ! {metric_batch_end, StreamId}
                     end,
    Ref = erlang:spawn_monitor(fun() -> stream_metric(BenchId, MetricName, SendMetricsFun) end),
    State#state{metric_streams = maps:put(StreamId, Ref, Streams)}.

remove_stream(StreamId, #state{metric_streams = Streams} = State) ->
    lager:info("Stoping metric stream with stream_id = ~p", [StreamId]),
    case maps:find(StreamId, Streams) of
        {ok, Ref} ->
            kill_streamer(Ref),
            State#state{metric_streams = maps:remove(StreamId, Streams)};
        error ->
            State
    end.

kill_streamer(undefined) -> ok;
kill_streamer({Pid, Ref}) ->
    erlang:demonitor(Ref, [flush]),
    erlang:exit(Pid, aborted).


%% Normalization

normalize(BenchInfos) ->
    Sorted = lists:sort(fun ({IdA, _}, {IdB, _}) ->
                                IdA >= IdB
                        end, BenchInfos),
    lists:map(fun normalize_bench/1, Sorted).

ensure_binary_tuple({A, B}) when is_binary(A) and is_atom(B) -> {A, atom_to_binary(B, utf8)};
ensure_binary_tuple({A, B}) when is_binary(A) and is_binary(B) -> {A, B}.

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
    DefaultVMArgs = application:get_env(mzbench_api, vm_args, undefined),
    BinaryEnv = lists:map(fun ensure_binary_tuple/1, Env),
    EnvMap = maps:remove(<<"mzb_script_name">>, maps:from_list(BinaryEnv)),
    EnvMap2 = if VMArgs =/= DefaultVMArgs -> maps:put(vm_args, VMArgs, EnvMap);
                  true -> EnvMap end,
    ScriptFields = #{script_body => ScriptBody,
                     script_name => ScriptName,
                     benchmark_name => BenchName,
                     nodes => Nodes,
                     cloud => Cloud,
                     env => EnvMap2},

    lists:foldl(fun (Map, Acc) -> maps:merge(Acc, Map) end,
                #{id => Id},
                [StatusFields, TimeFields, ScriptFields]).

%% Filtering

apply_filter(TimelineOpts, BenchInfos) -> 
    Query = mzb_bc:maps_get(<<"q">>, TimelineOpts, undefined),
    case Query of
        undefined -> BenchInfos;
        Q -> [Bench || Bench <- BenchInfos, is_satisfy_filter(Q, Bench)]
    end.

get_searchable_fields(BenchInfo) ->
    SearchFields = mzb_bc:maps_with([id, status, benchmark_name, script_name, start_time, finish_time], BenchInfo),
    Values = maps:values(SearchFields),
    lists:map(fun (X) when is_atom(X) -> atom_to_list(X);
                  (X) when is_integer(X) -> integer_to_list(X);
                  (X) -> X
              end, Values).

is_satisfy_filter(Query, BenchInfo) ->
    try
        QueryString = binary_to_list(Query),
        SearchFields = get_searchable_fields(BenchInfo),
        lists:any(fun(Field) ->
                      case re:run(Field, QueryString, [caseless]) of
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

%% Metrics reading process
stream_metric(Id, Metric, SendFun) ->
    #{start_time:= StartTime, finish_time:= FinishTime, config:= Config} = mzb_api_server:status(Id),
    Filename = mzb_api_bench:metrics_file(Metric, Config),
    FileReader = get_file_reader(Filename),
    try
        FinishTime2 = case FinishTime of
            undefined -> mzb_api_bench:seconds();
            T -> T
        end,
        BenchDuration = FinishTime2 - StartTime,
        SubsamplingInterval = trunc(BenchDuration/?MAX_DATA_POINTS_PER_METRIC),
        
        PollTimeout = application:get_env(mzbench_api, bench_poll_timeout, undefined),
        perform_streaming(Id, FileReader, SendFun, SubsamplingInterval, PollTimeout),
        lager:info("Streamer for #~b ~s has finished", [Id, Metric])
    after
        FileReader(close)
    end.

perform_streaming(Id, FileReader, SendFun, SubsamplingInterval, Timeout) ->
    FilteringSendFun = 
        fun({SubsamplingInterval2, LastSentValueTimestamp, CurrentMin, CurrentMax}, {data, Values}) ->
                {NewLastSentValueTimestamp, NewMin, NewMax, FilteredValues} 
                    = filter_metric_values(SubsamplingInterval2, LastSentValueTimestamp, CurrentMin, CurrentMax, Values),
                _ = SendFun({data, FilteredValues}),
                {SubsamplingInterval2, NewLastSentValueTimestamp, NewMin, NewMax};
            (StoredFilteringData, batch_end) ->
                _ = SendFun(batch_end),
                StoredFilteringData
        end,
    % 1, because if we have no data right now we have to send first bench_end anyway
    perform_streaming(Id, FileReader, FilteringSendFun, {SubsamplingInterval, undefined, undefined, undefined}, Timeout, [], 0, 1).
perform_streaming(Id, FileReader, FilteringSendFun, StoredFilteringData, Timeout, Buffer, LinesRead, LinesInBatch) ->
    case FileReader(read_line) of
        {ok, Data} when LinesRead > 2500 ->
            Buf = [Data|Buffer],
            NewStoredFilteringData = FilteringSendFun(StoredFilteringData, {data, lists:reverse(Buf)}),
            perform_streaming(Id, FileReader, FilteringSendFun, NewStoredFilteringData, Timeout, [], 0, LinesInBatch + 1);
        {ok, Data} ->
            perform_streaming(Id, FileReader, FilteringSendFun, StoredFilteringData, Timeout, [Data|Buffer], LinesRead + 1, LinesInBatch + 1);
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
                    timer:sleep(Timeout),
                    {_, LastSentValueTimestamp, CurrentMin, CurrentMax} = NewStoredFilteringData2,
                    % Reset subsampling interval to 0 to send all the data unfiltered
                    perform_streaming(Id, FileReader, FilteringSendFun, {0, LastSentValueTimestamp, CurrentMin, CurrentMax}, Timeout, [], 0, 0)
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
filter_metric_values(SubsamplingInterval, LastSentValueTimestamp, PreviousMin, PreviousMax, Values) ->
    {NewLastSentValueTimestamp, _, _, NewMin, NewMax, NewValuesReversed} = 
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
        end, {LastSentValueTimestamp, 0, 0, PreviousMin, PreviousMax, []}, Values),
    {NewLastSentValueTimestamp, NewMin, NewMax, lists:reverse(NewValuesReversed)}.

parse_value(Value) ->
    [TimeString | [ValueString | _]] = string:tokens(Value, "\t\n"),
    {list_to_integer(TimeString), mzb_string:list_to_number(ValueString)}.
