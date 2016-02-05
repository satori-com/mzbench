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
          metrics_streamers = #{} :: #{},
          guid = undefined :: term()
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

dispatch_info({metric_batch_end, Guid, Metric}, State = #state{}) ->
    {reply, #{type => "METRIC_BATCH_END", guid => Guid, metric => Metric}, State};

dispatch_info({metric_value, Guid, Metric, Values}, State) ->
    Event = #{
              type => "METRIC_DATA",
              guid => Guid,
              metric => Metric,
              data => erlang:list_to_binary(Values)
             },
    {reply, Event, State};

dispatch_info({notify, Severity, Msg}, State) ->
    Event = #{type => "NOTIFY",
              severity => atom_to_list(Severity),
              message => Msg},
    {reply, Event, State};

dispatch_info({'DOWN', MonRef, process, MonPid, Reason}, State = #state{metrics_streamers = Streamers}) ->
    case Reason of
        aborted -> ok;
        normal -> ok;
        _ -> lager:error("Metrics reader crashed with reason: ~p", [Reason])
    end,
    case lists:keyfind({MonPid, MonRef}, 2, maps:to_list(Streamers)) of
        {Metric, _} ->
            {ok, State#state{metrics_streamers = maps:put(Metric, undefined, Streamers)}};
        false ->
            lager:error("Can't find streamer by {~p,~p}", [MonPid, MonRef]),
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

dispatch_request(#{<<"cmd">> := <<"subscribe_metrics">>, <<"guid">> := Guid} = Cmd, State = #state{guid = Guid}) ->
    #{<<"bench">> := BenchId, <<"metrics">>:= Metrics} = Cmd,
    {ok, add_streamers(BenchId, Metrics, State)};
dispatch_request(#{<<"cmd">> := <<"subscribe_metrics">>, <<"guid">> := Guid} = Cmd, State = #state{}) ->
    #{<<"bench">> := BenchId, <<"metrics">> := Metrics} = Cmd,
    State1 = kill_all_streamers(State#state{guid = Guid}),
    {ok, add_streamers(BenchId, Metrics, State1)};

dispatch_request(Cmd, State) ->
    lager:warning("~p has received unexpected info: ~p", [?MODULE, Cmd]),
    {ok, State}.

add_streamers(BenchId, Metrics, #state{metrics_streamers = Streamers, guid = Guid} = State) ->
    lager:info("Add streamers for #~p ~p", [BenchId, Metrics]),
    Self = self(),

    NewStreamers =
        lists:foldl(
            fun (Metric, Acc) ->
                case maps:find(Metric, Acc) of
                    {ok, _} -> Acc;
                    error ->
                        SendMetricsFun = fun ({data, Values}) -> Self ! {metric_value, Guid, Metric, Values};
                                             (batch_end)      -> Self ! {metric_batch_end, Guid, Metric}
                                         end,
                        R2 = erlang:spawn_monitor(fun() ->
                            stream_metric(BenchId, Metric, SendMetricsFun)
                        end),
                        maps:put(Metric, R2, Acc)
                end
            end, Streamers, Metrics),

    State#state{metrics_streamers = NewStreamers}.

kill_all_streamers(#state{metrics_streamers = Streamers} = State) ->
    lager:info("Kill all metric streamers"),
    lists:foreach(fun ({_, R}) -> kill_streamer(R) end, maps:to_list(Streamers)),
    State#state{metrics_streamers = #{}}.

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
        fun(LastSentValueTimestamp, {data, Values}) ->
                {NewLastSentValueTimestamp, FilteredValues} = filter_metric_values(SubsamplingInterval, LastSentValueTimestamp, Values),
                _ = SendFun({data, FilteredValues}),
                NewLastSentValueTimestamp;
            (LastSentValueTimestamp, batch_end) ->
                _ = SendFun(batch_end),
                LastSentValueTimestamp
        end,
    perform_streaming(Id, FileReader, FilteringSendFun, undefined, Timeout, [], 0, 1). % 1, because if we have no data right now we have to send first bench_end anyway
perform_streaming(Id, FileReader, FilteringSendFun, LastSentValueTimestamp, Timeout, Buffer, LinesRead, LinesInBatch) ->
    case FileReader(read_line) of
        {ok, Data} when LinesRead > 2500 ->
            Buf = [Data|Buffer],
            NewLastSentValueTimestamp = FilteringSendFun(LastSentValueTimestamp, {data, lists:reverse(Buf)}),
            perform_streaming(Id, FileReader, FilteringSendFun, NewLastSentValueTimestamp, Timeout, [], 0, LinesInBatch + 1);
        {ok, Data} ->
            perform_streaming(Id, FileReader, FilteringSendFun, LastSentValueTimestamp, Timeout, [Data|Buffer], LinesRead + 1, LinesInBatch + 1);
        eof ->
            NewLastSentValueTimestamp = case Buffer of
                [] -> LastSentValueTimestamp;
                _ -> FilteringSendFun(LastSentValueTimestamp, {data, lists:reverse(Buffer)})
            end,
            NewLastSentValueTimestamp2 = if 
                LinesInBatch > 0 -> FilteringSendFun(NewLastSentValueTimestamp, batch_end);
                true -> NewLastSentValueTimestamp
            end,
            case mzb_api_server:is_datastream_ended(Id) of
                true  -> ok;
                false ->
                    timer:sleep(Timeout),
                    perform_streaming(Id, FileReader, FilteringSendFun, NewLastSentValueTimestamp2, Timeout, [], 0, 0)
            end;
        {error, Reason} ->
            _ = case Buffer of
                [] -> ok;
                _ -> FilteringSendFun(LastSentValueTimestamp, {data, lists:reverse(Buffer)})
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
filter_metric_values(SubsamplingInterval, LastSentValueTimestamp, Values) ->
    {NewLastSentValueTimestamp, NewValuesReversed} = lists:foldl(fun(Value, {LastRetainedTime, Acc}) ->
        ValueTimestamp = get_value_timestamp(Value),
        case LastRetainedTime of
            undefined -> {ValueTimestamp, [Value | Acc]};
            Timestamp ->
                Interval = ValueTimestamp - Timestamp,
                case Interval < SubsamplingInterval of
                    true -> {LastRetainedTime, Acc};
                    false -> {ValueTimestamp, [Value | Acc]}
                end
        end
    end, {LastSentValueTimestamp, []}, Values),
    {NewLastSentValueTimestamp, lists:reverse(NewValuesReversed)}.

get_value_timestamp(Value) ->
    [TimeString | _] = string:tokens(Value, "\t"),
    list_to_integer(TimeString).
