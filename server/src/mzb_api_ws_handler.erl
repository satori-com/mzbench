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
          currently_selected_bench = undefined :: undefined | non_neg_integer(),
          timeline_opts = undefined :: undefined | map(),
          timeline_bounds = {undefined, undefined} :: {undefined | non_neg_integer(), undefined | non_neg_integer()},
          metrics_reader_ref = undefined :: undefined | {pid(), reference()}
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

dispatch_info(metrics_batch_finished, State = #state{currently_selected_bench = Id}) ->
    {reply, #{type => "METRICS_BATCH_FINISHED", bench => Id}, State};

dispatch_info({transmit_metrics, BenchId, Values}, State = #state{currently_selected_bench = Id}) ->
    case Id of
        BenchId ->
            Event = #{type => "METRICS_UPDATE", bench => BenchId, data => erlang:list_to_binary(Values)},
            {reply, Event, State};
        _ ->
            {ok, State}
    end;

dispatch_info({notify, Severity, Msg}, State) ->
    Event = #{type => "NOTIFY",
              severity => atom_to_list(Severity),
              message => Msg},
    {reply, Event, State};

dispatch_info({'DOWN', MonRef, process, MonPid, Reason}, State = #state{metrics_reader_ref = {MonPid, MonRef}}) ->
    case Reason of
        aborted -> ok;
        normal -> ok;
        _ -> lager:error("Metrics reader crashed with reason: ~p", [Reason])
    end,
    {ok, State#state{metrics_reader_ref = undefined}};

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

dispatch_request(#{<<"cmd">> := <<"set_bench_for_metrics_updates">>} = Cmd, State = #state{metrics_reader_ref = MetricsReaderRef}) ->
    stop_reading_metrics(MetricsReaderRef),
    #{<<"bench">> := Id} = Cmd,
    Self = self(),
    NewMetricsReaderRef = start_reading_metrics(Id, fun () -> Self ! metrics_batch_finished end),
    {ok, State#state{currently_selected_bench = Id, metrics_reader_ref = NewMetricsReaderRef}};

dispatch_request(Cmd, State) ->
    lager:warning("~p has received unexpected info: ~p", [?MODULE, Cmd]),
    {ok, State}.

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
      env:=            Env} = Config,
    BinaryEnv = lists:map(fun ensure_binary_tuple/1, Env),
    EnvStr = erlang:iolist_to_binary(lists:flatten(
      [[K, <<"=">>, V, <<",">>] || {K, V} <- BinaryEnv, K =/= <<"mzb_script_name">>])),
    ScriptFields = #{script_body => ScriptBody,
                     script_name => ScriptName,
                     benchmark_name => BenchName,
                     nodes => Nodes,
                     cloud => Cloud,
                     env => EnvStr},

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
start_reading_metrics(BenchId, BatchFinishedCallback) ->
    erlang:spawn_monitor(fun() -> read_metrics_from_storage(BenchId, BatchFinishedCallback) end).

stop_reading_metrics(undefined) -> ok;
stop_reading_metrics({Pid, Ref}) ->
    erlang:demonitor(Ref),
    erlang:exit(Pid, aborted).

read_metrics_from_storage(BenchId, BatchFinishedCallback) ->
    #{config:= Config} = mzb_api_server:status(BenchId),
    #{metrics_compression:= Compression} = Config,
    Filename = mzb_api_bench:metrics_file(Config),

    FileReader = get_file_reader(Filename, Compression),
    try
        PollTimeout = application:get_env(mzbench_api, bench_poll_timeout, undefined),
        perform_reading(BenchId, FileReader, BatchFinishedCallback, PollTimeout)
    after
        FileReader(close)
    end.

perform_reading(BenchId, FileReader, BatchFinishedCallback, Timeout) ->
    perform_reading(BenchId, FileReader, BatchFinishedCallback, Timeout, "", 0).
perform_reading(BenchId, FileReader, BatchFinishedCallback, Timeout, Buffer, LinesRead) ->
    case FileReader(read_line) of
        {ok, Data} when LinesRead > 50 ->
            mzb_api_firehose:transmit_metrics(BenchId, string:concat(Buffer, Data)),
            perform_reading(BenchId, FileReader, BatchFinishedCallback, Timeout, "", 0);
        {ok, Data} ->
            perform_reading(BenchId, FileReader, BatchFinishedCallback, Timeout, string:concat(Buffer, Data), LinesRead + 1);
        eof ->
            case Buffer of
                "" -> ok;
                _ -> mzb_api_firehose:transmit_metrics(BenchId, Buffer)
            end,
            BatchFinishedCallback(),
            case mzb_api_server:is_datastream_ended(BenchId) of
                true  -> ok;
                false ->
                    timer:sleep(Timeout),
                    perform_reading(BenchId, FileReader, BatchFinishedCallback, Timeout, "", 0)
            end;
        {error, Reason} ->
            case Buffer of
                "" -> ok;
                _ -> mzb_api_firehose:transmit_metrics(BenchId, Buffer)
            end,
            erlang:error(Reason)
    end.

get_file_reader(Filename, none) ->
    ReadAtOnce = application:get_env(mzbench_api, bench_read_at_once, undefined),
    {ok, H} = file:open(Filename, [raw, read, {read_ahead, ReadAtOnce}]),
    fun (close) -> file:close(H);
        (read_line) -> file:read_line(H)
    end;
get_file_reader(Filename, deflate) ->
    P = erlang:spawn_link(fun () -> uncompressing_process(self(), Filename) end),
    fun (close) ->
            Ref = erlang:monitor(process, P),
            P ! close,
            receive
                {'DOWN', Ref, _, _, _} -> ok
            end;
        (read_line) ->
            P ! read_line,
            receive
                Response -> Response
            end
    end.

uncompressing_process(ParentPid, Filename) ->
    erlang:process_flag(trap_exit, true),
    ReadAtOnce = application:get_env(mzbench_api, bench_read_at_once, undefined),
    {ok, H} = file:open(Filename, [raw, read, {read_ahead, ReadAtOnce}]),
    Z = zlib:open(),
    ok = zlib:inflateInit(Z),
    Buffer = "",
    uncompressing_process(ParentPid, H, Z, Buffer).

uncompressing_process(ParentPid, File, ZStream, Buffer) ->
    Close = 
        fun() ->
            try
                ok = zlib:inflateEnd(ZStream),
                ok = zlib:close(ZStream)
            catch
                _:data_error -> ok
            end,
            file:close(File)
        end,

    receive
        {'EXIT', _, _} ->
            Close();
        close ->
            Close();
        read_line ->
            {Response, NewBuffer} = read_line_from_compressed_stream(File, ZStream, Buffer),
            ParentPid ! Response,
            uncompressing_process(ParentPid, File, ZStream, NewBuffer)
    end.

read_line_from_compressed_stream(File, ZStream, Buffer) ->
    Lines = string:tokens(Buffer, "\n"),
    case length(Lines) of
        1 ->
            case file:read(File, 255) of
                {ok, Data} ->
                    UncompressedData = zlib:inflate(ZStream, Data),
                    NewBuffer = string:concat(Buffer, UncompressedData),

                    Lines2 = string:tokens(NewBuffer, "\n"),
                    case length(Lines2) of
                        1 -> read_line_from_compressed_stream(File, ZStream, NewBuffer);
                        _ -> {{ok, hd(Lines2)}, string:join(tl(Lines2), "\n")}
                    end;
                eof ->
                    {eof, Buffer};
                {error, Reason} ->
                    {{error, Reason}, Buffer}
            end;
        _ -> {{ok, hd(Lines)}, string:join(tl(Lines), "\n")}
    end.

