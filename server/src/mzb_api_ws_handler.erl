-module(mzb_api_ws_handler).

-export([init/2,
         terminate/3,
         websocket_handle/3,
         websocket_info/3]).

-record(state, {
          ref = undefined,
          timeline_opts = undefined,
          timeline_bounds = undefined
       }).

init(Req, _Opts) ->
    Ref = erlang:make_ref(),
    ok = gen_event:add_handler(mzb_api_firehose, {mzb_api_firehose_handler, Ref}, [self()]),
    {cowboy_websocket, Req, #state{ref = Ref}}.

terminate(_Reason, _Req, #state{ref = Ref}) ->
    gen_event:delete_handler(mzb_api_firehose, {mzb_api_firehose_handler, Ref}, [self()]),
    ok.

websocket_handle({text, Msg}, Req, State) ->
    case dispatch_request(jiffy:decode(Msg, [return_maps]), State) of
        {reply, Reply, NewState} ->
            JsonReply = jiffy:encode(mzb_string:str_to_bstr(Reply)),
            {reply, {text, JsonReply}, Req, NewState};
        {ok, NewState} ->
            {ok, Req, NewState}
    end;

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info(Message, Req, State) ->
    case dispatch_info(Message, State) of
        {reply, Reply, NewState} ->
            JsonReply = jiffy:encode(mzb_string:str_to_bstr(Reply)),
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

dispatch_info(Info, State) ->
    lager:warning("~p has received unexpected info: ~p", [?MODULE, Info]),
    {ok, State}.

dispatch_request(#{<<"cmd">> := <<"ping">>}, State) ->
    {reply, <<"pong">>, State};

dispatch_request(#{<<"cmd">> := <<"get_timeline">>} = Cmd, State) ->
    BenchInfos0 = mzb_api_server:get_info(),
    BenchInfos1 = normalize(BenchInfos0),
    BenchInfos2 = apply_filter(Cmd, BenchInfos1),
    {TimelineItems, {MinId, MaxId}} = apply_pagination(Cmd, BenchInfos2),

    KV = [{next, MinId}, {prev, MaxId}],
    Pager = maps:from_list([T || T = {K,V} <- KV, V /= undefined]),

    Event = #{
               type => "INIT_TIMELINE",
               data => TimelineItems,
               pager => Pager
             },

    {reply, Event, State#state{timeline_opts   = Cmd,
                               timeline_bounds = {MinId, MaxId}}};

dispatch_request(Cmd, State) ->
    lager:warning("~p has received unexpected info: ~p", [?MODULE, Cmd]),
    {ok, State}.

%% Normalization

normalize(BenchInfos) ->
    Sorted = lists:sort(fun ({IdA, _}, {IdB, _}) ->
                                IdA >= IdB
                        end, BenchInfos),
    lists:map(fun normalize_bench/1, Sorted).

normalize_bench({_Id, Status = #{config:= Config}}) ->
    StatusFields =  maps:with([status, metrics, id], Status),

    TimeFields = maps:fold(fun (K, V, AccIn) when is_number(V) ->
                                   maps:put(K, mzb_string:iso_8601_fmt(V), AccIn);
                               (_, _, AccIn) -> AccIn
                           end,
                           #{},
                           maps:with([finish_time, start_time], Status)),

    #{script:= #{body:= ScriptBody, name:= ScriptName}} = Config,
    ScriptFields = #{script_body => ScriptBody, script_name => ScriptName},

    lists:foldl(fun (Map, Acc) -> maps:merge(Acc, Map) end,
                #{},
                [StatusFields, TimeFields, ScriptFields]).

%% Filtering

apply_filter(TimelineOpts, BenchInfos) -> 
    Query = maps:get(<<"q">>, TimelineOpts, undefined),
    case Query of
        undefined -> BenchInfos;
        Q -> [Bench || Bench <- BenchInfos, is_satisfy_filter(Q, Bench)]
    end.

get_searchable_fields(BenchInfo) ->
    SearchFields = maps:with([id, status, script_name, start_time, finish_time], BenchInfo),
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
                      case re:run(Field, QueryString, []) of
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
    Limit = maps:get(<<"limit">>, Pagination, 10),
    MaxId = maps:get(<<"max_id">>, Pagination, undefined),
    MinId = maps:get(<<"min_id">>, Pagination, undefined),
    BenchId = maps:get(<<"bench_id">>, Pagination, undefined),

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
