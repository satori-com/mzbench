-module(mzb_api_ws_handler).

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).

-record(state, {
            filter = undefined,
            pagination = undefined,
            bench_id = undefined
         }).

init(Req, _Opts) ->
    {cowboy_websocket, Req, #state{}}.

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

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

dispatch_request(#{<<"cmd">> := <<"ping">>}, State) ->
    {reply, <<"pong">>, State};

dispatch_request(#{<<"cmd">> := <<"get_timeline">>} = Cmd, State) ->
    Filter = maps:get(<<"q">>, Cmd, undefined),
    Pagination = maps:with([<<"max_id">>, <<"min_id">>, <<"bench_id">>, <<"limit">>], Cmd),

    BenchInfos0       = mzb_api_server:get_info(),
    BenchInfos1       = normalize(BenchInfos0),
    BenchInfos2       = apply_filter(Filter, BenchInfos1),
    {Timeline, Pager} = apply_pagination(Pagination, BenchInfos2),

    Event = #{
               type => "INIT_TIMELINE",
               data => Timeline,
               pager => Pager
             },

    {reply, Event, State#state{filter = Filter, pagination = Pagination}}.

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

apply_filter(undefined, BenchInfos) -> BenchInfos;
apply_filter(Query, BenchInfos) ->
    % todo check for empty query
    lists:filter(fun(BenchInfo) ->
                     is_satisfy_filter(Query, BenchInfo)
                 end, BenchInfos).

get_searchable_fields(BenchInfo) ->
    SearchFields = maps:with([status, script_name, start_time, finish_time], BenchInfo),
    Values = maps:values(SearchFields),
    lists:map(fun (X) when is_atom(X) -> atom_to_list(X);
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

get_boundary(_, []) -> no_bound;
get_boundary([H | _], [H | _]) -> no_bound;
get_boundary(_, [ #{id:= Id} | _]) -> Id.

prev_next_pager(BenchInfos, Paginated) ->
    Pager = case get_boundary(BenchInfos, Paginated) of
        no_bound -> #{};
        MaxId -> #{prev => MaxId}
    end,

    case get_boundary(lists:reverse(BenchInfos), lists:reverse(Paginated)) of
        no_bound -> Pager;
        MinId -> Pager#{next => MinId}
    end.

index_of(Id, List) -> index_of(Id, List, 1).

index_of(_, [], _)  -> not_found;
index_of(Id, [#{id:= Id}|_], Index) -> Index;
index_of(Id, [_|Tl], Index) -> index_of(Id, Tl, Index+1).

apply_pagination(Pagination, BenchInfos) ->
    Limit = maps:get(<<"limit">>, Pagination, 20),
    MaxId = maps:get(<<"max_id">>, Pagination, undefined),
    MinId = maps:get(<<"min_id">>, Pagination, undefined),
    BenchId = maps:get(<<"bench_id">>, Pagination, undefined),

    % check for bounds
    Bounded = lists:filter(fun(BenchInfo) ->
                               is_satisfy_bounds({MinId, MaxId}, BenchInfo)
                           end, BenchInfos),

    Paginated = pick_page_items({BenchId, MinId, MaxId}, Limit, Bounded),

    % Calculate pager for prev and next links on dashboards
    Pager = prev_next_pager(BenchInfos, Paginated),

    {Paginated, Pager}.

pick_page_items({BenchId, undefined, undefined}, Limit, Bounded) when is_integer(BenchId) ->
    Bounded1 = case index_of(BenchId, Bounded) of
        not_found -> Bounded;
        X when X =< Limit -> Bounded;
        X -> lists:nthtail(X-1, Bounded)
    end,
    lists:sublist(Bounded1, Limit);
pick_page_items({undefined, MinId, undefined}, Limit, Bounded) when is_integer(MinId) ->
    lists:reverse(lists:sublist(lists:reverse(Bounded), Limit));
pick_page_items(_, Limit, Bounded) ->
    lists:sublist(Bounded, Limit).

is_satisfy_bounds({MinId, MaxId}, #{id := Id}) ->
    IsBelowMax = undefined == MaxId orelse Id < MaxId,
    IsAboveMin = undefined == MinId orelse Id > MinId,
    IsBelowMax andalso IsAboveMin.
