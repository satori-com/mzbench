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
            JsonReply = jiffy:encode(Reply),
            {reply, {text, JsonReply}, Req, NewState};
        {ok, NewState} ->
            {ok, Req, NewState}
    end;

websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
    erlang:start_timer(1000, self(), <<"How' you doin'?">>),
    {reply, {text, Msg}, Req, State};

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

dispatch_request(#{<<"cmd">> := <<"ping">>}, State) ->
    {reply, <<"pong">>, State};

dispatch_request(#{<<"cmd">> := <<"get_timeline">>} = Cmd, State) ->
    Filter = maps:get(<<"q">>, Cmd, undefined),
    Pagination = maps:get(<<"pagination">>, Cmd, undefined),
    NewState = State#state{filter = Filter, pagination = Pagination},

    BenchInfos = lists:sort(fun ({IdA, _}, {IdB, _}) ->
                                    IdA >= IdB
                            end, mzb_api_server:get_info()),

    Filtered = apply_filter(Filter, BenchInfos),
    Paginated = apply_pagination(Pagination, Filtered),

    {reply, Paginated, NewState}.

apply_filter(Query, BenchInfos) ->
    lists:filter(fun(BenchInfo) ->
                     is_satisfy_filter(Query, BenchInfo)
                 end, BenchInfos).

get_searchable_fields(BenchInfo) ->
    {_Id, #{status:= Status, config:= Config, start_time:= StartTime, finish_time:= FinishTime}} = BenchInfo,
    #{script:= #{name:= ScriptName}} = Config,

    SearchFields = [Status, ScriptName, mzb_string:iso_8601_fmt(StartTime)],
    case FinishTime of
        Time when is_number(Time) ->
            [mzb_string:iso_8601_fmt(Time) | SearchFields];
        _ -> SearchFields
    end.

is_satisfy_filter(undefined, BenchInfo) ->
    BenchInfo;
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
        lager:error("Failed to apply filter: ~p ~p ~p~n~n~p", [Error, Query, BenchInfo, erlang:get_stacktrace()]),
        false
    end.

apply_pagination(Pagination, BenchInfos) ->
    WithinBounds = lists:filter(fun(BenchInfo) ->
                                    is_satisfy_bounds(Pagination, BenchInfo)
                                end, BenchInfos),
    Limit = maps:get(<<"limit">>, Pagination, 20),
    {Paginated, _} = lists:split(Limit, WithinBounds),
    Paginated.

is_satisfy_bounds(Pagination, {Id, _Status}) ->
    MaxId = maps:get(<<"max_id">>, Pagination, undefined),
    MinId = maps:get(<<"min_id">>, Pagination, undefined),
    IsBelowMax = undefined == MaxId orelse Id < MaxId,
    IsAboveMin = undefined == MinId orelse Id > MinId,
    IsBelowMax andalso IsAboveMin.
