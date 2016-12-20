-module(mongo_worker).

-export([initial_state/0, metrics/0,
         connect/2, connect/3, insert/4, delete/4, find_one/4, count/4, set_prefix/3]).

-record(s,
    { 
        prefix = "default",
        connection = undefined
    }).

-define(TIMED(Prefix, Expr),
    (fun () ->
        StartTime = os:timestamp(),
        Result = Expr,
        Value = timer:now_diff(os:timestamp(), StartTime),
        mzb_metrics:notify({Prefix ++ ".latency", histogram}, Value),
        count_response(Prefix, Result)
    end)()).

initial_state() -> #s{}.

metrics() -> metrics("default").

connect(State, _Meta, Param) ->
    BinaryParams = lists:map(fun ({host, L}) -> {host, L};
                            ({A, B}) when is_list(B) -> {A, list_to_binary(B)};
                            (C) -> C end, Param),
    {ok, C} = mc_worker_api:connect(BinaryParams),
    {nil, State#s{connection = C}}.

connect(State, _Meta) ->
    {ok, C} = mc_worker_api:connect([]),
    {nil, State#s{connection = C}}.

set_prefix(#s{} = State, _Meta, Prefix) ->
    _ = add_metric_group(Prefix),
    {nil, State#s{prefix = Prefix}}.


insert(#s{connection = C, prefix = Prefix} = State, _Meta, Collection, Document) ->
    _ = ?TIMED(Prefix, mc_worker_api:insert(C, to_bin(Collection), to_bin(Document))),
    {nil, State}.

delete(#s{connection = C, prefix = Prefix} = State, _Meta, Collection, Selector) ->
    _ = ?TIMED(Prefix, mc_worker_api:delete(C, to_bin(Collection), to_bin(Selector))),
    {nil, State}.

find_one(#s{connection = C, prefix = Prefix} = State, _Meta, Collection, Selector) ->
    _ = ?TIMED(Prefix, mc_worker_api:find_one(C, to_bin(Collection), to_bin(Selector))),
    {nil, State}.

count(#s{connection = C, prefix = Prefix} = State, _Meta, Collection, Selector) ->
    _ = ?TIMED(Prefix, mc_worker_api:count(C, to_bin(Collection), to_bin(Selector))),
    {nil, State}.

to_bin(T) when is_tuple(T) ->
    list_to_tuple(lists:map(fun to_bin/1, tuple_to_list(T)));
to_bin([H | _] = L) when is_tuple(H) ->
    lists:map(fun to_bin/1, L);
to_bin(L) when is_list(L) ->
    list_to_binary(L);
to_bin(B) -> B.

count_response(Prefix, M) when is_map(M) ->
    mzb_metrics:notify(Prefix ++ ".ok", 1);
count_response(Prefix, {true, _}) ->
    mzb_metrics:notify(Prefix ++ ".ok", 1);
count_response(Prefix, {{true, _}, _}) ->
    mzb_metrics:notify(Prefix ++ ".ok", 1);
count_response(Prefix, E) ->
    lager:error("Error: ~p", [E]),
    mzb_metrics:notify(Prefix ++ ".error", 1).

add_metric_group(GroupName) ->
    mzb_metrics:declare_metrics(metrics(GroupName)).

metrics(GroupName) ->
    [{group, "MongoDB (" ++ GroupName ++ ")", [
            {graph, #{title => "Latency",
                      units => "microsec",
                      metrics => [{GroupName ++ ".latency", histogram}]}},
            {graph, #{title => "Results",
                      units => "N",
                      metrics => [{GroupName ++ ".ok", counter},
                                  {GroupName ++ ".error", counter}]}}]}].