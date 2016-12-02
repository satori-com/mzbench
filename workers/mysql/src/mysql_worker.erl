-module(mysql_worker).

-export([initial_state/0, metrics/0,
         connect/3, query/3, query/4]).

-include_lib("emysql/include/emysql.hrl").

-define(POOL_NAME, mzbench_pool).
-define(DEFAULT_OPTIONS, [{size,1}, {encoding,utf8}]).

-record(s,
    { 
        groups = #{}
    }).

initial_state() -> #s{}.

metrics() -> [].

connect(State, _Meta, Param) ->
    {emysql:add_pool(?POOL_NAME, ?DEFAULT_OPTIONS ++ Param), State}.

query(State, Meta, Statement) ->
    query(State, Meta, "default", Statement).

query(State, Meta, Group, Statement) when is_list(Statement) ->
    query(State, Meta, Group, list_to_binary(Statement));
query(State, Meta, Group, Statement) when is_atom(Group) ->
    query(State, Meta, atom_to_list(Group), Statement);
query(State, Meta, Group, Statement) when is_binary(Group) ->
    query(State, Meta, binary_to_list(Group), Statement);
query(State, _Meta, Group, Statement) ->
    State2 = add_metric_group(State, Group),
    TimeStart = os:timestamp(),
    case emysql:execute(?POOL_NAME, Statement) of
        #error_packet{} -> mzb_metrics:notify(Group ++ ".error", 1);
        #result_packet{} -> mzb_metrics:notify(Group ++ ".result", 1);
        #ok_packet{} -> mzb_metrics:notify(Group ++ ".ok", 1)
    end,
    TimeFinish = os:timestamp(),
    mzb_metrics:notify({Group ++ ".latency", histogram}, timer:now_diff(TimeFinish, TimeStart)),
    {nil, State2}.

add_metric_group(#s{groups = Groups} = State, GroupName) ->
    case maps:is_key(GroupName, Groups) of
        true -> State;
        false -> mzb_metrics:declare_metrics([{group, "MySQL (" ++ GroupName ++ ")", [
                {graph, #{title => "Latency",
                          units => "microsec",
                          metrics => [{GroupName ++ ".latency", histogram}]}},
                {graph, #{title => "Results",
                          units => "N",
                          metrics => [{GroupName ++ ".ok", counter},
                                      {GroupName ++ ".error", counter},
                                      {GroupName ++ ".result", counter}]}}

                ]}]),
                State#s{groups = maps:put(GroupName, 1, Groups)}
    end.
