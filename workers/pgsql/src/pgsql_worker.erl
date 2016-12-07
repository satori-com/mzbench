-module(pgsql_worker).

-export([initial_state/0, metrics/0,
         connect/3, query/3, query/4]).

-define(DEFAULT_OPTIONS, [{timeout, 4000}]).

-record(s,
    { 
        groups = #{},
        connection
    }).

initial_state() -> #s{}.

metrics() -> [].

connect(State, _Meta, Param) ->
    User = proplists:get_value(user, Param),
    Password = proplists:get_value(password, Param),
    Host = proplists:get_value(host, Param, "localhost"),
    RemainingParams = lists:foldl(fun proplists:delete/2, Param, [user, password, host]),
    {ok, C} = epgsql:connect(Host, User, Password, RemainingParams ++ ?DEFAULT_OPTIONS),
    {nil, State#s{connection = C}}.

query(State, Meta, Statement) ->
    query(State, Meta, "default", Statement).

query(State, Meta, Group, Statement) when is_binary(Statement) ->
    query(State, Meta, Group, binary_to_list(Statement));
query(State, Meta, Group, Statement) when is_atom(Group) ->
    query(State, Meta, atom_to_list(Group), Statement);
query(State, Meta, Group, Statement) when is_binary(Group) ->
    query(State, Meta, binary_to_list(Group), Statement);
query(#s{connection = C} = State, _Meta, Group, Statement) ->
    State2 = add_metric_group(State, Group),
    TimeStart = os:timestamp(),
    case tuple_to_list(epgsql:squery(C, Statement)) of
        [error | E] -> lager:error("Error: ~p", [E]),
                    mzb_metrics:notify(Group ++ ".error", 1);
        [ok | _] -> mzb_metrics:notify(Group ++ ".ok", 1)
    end,
    TimeFinish = os:timestamp(),
    mzb_metrics:notify({Group ++ ".latency", histogram}, timer:now_diff(TimeFinish, TimeStart)),
    {nil, State2}.

add_metric_group(#s{groups = Groups} = State, GroupName) ->
    case maps:is_key(GroupName, Groups) of
        true -> State;
        false -> mzb_metrics:declare_metrics([{group, "PostgreSQL (" ++ GroupName ++ ")", [
                {graph, #{title => "Latency",
                          units => "microsec",
                          metrics => [{GroupName ++ ".latency", histogram}]}},
                {graph, #{title => "Results",
                          units => "N",
                          metrics => [{GroupName ++ ".ok", counter},
                                      {GroupName ++ ".error", counter}]}}

                ]}]),
                State#s{groups = maps:put(GroupName, 1, Groups)}
    end.
