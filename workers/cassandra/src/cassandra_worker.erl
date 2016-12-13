-module(cassandra_worker).

-export([initial_state/0, metrics/0]).

-export([connect/3, query/3, set_prefix/3]).

-record(s,
    { 
        prefix = "default",
        client = undefined
    }).

-define(TIMED(Name, Expr),
    (fun () ->
        StartTime = os:timestamp(),
        Result = Expr,
        Value = timer:now_diff(os:timestamp(), StartTime),
        mzb_metrics:notify({Name, histogram}, Value),
        Result
    end)()).

initial_state() -> #s{}.

metrics() -> metrics("default").

metrics(Prefix) ->
    [
        {group, "Cassandra (" ++ Prefix ++ ")", [
            {graph, #{title => "Result",
                      units => "N",
                      metrics => [{Prefix ++ ".ok", counter}, {Prefix ++ ".error", counter}]}},
            {graph, #{title => "Latency",
                      units => "microseconds",
                      metrics => [{Prefix ++ ".latency", histogram}]}}
        ]}
    ].

connect(State, _Meta, Param) ->
    Host = proplists:get_value(host, Param, "127.0.0.1"),
    Port = proplists:get_value(port, Param, 9042),
    RemainingParams = lists:foldl(fun proplists:delete/2, Param, [host, port]),
    {ok, C} = cqerl:get_client({Host, Port}, RemainingParams),
    {nil, State#s{client = C}}.

query(#s{prefix = Prefix, client = Client} = State, _Meta, Statement) ->
    case ?TIMED(Prefix ++ ".latency", cqerl:run_query(Client, Statement)) of
        {ok, _} -> mzb_metrics:notify(Prefix ++ ".ok", 1);
        E -> lager:error("Error: ~p", [E]),
             mzb_metrics:notify(Prefix ++ ".error", 1)
    end,
    {nil, State}.

set_prefix(State, _Meta, NewPrefix) ->
    mzb_metrics:declare_metrics(metrics(NewPrefix)),
    {nil, State#s{prefix = NewPrefix}}.

