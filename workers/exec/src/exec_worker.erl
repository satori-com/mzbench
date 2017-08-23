-module(exec_worker).

-export([
    initial_state/0,
    metrics/0,
    execute/3,
    statsd_opts/3,
    declare_metric/6,
    statsd/1
]).

-define(DEFAULT_STATSD_PORT, 8125).

initial_state() ->
    #{
        statsd_spawned => false,
        statsd_opts => []
    }.

metrics() ->
    [
        {group, "Summary", [
            {graph, #{title => "Results",
                      units => "N",
                      metrics => [{"success", counter}, {"fail", counter}]}},
            {graph, #{title => "Latency",
                      units => "microseconds",
                      metrics => [{"latency", histogram}]}}
        ]}
    ].

declare_metric(State, _Meta, Group, Title, Name, Type) ->
    mzb_metrics:declare_metric(Group, Title, Name, Type, #{}),
    {nil, State}.

statsd_opts(State, _Meta, Opts) ->
    {nil, State#{statsd_opts => Opts}}.

execute(State = #{statsd_spawned := StatsdSpawned}, Meta, Command) ->
    NewState =
        case StatsdSpawned of
            true -> State;
            false -> start_statsd(State)
        end,
    WorkerId = proplists:get_value(worker_id, Meta, undefined),
    lager:info("Executing ~p... on ~p~n", [Command, WorkerId]),
    TimeStart = os:timestamp(),
    case run(Command, [], WorkerId) of
        0 -> mzb_metrics:notify("success", 1);
        ExitCode ->
            mzb_metrics:notify("fail", 1),
            lager:error("Execution on ~p failed. Exit Code: ~p", [WorkerId, ExitCode])
    end,
    TimeFinish = os:timestamp(),
    mzb_metrics:notify({"latency", histogram}, timer:now_diff(TimeFinish, TimeStart)),
    {nil, NewState}.

run(Command, _Opts, WorkerId) ->
    {ok, Pid, _OsPid} = exec:run(Command, [monitor, stdout, stderr,
        {env, [{"MZB_WORKER_ID", integer_to_list(WorkerId)}]}]),
    get_data(Pid, WorkerId).

get_data(Port, WorkerId) -> get_data(Port, WorkerId, []).

get_data(Port, WorkerId, _Buffer) ->
    receive
        {stdout, _Pid, Bytes} ->
            lager:info("Worker ~p output: ~s", [WorkerId, Bytes]),
            get_data(Port, WorkerId, []);
        {stderr, _Pid, Bytes} ->
            lager:error("Worker ~p error: ~s", [WorkerId, Bytes]),
            get_data(Port, WorkerId, []);
        {'DOWN', _ , _, _, {exit_status, Code}} -> Code;
        {'DOWN', _ , _, _, normal} -> 0
    end.

statsd(Opts) ->
    Port =
        case proplists:get_value(port, Opts) of
            undefined -> ?DEFAULT_STATSD_PORT;
            P -> P
        end,
    {ok, ListenSocket} = gen_udp:open(Port, [binary, {active, false}]),
    accept(ListenSocket).

accept(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {udp, Socket, _Host, _Port, Binary} ->
            parse(Binary),
            accept(Socket)
    end.

parse(<<>>) -> ok;
parse(Bin) ->
    lists:map(fun report/1, binary:split(Bin, <<"\n">>, [global])).

report(<<>>) -> ok;
report(Bin) ->
    case binary:split(Bin, [<<"|">>, <<":">>], [global]) of
        [Metric, Value, Type|_] ->
            MetricStr = binary_to_list(Metric),
            {TypeAtom, ValueInt} =
                case Type of
                    <<"c">> -> {counter, parse_num(Value)};
                    <<"g">> -> {gauge, parse_num(Value)};
                    _ -> lager:info("Unknown type ~p", [Type]), gauge
                end,
            mzb_metrics:notify({MetricStr, TypeAtom}, ValueInt);
        _ -> lager:info("Unknown format: ~s", [Bin])
    end.

parse_num(Bin) ->
    try erlang:binary_to_integer(Bin) of
        V -> V
    catch
        _:_ ->
            erlang:round(erlang:binary_to_float(Bin))
    end.

start_statsd(State = #{statsd_opts := Opts}) ->
    erlang:spawn(?MODULE, statsd, [Opts]),
    State.

