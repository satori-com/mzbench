-module(tcpkali_worker).

-export([
    initial_state/0,
    metrics/0,
    statsd/0,
    start/3
]).

-define(Timeout, 5000).
-define(StatsdPort, 8125).

-record(state, {
            executable :: string()
        }).

initial_state() ->
    {ok, WorkerDirs} = application:get_env(mzbench, workers_dirs),
    Masks = [filename:join([D, "*", "resources/tcpkali"]) || D <- WorkerDirs],
    #state{executable = case lists:append([mzb_file:wildcard(M) || M <- Masks])  of
                [] -> erlang:error("Can't find tcpkali binary");
                [Path|_] -> Path
            end}.

metrics() ->
    [
        {group, "Tcpkali", [
            {graph, #{title => "Connections opened",
                      units => "N",
                      metrics => [{"tcpkali.connections.opened",  counter}]}},
            {graph, #{title => "Connections total",
                      units => "N",
                      metrics => [{"tcpkali.connections.total", gauge},
                                  {"tcpkali.connections.total.in", gauge},
                                  {"tcpkali.connections.total.out", gauge}]}},
            {graph, #{title => "Traffic bitrate",
                      units => "N",
                      metrics => [{"tcpkali.traffic.bitrate", gauge},
                                  {"tcpkali.traffic.bitrate.in", gauge},
                                  {"tcpkali.traffic.bitrate.out", gauge}]}},
            {graph, #{title => "Latency",
                      units => "ms",
                      metrics => [{"tcpkali.latency.message.mean", gauge},
                                  {"tcpkali.latency.message.50", gauge},
                                  {"tcpkali.latency.message.95", gauge},
                                  {"tcpkali.latency.message.99", gauge},
                                  {"tcpkali.latency.message.99.5", gauge},
                                  {"tcpkali.latency.message.max", gauge}]}},
            {graph, #{title => "Traffic data",
                      units => "N",
                      metrics => [{"tcpkali.traffic.data", counter},
                                  {"tcpkali.traffic.data.rcvd", counter},
                                  {"tcpkali.traffic.data.sent", counter},
                                  {"tcpkali.traffic.data.reads", counter},
                                  {"tcpkali.traffic.data.writes", counter}]}}
        ]}
    ].

start(#state{executable = Exec} = State, _Meta, Options) ->
    Command = Exec ++ lists:foldr(fun({raw, Raw}, A) -> A ++ " " ++ Raw;
                        ({url, Url}, A) -> A ++ " \"" ++ Url ++ "\"";
                        ({Opt, Val}, A) ->
                            L = string:join(string:tokens(atom_to_list(Opt), "_"), "-"),
                            Val2 = prepare_val(Val),
                            case length(L) of
                                1 -> A ++ " -" ++ L ++ Val2;
                                _ -> A ++ " --" ++ L ++ " " ++ Val2 end end,
                            "", Options),
    lager:info("Executing ~p...", [Command]),
    case run(Command) of
        0 -> ok;
        ExitCode -> erlang:error({tcpkali_error, ExitCode})
    end,
    {nil, State}.

prepare_val(Val) when is_float(Val) ->   float_to_list(Val);
prepare_val(Val) when is_integer(Val) -> integer_to_list(Val);
prepare_val(Val) when is_list(Val) ->
    case length(string:tokens(Val, " ")) of
        0 -> Val;
        _ -> "'" ++ Val ++ "'"
    end.

run(Command) ->
    erlang:spawn(?MODULE, statsd, []),
    {ok, Pid, _OsPid} = exec:run(Command, [monitor, stdout, stderr]),
    get_data(Pid).

get_data(Pid) ->
    receive
        {Stream, _Pid, Bytes} when (Stream == stdout) or (Stream == stderr) -> 
            lager:info("Output: ~s", [Bytes]),
            get_data(Pid);
        {'DOWN', _ , _, _, {exit_status, Code}} -> Code;
        {'DOWN', _ , _, _, normal} -> 0
    end.

statsd() ->
    {ok, ListenSocket} = gen_udp:open(?StatsdPort, [binary, {active, false}]),
    accept(ListenSocket).

parse(<<>>) -> ok;
parse(Bin) ->
    lists:map(fun report/1, binary:split(Bin, <<"\n">>, [global])).

report(<<>>) -> ok;
report(Bin) ->
    case binary:split(Bin, [<<"|">>, <<":">>], [global]) of
        [Metric, Value, Type|_] ->
                                 MetricStr = binary_to_list(Metric),
                                 {TypeAtom, ValueInt} = case Type of
                                                <<"c">> -> {counter, parse_int(Value)};
                                                <<"g">> -> {gauge, parse_int(Value)};
                                                _ -> lager:info("Unknown type ~p", [Type]), gauge
                                            end,
                                 mzb_metrics:notify({MetricStr, TypeAtom}, ValueInt);
        _ -> lager:info("Unknown format: ~s", [Bin])
    end.

parse_int(Bin) ->
    try erlang:binary_to_integer(Bin) of
        V -> V
    catch
        _:_ ->
            erlang:round(erlang:binary_to_float(Bin))
    end.

accept(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {udp, Socket, _Host, _Port, Binary} ->
            parse(Binary),
            accept(Socket)
    end.
