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
                      metrics => [{"latency.mean", gauge},
                                  {"latency.50", gauge},
                                  {"latency.95", gauge},
                                  {"latency.99", gauge},
                                  {"latency.99.5", gauge},
                                  {"latency.max", gauge}]}},
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
    Command = Exec ++ lists:foldr(fun({url, Url}, A) -> A ++ " " ++ Url;
                        ({Opt, Val}, A) ->
                            L = string:join(string:tokens(atom_to_list(Opt), "_"), "-"),
                            Val2 = case length(string:tokens(Val, " ")) of
                                0 -> Val;
                                1 -> Val;
                                _ -> "\"" ++ Val ++ "\""
                                    end,
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

run(Command) ->
    erlang:spawn(?MODULE, statsd, []),
    Port = open_port({spawn, Command}, [
        {line, 255}, stream, eof, exit_status, stderr_to_stdout]),
    get_data(Port).

get_data(Port) -> get_data(Port, []).

get_data(Port, Buffer) ->
    receive
        {Port, {data, {eol, Bytes}}} ->
            lager:info("Output: ~s", [Buffer ++ Bytes]),
            get_data(Port, []);
        {Port, {data, {noeol, Bytes}}} ->
            get_data(Port, Buffer ++ Bytes);
        {Port, eof} ->
            Port ! {self(), close},
            get_data(Port, Buffer);
        stop ->
            Port ! {self(), close},
            get_data(Port, Buffer);
        {Port, closed} ->
            receive
                {Port, {exit_status, Code}} -> Code
            end
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
        [Metric, Value, Type] -> ValueInt = binary_to_integer(Value),
                                 MetricStr = binary_to_list(Metric),
                                 TypeAtom = case Type of
                                                <<"c">> -> counter;
                                                <<"g">> -> gauge;
                                                _ -> lager:info("Unknown type ~p", [Type]), gauge
                                            end,
                                 mzb_metrics:notify({MetricStr, TypeAtom}, ValueInt);
        _ -> lager:info("Unknown format")
    end.

accept(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {udp, Socket, _Host, _Port, Binary} ->
            parse(Binary),
            accept(Socket)
    end.
