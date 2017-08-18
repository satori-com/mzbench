-module(tcpkali_worker).

-export([
    initial_state/0,
    metrics/0,
    statsd/0,
    start/3,
    start_cmd/3,
    json2cbor/3,
    json2cbor/4,
    encode/4,
    replace_expressions/2,
    replace_back/2
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

            {graph, #{title => "Traffic data",
                      units => "N",
                      metrics => [{"tcpkali.traffic.data", counter},
                                  {"tcpkali.traffic.data.rcvd", counter},
                                  {"tcpkali.traffic.data.sent", counter}]}},
            {graph, #{title => "Messages",
                      units => "N",
                      metrics => [{"tcpkali.traffic.msgs.sent", counter},
                                  {"tcpkali.traffic.msgs.rcvd", counter}]}},
            {graph, #{title => "Latency",
                      units => "ms",
                      metrics => [{"tcpkali.latency.message.min", gauge},
                                  {"tcpkali.latency.message.mean", gauge},
                                  {"tcpkali.latency.message.95", gauge},
                                  {"tcpkali.latency.message.99", gauge},
                                  {"tcpkali.latency.message.99.5", gauge},
                                  {"tcpkali.latency.message.max", gauge}]}},
            {graph, #{title => "Latency connect",
                      units => "ms",
                      metrics => [{"tcpkali.latency.connect.min", gauge},
                                  {"tcpkali.latency.connect.mean", gauge},
                                  {"tcpkali.latency.connect.95", gauge},
                                  {"tcpkali.latency.connect.99", gauge},
                                  {"tcpkali.latency.connect.99.5", gauge},
                                  {"tcpkali.latency.connect.max", gauge}]}}
        ]}
    ].

start(#state{executable = Exec} = State, _Meta, Options) ->
    Command = Exec ++ lists:foldl(fun({raw, Raw}, A) -> A ++ " " ++ Raw;
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

start_cmd(#state{executable = Exec} = State, _Meta, "tcpkali " ++ Options) ->
    Command = Exec ++ " " ++ Options,
    lager:info("Executing ~p...", [Command]),
    case run(Command) of
        0 -> ok;
        ExitCode -> erlang:error({tcpkali_error, ExitCode})
    end,
    {nil, State}.

generate_filler(N, Id) ->
    Prefix = "\\{" ++ erlang:integer_to_list(Id),
    Postfix = "}",
    NumToGenerate = N - length(Prefix) - length(Postfix),
    case NumToGenerate  >= 0 of
        true -> Prefix ++ [$\s || _  <- lists:seq(1, NumToGenerate)] ++ Postfix;
        false -> erlang:error({expression_too_small})
    end.

replace_expressions(Str) ->
    % Hack: {message.marker} will be replaced with 30 bytes long marker inside tcpkali
    % which will make cbor broken because "{message.marker}" itself has different length
    % so we replace it with "{message.marker             }" which has the same meaning
    % but it will not change the length after replacement is done
    Str2 = re:replace(Str, "\\\\{message.marker}", "\\\\{message.marker             }",[{return,list},global]),
    replace_expressions(Str2, []).

replace_expressions(Str, Acc) ->
    RE = "(?<PREFIX>^.*)(?<EXPR>\\\\{.*})\\((?<NUM>[0-9]+)\\)(?<ENDING>.*$)",
    case re:run(Str, RE, [{capture, [<<"PREFIX">>,<<"EXPR">>, <<"NUM">>, <<"ENDING">>], list}]) of
        {match, [Prefix, Expr, NumStr, Ending]} ->
            Num = erlang:list_to_integer(NumStr),
            LastId =
                case Acc of
                    [] -> 0;
                    [{N, _}|_] -> N
                end,
            Replacement = generate_filler(Num, LastId + 1),
            replace_expressions(Prefix ++ Replacement ++ Ending, [{LastId + 1, Expr}|Acc]);
        nomatch ->
            {Str, Acc}
    end.

replace_back(Str, []) -> Str;
replace_back(Str, [{Id, Expr} | Tail]) ->
    RE = lists:flatten(io_lib:format("\\\\{~b\\s*}", [Id])),
    Expr2 = re:replace(Expr, "\\\\", "\\\\\\\\",[{return,list}, global]),
    Res = re:replace(Str, RE, Expr2, [{return,list}]),
    replace_back(Res, Tail).

json2cbor(State, _Meta, Str) ->
    json2cbor(State, _Meta, Str, string).

json2cbor(State, _Meta, Str, Type) ->
    Str2 = re:replace(Str, "\\\\", "\\\\\\\\",[{return,list}, global]),

    {Str3, Expressions} = replace_expressions(Str2),

    JSON = jiffy:decode(Str3, [return_maps]),
    CBORBin = erlang:iolist_to_binary(cbor:encode(JSON)),
    CBOR = replace_back(CBORBin, Expressions),
    Formatted =
        case Type of
            binary -> iolist_to_binary(CBOR);
            string -> lists:flatten([format_byte(X) || <<X:8>> <= iolist_to_binary(CBOR)])
        end,
    {Formatted, State}.

format_byte(B) ->
    case lists:member(B, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789()[]{} .,;-+=\\/") of
        true -> B;
        false -> io_lib:format("\\x~2.16.0B",[B])
    end.

encode(State, Meta, "cbor", Str) ->
    json2cbor(State, Meta, Str);
encode(State, _Meta, _, Str) ->
    {Str, State}.

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
