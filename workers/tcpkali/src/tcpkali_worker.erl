-module(tcpkali_worker).

-export([
    initial_state/0,
    metrics/0,
    statsd/2,
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
            {graph, #{title => "Traffic bitrate (aggregated)",
                      units => "bits/sec",
                      metrics => [
                                  {"tcpkali.traffic.bitrate.in", derived, #{resolver => traffic_in_resolver}}
%                                  {"tcpkali.traffic.bitrate.out", derived, #{resolver => traffic_out_resolver}}
                                  ]}}
        ]}
    ].

metric_by_name("tcpkali.latency.connect." ++ _ = Name) ->
    {group, "Tcpkali", [{graph, #{title => "Latency connect", units => "ms", metrics => [{Name, gauge}]}}]};
metric_by_name("tcpkali.latency.message." ++ _ = Name) ->
    {group, "Tcpkali", [{graph, #{title => "Latency", units => "ms", metrics => [{Name, gauge}]}}]};
metric_by_name("tcpkali.traffic.bitrate." ++ _ = Name) ->
    {group, "Tcpkali", [{graph, #{title => "Traffic bitrate", units => "bits/s", metrics => [{Name, gauge}]}}]};
metric_by_name("tcpkali.connections.total." ++ _ = Name) ->
    {group, "Tcpkali", [{graph, #{title => "Connections total", units => "N", metrics => [{Name, gauge}]}}]};
metric_by_name("tcpkali.traffic.msgs." ++ _ = Name) ->
    {group, "Tcpkali", [{graph, #{title => "Messages", units => "N", metrics => [{Name, counter}]}}]};
metric_by_name("tcpkali.traffic.data." ++ _ = Name) ->
    {group, "Tcpkali", [{graph, #{title => "Traffic data", units => "bytes/s", metrics => [{Name, counter}]}}]};
metric_by_name("tcpkali.connections.opened" ++ _ = Name) ->
    {group, "Tcpkali", [{graph, #{title => "Connections opened", units => "N", metrics => [{Name, counter}]}}]};
metric_by_name(_) -> undefined.

start(#state{executable = Exec} = State, Meta, Options) ->
    Command = Exec ++ lists:foldl(fun({raw, Raw}, A) -> A ++ " " ++ Raw;
                        ({url, Url}, A) -> A ++ " \"" ++ Url ++ "\"";
                        ({Opt, Val}, A) ->
                            L = string:join(string:tokens(atom_to_list(Opt), "_"), "-"),
                            Val2 = prepare_val(Val),
                            case length(L) of
                                1 -> A ++ " -" ++ L ++ Val2;
                                _ -> A ++ " --" ++ L ++ " " ++ Val2 end end,
                            "", Options),
    WorkerID = proplists:get_value(worker_id, Meta),
    case run(Command, WorkerID) of
        0 -> ok;
        ExitCode -> erlang:error({tcpkali_error, ExitCode})
    end,
    {nil, State}.

start_cmd(#state{executable = Exec} = State, Meta, "tcpkali " ++ Options) ->
    Command = Exec ++ " " ++ Options,
    WorkerID = proplists:get_value(worker_id, Meta),
    PoolID = proplists:get_value(pool_id, Meta),
    ID = lists:flatten(io_lib:format("~b.~b", [PoolID, WorkerID])),
    case run(Command, ID) of
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

run(Command, WorkerID) ->
    StatsdPort = spawn_statsd(WorkerID),
    PortOption = lists:flatten(io_lib:format(" --statsd-port ~b", [StatsdPort])),
    lager:info("Executing ~p...", [Command ++ PortOption]),
    {ok, Pid, _OsPid} = exec:run(Command ++ PortOption, [monitor, stdout, stderr]),
    get_data(Pid).

get_data(Pid) ->
    receive
        {Stream, _Pid, Bytes} when (Stream == stdout) or (Stream == stderr) -> 
            lager:info("Output: ~s", [Bytes]),
            get_data(Pid);
        {'DOWN', _ , _, _, {exit_status, Code}} -> Code;
        {'DOWN', _ , _, _, normal} -> 0
    end.

spawn_statsd(WorkerID) ->
    erlang:spawn(?MODULE, statsd, [self(), WorkerID]),
    receive
        {started, Port} -> Port
    after
        5000 -> erlang:error(statsd_start_timed_out)
    end.

statsd(Parent, WorkerID) ->
    {ok, ListenSocket} = gen_udp:open(0, [binary, {active, false}]),
    {ok, Port} = inet:port(ListenSocket),
    Parent ! {started, Port},
    accept(ListenSocket, WorkerID).

parse(<<>>, _) -> ok;
parse(Bin, WId) ->
    lists:map(fun (B) -> report(B, WId) end, binary:split(Bin, <<"\n">>, [global])).

report(<<>>, _) -> ok;
report(Bin, WorkerID) ->
    case binary:split(Bin, [<<"|">>, <<":">>], [global]) of
        [Metric, Value, Type|_] ->
            MetricStr = binary_to_list(Metric),
            {TypeAtom, ValueInt} =
                case Type of
                    <<"c">> -> {counter, parse_int(Value)};
                    <<"g">> -> {gauge, parse_int(Value)};
                    _ -> lager:info("Unknown type ~p", [Type]), gauge
                end,
            case TypeAtom of
                gauge -> notify(MetricStr ++ "." ++ WorkerID, TypeAtom, ValueInt);
                counter -> notify(MetricStr, TypeAtom, ValueInt)
            end;
        _ -> lager:info("Unknown format: ~s", [Bin])
    end.

notify(Metric, Type, Value) ->
    case metric_by_name(Metric) of
        undefined -> ignore;
        MetricSpec ->
            case erlang:get({metric_cache, MetricSpec}) of
                declared -> ok;
                undefined ->
                    mzb_metrics:declare_metrics([MetricSpec]),
                    erlang:put({metric_cache, MetricSpec}, declared)
            end,
            mzb_metrics:notify({Metric, Type}, Value)
    end.

parse_int(Bin) ->
    try erlang:binary_to_integer(Bin) of
        V -> V
    catch
        _:_ ->
            erlang:round(erlang:binary_to_float(Bin))
    end.

accept(Socket, WId) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {udp, Socket, _Host, _Port, Binary} ->
            parse(Binary, WId),
            accept(Socket, WId)
    end.
