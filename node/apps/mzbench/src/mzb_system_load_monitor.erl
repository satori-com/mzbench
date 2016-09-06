-module(mzb_system_load_monitor).

-behaviour(gen_server).

%% API
-export([
    start_link/1,
    metric_names/0
    ]).

%% gen_server callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3
		]).

% for tests:
-export([parse_darwin_netstat_output/1, parse_linux_netstat_output/1]).

-record(state, {
    net_stat_state :: [{Name :: string(), Props :: [term()]}],
    last_trigger_timestamp :: erlang:timestamp() | not_available,
    interval_ms :: undefined | integer()
    }).

%% API functions

start_link(IntervalMs) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [IntervalMs], []).

metric_names() ->
    [{group, "System Load", [
        {graph, #{title => "Load average",
                  units => "la1",
                  metrics => [{metric_name("la1"), gauge}]}},

        {graph, #{title => "CPU",
                  units => "%",
                  metrics => [{metric_name("cpu"), gauge}]}},

        {graph, #{title => "RAM",
                  units => "%",
                  metrics => [{metric_name("ram"), gauge}]}}] ++

        [{graph, #{title => "Network transmit - " ++ I,
                  units => "bytes",
                  metrics => [{metric_name("nettx." ++ I), gauge}]}} || I <- local_interfaces() ] ++

        [{graph, #{title => "Network receive - " ++ I,
                  units => "bytes",
                  metrics => [{metric_name("netrx." ++ I), gauge}]}} || I <- local_interfaces() ]
                  },
     {group, "MZBench Internals", [
        {graph, #{title => "Mailbox messages",
                  metrics => [{metric_name("message_queue"), gauge}]}},
        {graph, #{title => "Erlang processes",
                  metrics => [{metric_name("process_count"), gauge}]}},
        {graph, #{title => "System metrics report interval",
                  units => "sec",
                  metrics => [{metric_name("interval"), gauge}]}},
        {graph, #{title => "Actual time diff with director",
                  units => "us",
                  metrics => [{metric_name("dir_time_diff"), gauge}]}},
        {graph, #{title => "Time offset at node",
                  units => "us",
                  metrics => [{metric_name("time_offset"), gauge}]}},
        {graph, #{title => "Director ping time",
                  units => "us",
                  metrics => [{metric_name("director_ping"), gauge}]}}
        ]}].

%% gen_server callbacks

init([IntervalMs]) ->
    system_log:info("~p started on node ~p", [?MODULE, node()]),
    _ = spawn_link(fun () -> mailbox_len_reporter(IntervalMs) end),
    erlang:send_after(IntervalMs, self(), trigger),
    {ok, #state{
            net_stat_state = [],
            last_trigger_timestamp = not_available,
            interval_ms = IntervalMs}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(trigger,
    #state{net_stat_state = OldNetStat,
           last_trigger_timestamp = LastTriggerTimestamp,
           interval_ms = IntervalMs} = State) ->

    Now = os:timestamp(),

    LastIntervalDuration = case LastTriggerTimestamp of
        not_available -> IntervalMs;
        _ -> timer:now_diff(Now, LastTriggerTimestamp) / 1000
    end,
    ok = mzb_metrics:notify({metric_name("interval"), gauge}, LastIntervalDuration / 1000),

    case cpu_sup:avg1() of
        {error, LAFailedReason} ->
            system_log:info("cpu_sup:avg1() failed with reason ~p", [LAFailedReason]);
        La1 ->
            ok = mzb_metrics:notify({metric_name("la1"), gauge}, La1 / 256)
    end,

    {TotalMem, AllocatedMem, _} = memsup:get_memory_data(),
    ok = mzb_metrics:notify({metric_name("ram"), gauge}, (AllocatedMem / TotalMem) * 100),

    case os:type() of
        {unix, linux} ->
            case cpu_sup:util() of
                {error, UtilFailedReason} ->
                    system_log:info("cpu_sup:util() failed with reason ~p", [UtilFailedReason]);
                CpuUtil ->
                    ok = mzb_metrics:notify({metric_name("cpu"), gauge}, CpuUtil)
            end;
        % TODO: solaris supports cpu_sup:util too
        _ -> ok
    end,

    NewState = try
        NetStat = network_usage(),
        lists:foreach(fun
            ({"lo", _}) -> ok;
            ({"lo0", _}) -> ok;
            ({IfName, PL}) ->
                case proplists:get_value(IfName, OldNetStat) of
                    undefined -> ok;
                    OldPL ->
                        CurrentRX = proplists:get_value(ibytes, PL),
                        OldRX = proplists:get_value(ibytes, OldPL),
                        CurrentTX = proplists:get_value(obytes, PL),
                        OldTX = proplists:get_value(obytes, OldPL),
                        RXRate = (CurrentRX - OldRX) / (LastIntervalDuration / 1000),
                        TXRate = (CurrentTX - OldTX) / (LastIntervalDuration / 1000),

                        ok = mzb_metrics:notify({metric_name("netrx."++IfName), gauge}, RXRate),
                        ok = mzb_metrics:notify({metric_name("nettx."++IfName), gauge}, TXRate)
                end
        end, NetStat),

        State#state{net_stat_state = NetStat}
    catch
        C:E -> system_log:error("Exception while getting net stats: ~p~nStacktrace: ~p", [{C,E}, erlang:get_stacktrace()]),
        State
    end,

    ok = mzb_metrics:notify({metric_name("process_count"), gauge}, erlang:system_info(process_count)),

    ok = mzb_metrics:notify({metric_name("time_offset"), gauge}, mzb_time:get_offset()),

    try
        T1 = mzb_time:timestamp(),
        DirectorTime = mzb_interconnect:call_director(get_local_timestamp),
        T2 = mzb_time:timestamp(),
        ok = mzb_metrics:notify({metric_name("director_ping"), gauge}, timer:now_diff(T2, T1)),
        ok = mzb_metrics:notify({metric_name("dir_time_diff"), gauge}, (timer:now_diff(T1, DirectorTime) + timer:now_diff(T2, DirectorTime)) div 2)
    catch
        error:not_connected -> ok
    end,

    %system_log:info("System load at ~p: cpu ~p, la ~p, ram ~p", [node(), Cpu, La1, AllocatedMem / TotalMem]),
    erlang:send_after(IntervalMs, self(), trigger),
    {noreply, NewState#state{last_trigger_timestamp = Now}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

mailbox_len_reporter(IntervalMs) ->
    {MailboxSize, _} = lists:foldl(
        fun (P, {Acc, N}) ->
            QueueLen = try
                element(2, erlang:process_info(P, message_queue_len))
            catch
                _:_ -> 0
            end,
            ((N rem 100) == 0) andalso timer:sleep(1),
            {Acc + QueueLen, N + 1}
        end, {0, 0}, erlang:processes()),

    ok = mzb_metrics:notify({metric_name("message_queue"), gauge}, MailboxSize),
    timer:sleep(IntervalMs),
    mailbox_len_reporter(IntervalMs).

metric_name(GaugeName) ->
    metric_name(GaugeName, atom_to_list(node())).

metric_name(GaugeName, Node) when is_atom(Node) ->
    metric_name(GaugeName, atom_to_list(Node));
metric_name(GaugeName, Node) ->
    "systemload." ++ GaugeName ++ "." ++ nodename_str(Node).

nodename_str(Node) when is_atom(Node) ->
    nodename_str(atom_to_list(Node));
nodename_str(NodeStr) ->
    hd(string:tokens(NodeStr, "@")).

network_usage() ->
    try
        network_load_for_arch(os:type())
    catch
        _:E ->
            system_log:error("Error parsing network info: ~p", [E]),
            []
    end.

local_interfaces() ->
    [If || {If, _} <- network_usage(), If /= "lo", If /= "lo0"].

network_load_for_arch({_, darwin}) ->
    parse_darwin_netstat_output(os:cmd("netstat -ibn"));
network_load_for_arch({_, linux}) ->
    parse_linux_netstat_output(os:cmd("netstat -ine")).

parse_darwin_netstat_output(Str) ->
    try
        [Headers|Tokens] = [string:tokens(L, " ") || L <- string:tokens(Str, "\n")],
        Data = lists:filtermap(
            fun (Values) ->
                try lists:zip(Headers, Values) of
                    Proplist ->
                        Name = proplists:get_value("Name", Proplist),
                        IBytes = proplists:get_value("Ibytes", Proplist),
                        OBytes = proplists:get_value("Obytes", Proplist),
                        {true, {Name, [{ibytes, list_to_integer(IBytes)}, {obytes, list_to_integer(OBytes)}]}}
                catch
                    _:_ -> false
                end
            end, Tokens),
        Sorted = lists:usort(Data), % need to remove duplicates
        % MacOS shortens interface names hence long interface names might look the same
        % It is needed to add postfixes so we can distinguish them
        enumerate_shortened(Sorted, [])
    catch
        _:E -> erlang:error({parse_netstat_output_error, E, Str})
    end.

enumerate_shortened([], Res) -> lists:reverse(Res);
enumerate_shortened([{Name, Data} | Tail], Res) ->
    Add = fun (N, I) -> N ++ "-" ++ integer_to_list(I) end,
    {NewTail, Num} = lists:mapfoldl(
        fun ({N, D}, Acc) when N == Name -> {{Add(N, Acc + 1), D}, Acc + 1};
            (E, Acc) -> {E, Acc}
        end, 0, Tail),
    case Num > 0 of
        true  -> enumerate_shortened(NewTail, [{Add(Name, 0), Data}|Res]);
        false -> enumerate_shortened(NewTail, [{Name, Data}|Res])
    end.

parse_linux_netstat_output(Str) ->
    try
        Bin = list_to_binary(Str),
        [_Header, Data] = binary:split(Bin, <<"\n">>),
        Sections = [binary_to_list(B) || B <- binary:split(Data, <<"\n\n">>, [global]), B /= <<>>],
        Parsed = lists:filtermap(
            fun (S) ->
                try {true, parse_netstat_section(S)}
                catch
                    _:_ -> false % Some interfaces don't have that info at all
                end
            end, Sections),
        (Parsed == []) andalso erlang:error(no_info),
        Parsed
    catch
        _:E -> erlang:error({parse_netstat_output_error, E, Str})
    end.

parse_netstat_section(Str) ->
    try parse_netstat_sectionV1(Str)
    catch
        _:_ -> parse_netstat_sectionV2(Str)
    end.

parse_netstat_sectionV1(Str) ->
    Name =
        case re:run(Str, "^(\\S+)\\s", [{capture, [1], list}]) of
            {match, [NameStr]} -> NameStr;
            nomatch -> error({wrong_format, Str})
        end,

    In =
        case re:run(Str, "RX bytes:(\\d+)", [{capture, [1], list}]) of
            {match, [InStr]} -> erlang:list_to_integer(InStr);
            nomatch -> error({wrong_format, Str})
        end,

    Out =
        case re:run(Str, "TX bytes:(\\d+)", [{capture, [1], list}]) of
            {match, [OutStr]} -> erlang:list_to_integer(OutStr);
            nomatch -> error({wrong_format, Str})
        end,
    {Name, [{ibytes, In}, {obytes, Out}]}.

parse_netstat_sectionV2(Str) ->
    Name =
        case re:run(Str, "^(.+):", [{capture, [1], list}]) of
            {match, [NameStr]} -> NameStr;
            nomatch -> error({wrong_format, Str})
        end,

    In =
        case re:run(Str, "RX packets \\d+\\s+bytes (\\d+)", [{capture, [1], list}]) of
            {match, [InStr]} -> erlang:list_to_integer(InStr);
            nomatch -> error({wrong_format, Str})
        end,

    Out =
        case re:run(Str, "TX packets \\d+\\s+bytes (\\d+)", [{capture, [1], list}]) of
            {match, [OutStr]} -> erlang:list_to_integer(OutStr);
            nomatch -> error({wrong_format, Str})
        end,

    {Name, [{ibytes, In}, {obytes, Out}]}.

