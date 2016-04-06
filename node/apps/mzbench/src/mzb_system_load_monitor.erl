-module(mzb_system_load_monitor).

-behaviour(gen_server).

%% API
-export([
    start_link/1,
    metric_names/1
    ]).

%% gen_server callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3
		]).

-record(state, {
    last_rx_bytes :: integer() | not_available,
    last_tx_bytes :: integer() | not_available,
    last_trigger_timestamp :: erlang:timestamp() | not_available,
    interval_ms :: undefined | integer()
    }).

%% API functions

start_link(IntervalMs) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [IntervalMs], []).

metric_names(Nodes) ->
    [{group, "System Load", [
        {graph, #{title => "Load average",
                  units => "la1",
                  metrics => [{metric_name("la1", N), gauge} || N <- Nodes]}},

        {graph, #{title => "CPU",
                  units => "%",
                  metrics => [{metric_name("cpu", N), gauge} || N <- Nodes]}},

        {graph, #{title => "RAM",
                  units => "%",
                  metrics => [{metric_name("ram", N), gauge} || N <- Nodes]}},

        {graph, #{title => "Network transmit",
                  units => "bytes",
                  metrics => [{metric_name("nettx", N), gauge} || N <- Nodes]}},

        {graph, #{title => "Network receive",
                  units => "bytes",
                  metrics => [{metric_name("netrx", N), gauge} || N <- Nodes]}}
                  ]},
     {group, "MZBench Internals", [
        {graph, #{title => "Mailbox messages",
                  metrics => [{metric_name("message_queue", N), gauge} || N <- Nodes]}},
        {graph, #{title => "Erlang processes",
                  metrics => [{metric_name("process_count", N), gauge} || N <- Nodes]}},
        {graph, #{title => "System metrics report interval",
                  units => "sec",
                  metrics => [{metric_name("interval", N), gauge} || N <- Nodes]}}
        ]}].

%% gen_server callbacks

init([IntervalMs]) ->
    system_log:info("~p started on node ~p", [?MODULE, node()]),
    _ = spawn_link(fun () -> mailbox_len_reporter(IntervalMs) end),
    erlang:send_after(IntervalMs, self(), trigger),
    {ok, #state{
            last_rx_bytes = not_available,
            last_tx_bytes = not_available,
            last_trigger_timestamp = not_available,
            interval_ms = IntervalMs}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(trigger,
    #state{last_rx_bytes = LastRXBytes,
        last_tx_bytes = LastTXBytes,
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
        {CurrentRXBytes, CurrentTXBytes} = network_usage(),

        case LastRXBytes of
            not_available -> ok;
            _ ->
                RXRate = (CurrentRXBytes - LastRXBytes) / (LastIntervalDuration / 1000),
                ok = mzb_metrics:notify({metric_name("netrx"), gauge}, RXRate)
        end,

        case LastTXBytes of
            not_available -> ok;
            _ ->
                TXRate = (CurrentTXBytes - LastTXBytes) / (LastIntervalDuration / 1000),
                ok = mzb_metrics:notify({metric_name("nettx"), gauge}, TXRate)
        end,

        State#state{last_rx_bytes = CurrentRXBytes, last_tx_bytes = CurrentTXBytes}
    catch
        C:E -> system_log:error("Exception while getting net stats: ~p~nStacktrace: ~p", [{C,E}, erlang:get_stacktrace()]),
        State
    end,

    ok = mzb_metrics:notify({metric_name("process_count"), gauge}, erlang:system_info(process_count)),

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
    "systemload." ++ GaugeName ++ "." ++ mzb_utility:hostname_str(Node).

network_usage() ->
    try
        network_load_for_arch(os:type())
    catch
        _:E ->
            lager:error("Error parsing network info: ~p", [E]),
            {0, 0}
    end.

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
                        {true, [{H, V} || {H, V} <- Proplist,
                                               N <- ["Name", "Ibytes", "Obytes"],
                                          N == H]}
                catch
                    _:_ -> false
                end
            end, Tokens),
        lists:foldl(
            fun ([_, {"Ibytes", I}, {"Obytes", O}], {IAcc, OAcc}) ->
                {erlang:list_to_integer(I) + IAcc, erlang:list_to_integer(O) + OAcc}
            end, {0, 0}, lists:usort(Data))
    catch
        _:E -> erlang:error({parse_netstat_output_error, E, Str})
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
        In  = lists:sum([I || {_, I, _} <- Parsed]),
        Out = lists:sum([O || {_, _, O} <- Parsed]),
        {In, Out}
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
    {Name, In, Out}.

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
    {Name, In, Out}.

