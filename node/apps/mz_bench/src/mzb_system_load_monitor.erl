-module(mzb_system_load_monitor).

-behaviour(gen_server).

%% API
-export([start_link/0,
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
    last_trigger_timestamp :: erlang:timestamp() | not_available
    }).

interval() -> 10000. % ten seconds

%% API functions

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

metric_names(Nodes) ->
    [
       [{metric_name(T, N), gauge} || N <- Nodes]
       || T <- ["la1", "cpu", "ram", "nettx", "netrx", "interval"]
    ].

%% gen_server callbacks

init([]) ->
    lager:info("~p started on node ~p", [?MODULE, node()]),
    erlang:send_after(interval(), self(), trigger),
    {ok, #state{last_rx_bytes = not_available,
        last_tx_bytes = not_available,
        last_trigger_timestamp = not_available}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(trigger,
    #state{last_rx_bytes = LastRXBytes,
        last_tx_bytes = LastTXBytes,
        last_trigger_timestamp = LastTriggerTimestamp} = State) ->

    Now = os:timestamp(),

    LastIntervalDuration = case LastTriggerTimestamp of
        not_available -> interval();
        _ -> timer:now_diff(Now, LastTriggerTimestamp)
    end,
    ok = mzb_metrics:notify({metric_name("interval"), gauge}, LastIntervalDuration / 1000),

    case cpu_sup:avg1() of
        {error, LAFailedReason} ->
            lager:info("cpu_sup:avg1() failed with reason ~p", [LAFailedReason]);
        La1 ->
            ok = mzb_metrics:notify({metric_name("la1"), gauge}, La1 / 256)
    end,

    {TotalMem, AllocatedMem, _} = memsup:get_memory_data(),
    ok = mzb_metrics:notify({metric_name("ram"), gauge}, AllocatedMem / TotalMem),

    case os:type() of
        {unix, linux} ->
            case cpu_sup:util() of
                {error, UtilFailedReason} ->
                    lager:info("cpu_sup:util() failed with reason ~p", [UtilFailedReason]);
                CpuUtil ->
                    ok = mzb_metrics:notify({metric_name("cpu"), gauge}, CpuUtil)
            end;
        % TODO: solaris supports cpu_sup:util too
        _ -> ok
    end,

    NewState = try
        NetStatsString = os:cmd(mzb_utility:expand_filename("~/mz/mz_bench/bin/report_network_usage.py")),
        {ok, Tokens, _} = erl_scan:string(NetStatsString),
        {ok, NetStats} = erl_parse:parse_term(Tokens),

        lager:info("NetStatsString: ~p, NetStats: ~p", [NetStatsString, NetStats]),

        CurrentTXBytes = lists:sum([maps:get(tx_bytes, Info) || Info <- NetStats]),
        CurrentRXBytes = lists:sum([maps:get(rx_bytes, Info) || Info <- NetStats]),

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

        #state{last_rx_bytes = CurrentRXBytes, last_tx_bytes = CurrentTXBytes}
    catch
        C:E -> lager:error("Exception while getting net stats: ~p", [{C,E}]),
        State
    end,

    %lager:info("System load at ~p: cpu ~p, la ~p, ram ~p", [node(), Cpu, La1, AllocatedMem / TotalMem]),
    erlang:send_after(interval(), self(), trigger),
    {noreply, NewState#state{last_trigger_timestamp = Now}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions


metric_name(GaugeName) ->
    metric_name(GaugeName, atom_to_list(node())).

metric_name(GaugeName, Node) when is_atom(Node) ->
    metric_name(GaugeName, atom_to_list(Node));
metric_name(GaugeName, Node) ->
    "systemload." ++ GaugeName ++ "." ++ string:join(string:tokens(Node, "@"), ".").
