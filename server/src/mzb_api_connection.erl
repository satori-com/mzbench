-module(mzb_api_connection).

-export([start_link/4,
         send_message/2,
         wait_close/2]).

start_link(Purpose, Hosts, Port, Dispatcher) ->
    Self = self(),
    Pids = lists:map(fun (Host) ->
        Pid = spawn_link(fun () ->
            try gen_tcp:connect(Host, Port, [{active, false}, {packet, 4}, binary]) of
                {ok, Socket} ->
                    lager:info("Connection is started for ~p on ~s", [Purpose, Host]),
                    Self ! {self(), connected, Socket},
                    process_data(Purpose, Host, Socket, Dispatcher);
                {error, Reason} ->
                    Self ! {self(), failed, Reason}
            catch
                C:E ->
                    ST = erlang:get_stacktrace(),
                    Self ! {self(), failed, {C, E, ST}}
            end
        end),
        {Pid, Purpose, Host}
    end, Hosts),
    wait_collectors(Pids, []).

send_message({_, Socket, _, _}, Message) ->
    gen_tcp:send(Socket, erlang:term_to_binary(Message)).

wait_close({Pid, _, Purpose, Host}, Timeout) ->
    Ref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref, process, _, _Info} -> ok
    after Timeout ->
        erlang:error({connection_close_timeout, Purpose, Host})
    end.

wait_collectors([], Acc) -> Acc;
wait_collectors([{Pid, Purpose, Host} | Tail], Acc) ->
    receive
        {Pid, connected, Socket} -> wait_collectors(Tail, [{Pid, Socket, Purpose, Host} | Acc]);
        {Pid, failed, Reason} ->
            lager:error("Connection '~p' is failed to start on host ~s with reason ~p", [Purpose, Host, Reason]),
            erlang:error({catch_collector_connect_failed, Host, Reason})
    after 30000 ->
        lager:error("Connection '~p' is timed-out to start on host ~s", [Purpose, Host]),
        erlang:error({catch_collector_connect_timedout, Host})
    end.

process_data(Purpose, Host, Socket, Dispatcher) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            ok = Dispatcher({message, Data}),
            process_data(Purpose, Host, Socket, Dispatcher);
        {error, closed} ->
            lager:info("Connection '~p' is closed on host ~s", [Purpose, Host]),
            Dispatcher({error, closed});
        {error, Reason} ->
            lager:error("Connection '~p' is failed on host ~s with reason ~p", [Purpose, Host, Reason]),
            Dispatcher({error, Reason})
    end.
