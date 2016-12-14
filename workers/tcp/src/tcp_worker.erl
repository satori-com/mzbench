-module(tcp_worker).

-export([initial_state/0,
         metrics/0,
         connect/4,
         connect_sync/4,
         request_sync/3,
         send_n_get_sync/2,
         request/3,
         wait_finish/2,
         sender/2,
         send_n_get/4]).

-define(Options, [
    binary,
    {active, false},
    {buffer, 65536},
    {keepalive, true},
    {reuseaddr, true}
]).

-define(Timeout, 5000).
-define(WaitPollTime, 100).

-record(s, {
    socket = undefined,
    sender = undefined
}).

metrics() ->
    [
        {group, "Summary", [
            {graph, #{title => "Requests",
                      metrics => [{"request.ok", counter}, {"request.error", counter}]}},
            {graph, #{title => "Latencies",
                      units => "msec",
                      metrics => [{"latency", histogram}]}}
        ]}
    ].

initial_state() -> #s{}.

connect(#s{sender = undefined} = State, _Meta, Host, Port) ->
    {nil, State#s{sender = spawn_link(?MODULE, sender, [Host, Port])}}.

request(State, Meta, Message) when is_list(Message) ->
  request(State, Meta, list_to_binary(Message));
request(#s{sender = Pid} = State, _Meta, Message) ->
  Pid ! {request, os:system_time(milli_seconds), Message},
  {nil, State}.

wait_finish(#s{sender = Pid} = State, _Meta) ->
  Pid ! close,
  wait_for_empty_inbox(Pid),
  {nil, State}.

wait_for_empty_inbox(Pid) ->
  QL = element(2, erlang:process_info(Pid, message_queue_len)),
  if QL > 0 -> timer:sleep(?WaitPollTime), wait_for_empty_inbox(Pid);
              true -> ok
      end.

sender(Host, Port) ->
  sender(undefined, Host, Port). % connect(Host, Port)

sender(Socket, Host, Port) ->
  receive
    {request, Start, Message} -> Socket2 = send_n_get(Socket, Host, Port, Message),
                        mzb_metrics:notify({"latency", histogram}, os:system_time(milli_seconds) - Start),
                        sender(Socket2, Host, Port);
    close -> if Socket =/= undefined -> gen_tcp:close(Socket); true -> ok end,
             sender(undefined, Host, Port);
    Message -> lager:error("Unexpected message ~p", [Message]), sender(Socket, Host, Port)
  end.

connect(Host, Port) ->
  case gen_tcp:connect(Host, Port, ?Options) of
      {ok, Socket} -> Socket;
      _ -> undefined end.

send_n_get(Socket, Host, Port, Message) ->
  Socket2 = if Socket == undefined -> connect(Host, Port); true -> Socket end,
  if Socket2 == undefined -> mzb_metrics:notify({"request.error", counter}, 1), undefined;
     true -> gen_tcp:send(Socket2, Message),
             case gen_tcp:recv(Socket2, 0, ?Timeout) of
                {ok, _Binary} -> mzb_metrics:notify({"request.ok", counter}, 1), Socket2;
                _ -> gen_tcp:close(Socket2), mzb_metrics:notify({"request.error", counter}, 1),
                undefined end
    end.

%% The following functions are synchronous
%% They are not recommended to use if you have big difference between percentile levels
%% due to coordinated omission problem

connect_sync(State, _Meta, Host, Port) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, ?Options),
    {nil, State#s{socket = Socket}}.

request_sync(State, Meta, Message) when is_list(Message) ->
  request_sync(State, Meta, list_to_binary(Message));
request_sync(#s{socket = Socket} = State, _Meta, Message) ->
  {Latency, E} = timer:tc(?MODULE, send_n_get_sync, [Socket, Message]),
  mzb_metrics:notify({"latency", histogram}, Latency div 1000),
  case E of
      ok -> mzb_metrics:notify({"request.ok", counter}, 1);
      E -> lager:error("Request sync error: ~p", [E]),
           mzb_metrics:notify({"request.error", counter}, 1)
  end,
  {nil, State}.

send_n_get_sync(Socket, Message) ->
  gen_tcp:send(Socket, Message),
    case gen_tcp:recv(Socket, 0, ?Timeout) of
        {ok, _Binary} -> ok;
        E -> E
    end.