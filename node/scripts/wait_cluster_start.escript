#!/usr/bin/env escript
-mode(compile).

-export([stop/2]).

main([TimeoutStr | NodesStr]) ->
    Timeout =
        try erlang:list_to_integer(TimeoutStr) of
            TO when TO > 0 -> TO;
            _ -> bad_arg("timeout", TimeoutStr)
        catch
            _:_ -> bad_arg("timeout", TimeoutStr)
        end,

    Nodes = [erlang:list_to_atom(N) || N <- NodesStr],
    Nodes == [] andalso bad_arg("host list", "(empty)"),
%   Hostnames = [lists:last(string:tokens(atom_to_list(Node), "@")) || Node <- Nodes],

    timer:apply_after(Timeout, ?MODULE, stop, [self(), timeout]),

    init_net_kernel(),

%    ok = ensure_hostnames_are_resolvable(Hostnames),
%    ok = ensure_hostnames_are_reachable_from_each_other(Hostnames),
    ok = is_nodes_ready(Nodes),

    connect_nodes(Nodes);

main(_) ->
    usage().

ensure_hostnames_are_reachable_from_each_other(Hostnames) ->
    lists:foreach(
        fun({From, To}) ->
            SSHOptions = "-o StrictHostKeyChecking=no",
            Cmd = lists:flatten(io_lib:format("ssh ~s ~s \"ping -qc1 ~s\"",
                [SSHOptions, From, To])),
            {Code, Output} = cmd(Cmd),
            case Code of
                0 -> ok;
                _ ->
                    error({cant_reach, To, from, From, Code, Output})
            end
        end,
        [{From, To} || From <- Hostnames, To <- Hostnames, From =/= To]).

ensure_hostnames_are_resolvable(Nodes) ->
    lists:foreach(
        fun(Node) ->
            case inet:gethostbyname(Node) of
                {ok, _} -> ok;
                Error -> error({cant_resolve_hostname, Node, Error})
            end
        end,
        Nodes).

is_nodes_ready([]) -> ok;
is_nodes_ready(Nodes) ->
    Refs = lists:map(fun spawn_is_ready/1, Nodes),
    Answers = lists:map(fun receive_answer/1, Refs),
    BadAnswers = lists:filter(fun (A) -> not element(2, A) end, Answers),
    {NewNodes, _} = lists:unzip(BadAnswers),
    timer:sleep(1000),
    is_nodes_ready(NewNodes).

bad_arg(Name, Val) ->
    io:format("Invalid ~s: ~s~n", [Name, Val]),
    usage().

usage() ->
    io:format("Usage: ~s Timeout Host1 [ Host2 [ Host3 ...]]~n", [escript:script_name()]),
    halt(1).

nodename_gen() ->
    {N1,N2,N3} = erlang:now(),
    Str = lists:flatten(io_lib:format("~p-~p~p", [N1,N2,N3])),
    erlang:list_to_atom(Str).

init_net_kernel() ->
    ok = application:start(inets),
    {ok, _} = net_kernel:start([nodename_gen(), shortnames]),
    ok.

spawn_is_ready(Node) ->
    Ref = erlang:make_ref(),
    Self = self(),
    spawn(
        fun () ->
            Self ! {Ref, {Node, is_node_ready(Node)}}
        end),
    Ref.

is_node_ready(Node) ->
    Res = try
        case net_kernel:hidden_connect_node(Node) of
            true  ->
                case rpc:call(Node, mzb_bench_sup, is_ready, []) of
                    {badrpc, Reason} -> false;
                    Reply -> Reply
                end;
            false -> false
        end
    catch
        _:E ->
            io:format("is_node_ready exception: ~p~n~p", [E, erlang:get_stacktrace()]),
            false
    end,

    Res == true andalso io:format("Node ~p is ready~n", [Node]),

    Res.

receive_answer(Ref) ->
    receive
        {Ref, Res} -> Res
    end.

stop(Pid, Reason) ->
    io:format("unexpected stop: ~s~n", [Reason]),
    erlang:exit(Pid, Reason),
    halt(1).

connect_nodes(Nodes = [N | _]) ->
    ConnectedNodes = rpc:call(N, mzb_bench_sup, connect_nodes, [Nodes]),
    case Nodes -- ConnectedNodes of
        [] ->
            io:format("All nodes are connected~n");
        List ->
            io:format("Several nodes are not connected: ~p~n", [List]),
            halt(1)
    end.

cmd(Command) ->
    Port = open_port({spawn, Command}, [eof, exit_status, {line, 255}]),
    Loop = fun Loop(Output) ->
        receive
            {Port, {data, {eol, Data}}} ->
                Loop([Data | Output]);
            {Port, {data, {noeol, Data}}} ->
                Loop([Data | Output]);
            {Port, eof} ->
                Port ! {self(), close},
                Loop(Output);
            stop ->
                Port ! {self(), close},
                Loop(Output);
            {Port, closed} ->
                receive
                    {Port, {exit_status, Code}} ->
                        {Code, lists:reverse(Output)}
                end
        end end,
    Loop([]).
