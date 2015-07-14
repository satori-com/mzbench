-module(mzb_escript_shared).

-define(MZBENCH_NODE,  "mzb_director").

-export([nodename_gen/0, hostname/0, read_env/1, convert/1, start_and_connect/1]).

nodename_gen() ->
    {N1,N2,N3} = erlang:now(),
    Str = lists:flatten(io_lib:format("~p-~p~p", [N1,N2,N3])),
    erlang:list_to_atom(Str).

hostname() ->
    [_, H] = string:tokens(erlang:atom_to_list(node()), "@"),
    H.

read_env(EnvFile) ->
    {BinEnv} = case EnvFile of
                    undefined -> {[]};
                    _  ->
                        {ok, BinJSON} = file:read_file(EnvFile),
                        jiffy:decode(BinJSON)
                end,
    lists:map(fun ({K, V}) -> {convert(K), convert(V)};
                        (X) -> X
                    end, BinEnv).

convert(Binary) when is_binary(Binary) -> binary_to_list(Binary);
convert(X) -> X.

start_and_connect(Node) ->
    [] = os:cmd("epmd -daemon"),
    ok = application:start(inets),
    {ok, _} = net_kernel:start([nodename_gen(), shortnames]),
    true == net_kernel:hidden_connect_node(Node) orelse erlang:error({connect_failed, Node}),
    Node.
