-module(mzb_interconnect).

-behaviour(gen_server).

%% API
-export([
    start_link/1,
    accept_connection/4,
    nodes/0,
    set_director/1,
    set_handler/1,
    call/2,
    call/3,
    cast/2,
    call_director/1,
    call_director/2,
    cast_director/1,
    multi_call/2,
    multi_call/3,
    abcast/2,
    monitor/2,
    demonitor/1,
    demonitor/2,
    handle/1,
    get_director/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(s, {
    role = worker,
    nodes = #{},
    remote_monitors = #{},
    local_monitors = #{},
    connection_monitors = #{},
    director = undefined,
    handler = undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Handler) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Handler], []).

accept_connection(Host, Role, Owner, Sender) ->
    gen_server:call(?MODULE, {accept, Host, Role, Owner, Sender}).

nodes() ->
    try
        gen_server:call(?MODULE, nodes, infinity)
    catch
        exit:{noproc, _} -> []
    end.

% Mark current node as a director and set the list of hosts to connect to
set_director(Hosts) ->
    gen_server:call(?MODULE, {set_director, Hosts}).

set_handler(Handler) ->
    gen_server:call(?MODULE, {set_handler, Handler}).

cast_director(Msg) ->
    gen_server:cast(?MODULE, {cast_director, Msg}).

call_director(Req) -> call_director(Req, infinity).
call_director(Req, Timeout) ->
    case gen_server:call(?MODULE, {call_director, Req}, Timeout) of
        {ok, Res} -> Res;
        {exception, {C, E, ST}} -> erlang:raise(C, E, ST)
    end.

call(Node, Req) -> call(Node, Req, infinity).

call(Node, Req, Timeout) ->
    case gen_server:call(?MODULE, {call, Node, Req}, Timeout) of
        {ok, Res} -> Res;
        {exception, {C, E, ST}} -> erlang:raise(C, E, ST)
    end.

cast(Node, Msg) ->
    gen_server:cast(?MODULE, {cast, Node, Msg}).

multi_call(Nodes, Req) -> multi_call(Nodes, Req, infinity).

multi_call(Nodes, Req, Timeout) ->
    L = mzb_lists:pmap(fun (N) ->
            try call(N, Req, Timeout) of
                R -> {ok, {N, R}}
            catch
                _:Reason ->
                    lager:error("Call ~p to ~p failed with reason: ~p~n~p", [Req, N, Reason, erlang:get_stacktrace()]),
                    {bad, N}
            end
        end, Nodes),
    {[V || {ok, V} <- L], [V || {bad, V} <- L]}.

abcast(Nodes, Msg) ->
    gen_server:cast(?MODULE, {abcast, Nodes, Msg}).

monitor(process, Pid) ->
    gen_server:call(?MODULE, {monitor, self(), Pid}, infinity).

demonitor({interconnect_monitor, Pid, Ref}) ->
    gen_server:call(?MODULE, {demonitor, Pid, Ref}, infinity).

demonitor({interconnect_monitor, _, _} = Ref, [flush]) ->
    ?MODULE:demonitor(Ref),
    receive
        {'DOWN', Ref, _, _, _} -> ok
    after 0 -> ok
    end.

handle(Msg) ->
    gen_server:cast(?MODULE, {from_remote, Msg}).

get_director() ->
    gen_server:call(?MODULE, get_director).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Handler]) ->
    {ok, #s{nodes = #{}, connection_monitors = #{}, handler = Handler}}.

handle_call({accept, Node, Role, Owner, Sender}, _From, #s{nodes = Nodes, connection_monitors = Mons, role = MyRole} = State) ->
    system_log:info("Connection to ~p established", [Node]),
    Ref = erlang:monitor(process, Owner),
    Director = case Role of
            director -> Node;
            worker -> State#s.director
        end,
    {reply, {ok, MyRole}, State#s{nodes = maps:put(Node, Sender, Nodes),
                                  connection_monitors = maps:put(Ref, Node, Mons),
                                  director = Director}};

handle_call({set_director, Hosts}, _From, State) ->
    _ = [mzb_interconnect_sup:start_client(Host, Port, director) || {Host, Port} <- Hosts],
    {reply, ok, State#s{role = director}};

handle_call({set_handler, Handler}, _From, State) ->
    {reply, ok, State#s{handler = Handler}};

handle_call(nodes, _From, #s{nodes = Nodes} = State) ->
    {reply, maps:keys(Nodes), State};

handle_call({call, Node, Req}, From, State) when Node == node() ->
    try handle_message(Req, fun (R) -> gen_server:reply(From, {ok, R}), ok end, State) of
        noreply -> {noreply, State};
        {reply, Res} -> {reply, {ok, Res}, State}
    catch
        C:E ->
            {reply, {exception, {C,E,erlang:get_stacktrace()}}, State}
    end;

handle_call({call, Node, Req}, From, State) ->
    send_to(Node, {call, {node(), From}, Req}, State),
    {noreply, State};

handle_call({call_director, Req}, From, #s{role = director} = State) ->
    try handle_message(Req, fun (R) -> gen_server:reply(From, {ok, R}), ok end, State) of
        noreply -> {noreply, State};
        {reply, Res} -> {reply, {ok, Res}, State}
    catch
        C:E ->
            {reply, {exception, {C,E,erlang:get_stacktrace()}}, State}
    end;

handle_call({call_director, Req}, From, #s{role = worker, director = Director} = State) ->
    send_to(Director, {call, {node(), From}, Req}, State),
    {noreply, State};

handle_call({monitor, Owner, Pid}, _From, State) when node(Pid) == node() ->
    Ref = erlang:monitor(process, Pid),
    {reply, {interconnect_monitor, Pid, Ref},
            add_lmon(Ref, Owner, Pid, add_rmon(Ref, Owner, Pid, State))};
handle_call({monitor, Owner, Pid}, From, State) ->
    send_to(node(Pid), {monitor, {node(), From}, Owner, Pid}, State),
    {noreply, State};

handle_call({demonitor, Pid, Ref}, _From, State) when node(Pid) == node() ->
    catch erlang:demonitor(Ref, [flush]),
    {reply, ok, rm_lmon(Ref, rm_rmon(Pid, Ref, State))};

handle_call({demonitor, Pid, Ref}, _From, #s{} = State) ->
    send_to(node(Pid), {demonitor, {}, Pid, Ref}, State),
    {reply, ok, rm_rmon(Pid, Ref, State)};

handle_call(get_director, _From, #s{role = director} = State) ->
    {reply, node(), State};

handle_call(get_director, _From, #s{role = worker, director = Director} = State) ->
    {reply, Director, State};

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({cast, Node, Msg}, State) when Node == node() ->
    _ = (catch handle_message(Msg, State)),
    {noreply, State};

handle_cast({cast, Node, Msg}, State) ->
    send_to(Node, {cast, Msg}, State),
    {noreply, State};

handle_cast({abcast, Nodes, Msg}, State) ->
    handle_abcast(Nodes, Msg, State),
    {noreply, State};

handle_cast({cast_director, Msg}, #s{role = director} = State) ->
    _ = (catch handle_message(Msg, State)),
    {noreply, State};

handle_cast({cast_director, Msg}, #s{role = worker, director = DirNode} = State) ->
    send_to(DirNode, {cast, Msg}, State),
    {noreply, State};

handle_cast({from_remote, {cast, Msg}}, State) ->
    _ = (catch handle_message(Msg, State)),
    {noreply, State};

handle_cast({from_remote, {abcast, Nodes, Msg}}, State) ->
    handle_abcast(Nodes, Msg, State),
    {noreply, State};

handle_cast({from_remote, {call, {FromNode, From}, Msg}}, State) ->
    ReplyFun = fun (R) -> send_to(FromNode, {reply, From, {ok, R}}, State), ok end,
    try handle_message(Msg, ReplyFun, State) of
        noreply -> ok;
        {reply, Res} -> ReplyFun(Res)
    catch
        C:E ->
            send_to(FromNode, {reply, From, {exception, {C,E,erlang:get_stacktrace()}}}, State)
    end,
    {noreply, State};

handle_cast({from_remote, {transit, To, Msg}}, #s{role = director} = State) ->
    send_to(To, Msg, State),
    {noreply, State};

handle_cast({from_remote, {transit, To, Msg}}, State) ->
    system_log:error("Transit message for ~p on non director node: ~p~nMessage: ~p", [To, node(), Msg]),
    {noreply, State};

handle_cast({from_remote, {reply, From, Res}}, State) ->
    gen_server:reply(From, Res),
    {noreply, State};

handle_cast({from_remote, {monitor, {FromNode, From}, Owner, Pid}}, State) ->
    Ref = erlang:monitor(process, Pid),
    send_to(FromNode, {monitor_res, From, {node(), Owner, Pid, Ref}}, State),
    {noreply, add_lmon(Ref, Owner, Pid, State)};

handle_cast({from_remote, {monitor_res, From, {_Node, Owner, Pid, Ref}}}, State) ->
    gen_server:reply(From, {interconnect_monitor, Pid, Ref}),
    {noreply, add_rmon(Ref, Owner, Pid, State)};

handle_cast({from_remote, {demonitor, _Pid, Ref}}, State) ->
    erlang:demonitor(Ref, [flush]),
    {noreply, rm_lmon(Ref, State)};

handle_cast({from_remote, {down, _Node, Owner, Ref, Pid, Reason}}, State) ->
    Owner ! {'DOWN', {interconnect_monitor, Pid, Ref}, process, Pid, Reason},
    {noreply, rm_rmon(Pid, Ref, State)};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_message(Msg, State) -> handle_message(Msg, fun (_) -> ok end, State).

handle_message(Msg, _, #s{handler = undefined}) ->
    system_log:warning("Ignoring msg due to undefined handler~nMessage: ~p", [Msg]),
    noreply;
handle_message(Msg, ReplyFun, #s{handler  = Handler}) ->
    try Handler(Msg, ReplyFun) of
        {reply, Res} -> {reply, Res};
        noreply -> noreply
    catch
        _:E ->
            system_log:error("Handler for ~p has crashed at ~p: ~p~n~p", [Msg, node(), E, erlang:get_stacktrace()]),
            noreply
    end.

handle_info({'DOWN', Ref, _, _, Reason}, #s{nodes = Nodes,
                                            connection_monitors = Mons,
                                            remote_monitors = RMons,
                                            local_monitors = LMons} = State) ->
    case maps:find(Ref, Mons) of
        {ok, Node} ->
            system_log:warning("Node ~p disconnected: ~p", [Node, Reason]),

            NewState = lists:foldl(
                fun ({R, {Owner, Pid}}, Acc) ->
                    Owner ! {'DOWN', {interconnect_monitor, Pid, R}, process, Pid, noconnection},
                    rm_rmon(Pid, R, Acc)
                end, State, maps:to_list(mzb_bc:maps_get(Node, RMons, #{}))),

            {noreply, NewState#s{nodes = maps:remove(Node, Nodes),
                                 connection_monitors = maps:remove(Ref, Mons)}};
        error ->
            case maps:find(Ref, LMons) of
                {ok, {Owner, Pid}} when node(Owner) == node() ->
                    Owner ! {'DOWN', {interconnect_monitor, Pid, Ref}, process, Pid, Reason},
                    {noreply, rm_lmon(Ref, rm_rmon(Pid, Ref, State))};
                {ok, {Owner, Pid}} ->
                    send_to(node(Owner), {down, node(), Owner, Ref, Pid, Reason}, State),
                    {noreply, rm_lmon(Ref, State)};
                error ->
                    {noreply, State}
            end
    end;

handle_info(_Info, State) ->
    {noreply, State}.

handle_abcast(Nodes, Msg, #s{role = director} = State) ->
    lists:foreach(
        fun (N) when N == node() -> catch handle_message(Msg, State);
            (N) -> send_to(N, {cast, Msg}, State)
        end, Nodes);
handle_abcast(Nodes, Msg, #s{role = worker, director = Director} = State) ->
    NewNodes = case lists:member(node(), Nodes) of
        true  ->
            catch handle_message(Msg, State),
            lists:delete(node(), Nodes);
        false ->
            Nodes
    end,
    case NewNodes of
        [] -> ok;
        _ -> send_to(Director, {abcast, NewNodes, Msg}, State)
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_to(To, Msg, #s{role = Role, nodes = Nodes, director = Director}) ->
    case maps:find(To, Nodes) of
        {ok, Sender} -> Sender(Msg);
        error when Role == worker ->
            Sender = maps:get(Director, Nodes),
            Sender({transit, To, Msg});
        error when Role == director ->
            system_log:warning("Skip msg for unknown node: ~p~nMessage: ~p", [To, Msg]),
            ok
    end.


add_lmon(Ref, Owner, Pid, #s{local_monitors = LMons} = State) ->
    State#s{local_monitors = maps:put(Ref, {Owner, Pid}, LMons)}.
rm_lmon(Ref, #s{local_monitors = LMons} = State) ->
    State#s{local_monitors = maps:remove(Ref, LMons)}.

add_rmon(Ref, Owner, Pid, #s{remote_monitors = RMons} = State) ->
    Node = node(Pid),
    NodeMons = mzb_bc:maps_get(Node, RMons, #{}),
    State#s{remote_monitors = maps:put(Node, maps:put(Ref, {Owner, Pid}, NodeMons), RMons)}.
rm_rmon(Pid, Ref, #s{remote_monitors = RMons} = State) ->
    Node = node(Pid),
    NodeMons = mzb_bc:maps_get(Node, RMons, #{}),
    State#s{remote_monitors = maps:put(Node, maps:remove(Ref, NodeMons), RMons)}.
