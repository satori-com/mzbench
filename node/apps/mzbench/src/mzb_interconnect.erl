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
    handle/1
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
                _:_ -> {bad, N}
            end
        end, Nodes),
    {[V || {ok, V} <- L], [V || {bad, V} <- L]}.

abcast(Nodes, Msg) ->
    [cast(N, Msg) || N <- Nodes].

monitor(process, Pid) when node(Pid) == node() ->
    erlang:monitor(process, Pid);
monitor(process, Pid) ->
    gen_server:call(?MODULE, {monitor, self(), Pid}).

demonitor({interconnect_monitor, Pid, Ref}) ->
    gen_server:call(?MODULE, {demonitor, Pid, Ref});
demonitor(Ref) ->
    erlang:demonitor(Ref).

demonitor({interconnect_monitor, Pid, Ref}, [flush]) ->
    ?MODULE:demonitor({interconnect_monitor, Pid, Ref}),
    receive
        {'DOWN', Ref, _, _, _} -> ok
    after 0 -> ok
    end;
demonitor(Ref, [flush]) ->
    erlang:demonitor(Ref, [flush]).

handle(Msg) ->
    system_log:info("Message at ~p: ~p", [node(), Msg]),
    gen_server:cast(?MODULE, {from_remote, Msg}).

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

handle_call({call, Node, Req}, _From, State) when Node == node() ->
    try handle_message(Req, State) of
        ignore -> {noreply, State};
        {res, Res} -> {reply, {ok, Res}, State}
    catch
        C:E ->
            {reply, {exception, {C,E,erlang:get_stacktrace()}}, State}
    end;

handle_call({call, Node, Req}, From, State) ->
    send_to(Node, {call, {node(), From}, Req}, State),
    {noreply, State};

handle_call({call_director, Req}, _From, #s{role = director} = State) ->
    try handle_message(Req, State) of
        ignore -> {noreply, State};
        {res, Res} -> {reply, {ok, Res}, State}
    catch
        C:E ->
            {reply, {exception, {C,E,erlang:get_stacktrace()}}, State}
    end;

handle_call({call_director, Req}, From, #s{role = worker, director = Director} = State) ->
    send_to(Director, {call, {node(), From}, Req}, State),
    {noreply, State};

handle_call({monitor, Owner, Pid}, From, State) ->
    send_to(node(Pid), {monitor, {node(), From}, Owner, Pid}, State),
    {noreply, State};

handle_call({demonitor, Pid, Ref}, _From, #s{remote_monitors = RMons} = State) ->
    Node = node(Pid),
    NodeMons = mzb_bc:maps_get(Node, RMons, #{}),
    send_to(node(Pid), {demonitor, Pid, Ref}, State),
    {noreply, State#s{remote_monitors = maps:put(Node, maps:remove(Ref, NodeMons), RMons)}};

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({cast, Node, Msg}, State) when Node == node() ->
    _ = (catch handle_message(Msg, State)),
    {noreply, State};

handle_cast({cast, Node, Msg}, State) ->
    send_to(Node, {cast, Msg}, State),
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

handle_cast({from_remote, {call, {FromNode, From}, Msg}}, State) ->
    try handle_message(Msg, State) of
        ignore -> ok;
        {res, Res} -> send_to(FromNode, {reply, From, {ok, Res}}, State)
    catch
        C:E ->
            send_to(FromNode, {reply, From, {exception, {C,E,erlang:get_stacktrace()}}}, State)
    end,
    {noreply, State};

handle_cast({from_remote, {transit, To, Msg}}, #s{role = director} = State) ->
    send_to(To, Msg, State),
    {noreply, State};

handle_cast({from_remote, {transit, To, Msg}}, #{} = State) ->
    system_log:error("Transit message for ~p on non director node: ~p~nMessage: ~p", [To, node(), Msg]),
    {noreply, State};

handle_cast({from_remote, {reply, From, Res}}, State) ->
    gen_server:reply(From, Res),
    {noreply, State};

handle_cast({from_remote, {monitor, {FromNode, From}, Owner, Pid}}, #s{local_monitors = LMons} = State) ->
    Ref = erlang:monitor(process, Pid),
    send_to(FromNode, {monitor_res, From, {node(), Owner, Pid, Ref}}, State),
    {noreply, State#s{local_monitors = maps:put(Ref, {Owner, Pid}, LMons)}};

handle_cast({from_remote, {monitor_res, From, {Node, Owner, Pid, Ref}}}, #s{remote_monitors = RMons} = State) ->
    NodeMons = mzb_bc:maps_get(Node, RMons, #{}),
    gen_server:reply(From, {interconnect_monitor, Pid, Ref}),
    {noreply, State#s{remote_monitors = maps:put(Node, maps:put(Ref, {Owner, Pid}, NodeMons), RMons)}};

handle_cast({from_remote, {demonitor, _Pid, Ref}}, #s{local_monitors = LMons} = State) ->
    erlang:demonitor(Ref, [flush]),
    {noreply, State#s{local_monitors = maps:remove(Ref, LMons)}};

handle_cast({from_remote, {down, Node, Owner, Ref, Pid, Reason}}, #s{remote_monitors = RMons} = State) ->
    NodeMons = mzb_bc:maps_get(Node, RMons, #{}),
    Owner ! {'DOWN', {interconnect_monitor, Pid, Ref}, process, Pid, Reason},
    {noreply, State#s{remote_monitors = maps:put(Node, maps:remove(Ref, NodeMons), RMons)}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_message(Msg, #s{handler = undefined}) ->
    system_log:warning("Ignoring msg due to undefined handler~nMessage: ~p", [Msg]),
    ignore;
handle_message(Msg, #s{handler  = Handler}) ->
    {res, Handler(Msg)}.

handle_info({'DOWN', Ref, _, _, Reason}, #s{nodes = Nodes,
                                            connection_monitors = Mons,
                                            remote_monitors = RMons,
                                            local_monitors = LMons} = State) ->
    case maps:find(Ref, Mons) of
        {ok, Node} ->
            system_log:warning("Node ~p disconnected: ~p", [Node, Reason]),

            lists:foreach(
                fun ({R, {Owner, Pid}}) ->
                    Owner ! {'DOWN', {interconnect_monitor, Pid, R}, process, Pid, noconnection}
                end, maps:to_list(mzb_bc:maps_get(Node, RMons, #{}))),

            {noreply, State#s{nodes = maps:remove(Node, Nodes),
                              connection_monitors = maps:remove(Ref, Mons),
                              remote_monitors = maps:put(Node, #{}, RMons)}};
        error ->
            case maps:find(Ref, LMons) of
                {ok, {Owner, Pid}} ->
                    send_to(node(Owner), {down, node(), Owner, Ref, Pid, Reason}, State),
                    {noreply, State#s{local_monitors = maps:remove(Ref, LMons)}};
                error ->
                    {noreply, State}
            end
    end;

handle_info(_Info, State) ->
    {noreply, State}.

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

