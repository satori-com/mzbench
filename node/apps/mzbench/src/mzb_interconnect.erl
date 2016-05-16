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
    multi_call/2,
    multi_call/3,
    abcast/2,
    handle/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(s, {
    role = worker,
    nodes = #{},
    monitors = #{},
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

handle(Msg) ->
    system_log:info("Message at ~p: ~p", [node(), Msg]),
    gen_server:cast(?MODULE, {from_remote, Msg}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Handler]) ->
    {ok, #s{nodes = #{}, monitors = #{}, handler = Handler}}.

handle_call({accept, Node, Role, Owner, Sender}, _From, #s{nodes = Nodes, monitors = Mons, role = MyRole} = State) ->
    system_log:info("Connection to ~p established", [Node]),
    Ref = erlang:monitor(process, Owner),
    Director = case Role of
            director -> Node;
            worker -> State#s.director
        end,
    {reply, {ok, MyRole}, State#s{nodes = maps:put(Node, Sender, Nodes),
                          monitors = maps:put(Ref,  Node,   Mons),
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

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({cast, Node, Msg}, State) when Node == node() ->
    _ = (catch handle_message(Msg, State)),
    {noreply, State};

handle_cast({cast, Node, Msg}, State) ->
    send_to(Node, {cast, Msg}, State),
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

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_message(Msg, #s{handler = undefined}) ->
    system_log:warning("Ignoring msg due to undefined handler~nMessage: ~p", [Msg]),
    ignore;
handle_message(Msg, #s{handler  = Handler}) ->
    {res, Handler(Msg)}.

handle_info({'DOWN', Ref, _, _, Reason}, #s{nodes = Nodes, monitors = Mons} = State) ->
    case maps:find(Ref, Mons) of
        {ok, Node} ->
            system_log:warning("Node ~p disconnected: ~p", [Node, Reason]),
            {noreply, State#s{nodes = maps:remove(Node, Nodes), monitors = maps:remove(Ref, Mons)}};
        error -> {noreply, State}
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

