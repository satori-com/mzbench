-module(mzb_interconnect).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    accept_connection/3,
    nodes/0,
    connect/2,
    call/2,
    call/3,
    cast/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(s, {
    nodes = #{},
    monitors = #{}
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

accept_connection(Host, Owner, Sender) ->
    gen_server:call(?MODULE, {accept, Host, Owner, Sender}).

nodes() ->
    gen_server:call(?MODULE, nodes, infinity).

connect(Host, Port) ->
    mzb_interconnect_sup:start_client(Host, Port).

call(Node, Req) -> call(Node, Req, infinity).

call(Node, Req, Timeout) ->
    gen_server:call(?MODULE, {call, Node, Req}, Timeout).

cast(Node, Msg) ->
    gen_server:cast(?MODULE, {cast, Node, Msg}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #s{nodes = #{}, monitors = #{}}}.

handle_call({accept, Node, Owner, Sender}, _From, #s{nodes = Nodes, monitors = Mons} = State) ->
    system_log:info("Connection to ~p established", [Node]),
    Ref = erlang:monitor(process, Owner),
    {reply, true, State#s{nodes    = maps:put(Node, Sender, Nodes),
                          monitors = maps:put(Ref,  Node,   Mons)}};

handle_call(nodes, _From, #s{nodes = Nodes} = State) ->
    {reply, maps:keys(Nodes), State};

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

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



