-module(mzb_dummycloud_plugin).

-behaviour(gen_server).

-export([
    start/2,
    create_cluster/3,
    destroy_cluster/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

start(Name, Opts) ->
    Spec = {Name,
            _MFA = {gen_server, start_link, [?MODULE, [Opts], []]},
            permanent, 5000, worker, [?MODULE]},
    {ok, Child} = supervisor:start_child(mzb_api_sup, Spec),
    Child.

create_cluster(Pid, N, _Config) ->
    case lock_hosts(Pid, N) of
        {ok, User, L} -> {ok, {Pid, L}, User, L};
        {error, locked} -> erlang:error(hosts_are_busy)
    end.

destroy_cluster({Pid, Hosts}) ->
    gen_server:call(Pid, {unlock_hosts, Hosts}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Opts]) ->
    Hosts = mzb_bc:maps_get(hosts, Opts, ["localhost"]),
    {Hostnames, Username} = parse_hosts(Hosts),
    {ok, #{hosts => Hostnames, user_name => Username, hosts_locked => []}}.

handle_call({lock_hosts, _}, _, #{hosts:= [Host], hosts_locked:= [], user_name:= User} = State) ->
    {reply, {ok, User, [Host]}, State#{hosts_locked => [Host]}};

handle_call({lock_hosts, N}, _, #{hosts:= Hosts, hosts_locked:= Locked, user_name:= User} = State) ->
    Available = lists:subtract(Hosts, Locked),
    case erlang:length(Available) of
        A when A < N -> {reply, {error, locked}, State};
        _ -> {Result, _} = lists:split(N, Available),
            {reply, {ok, User, Result}, State#{hosts_locked => Locked ++ Result}}
    end;

handle_call({unlock_hosts, Hosts}, _, #{hosts_locked := Locked} = State) ->
    {reply, ok, State#{hosts_locked => lists:subtract(Locked, Hosts)}};

handle_call(Request, _From, State) ->
    lager:error("Unhandled call: ~p ~p", [Request, State]),
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

parse_hosts(Hosts) ->
    lists:mapfoldl(
        fun (Host, Acc) ->
            case string:tokens(Host, "@") of
                [U, H] -> {H, U};
                [H] -> {H, Acc}
            end
        end, undefined, lists:reverse(Hosts)).

lock_hosts(Pid, N) -> lock_hosts(Pid, N, 60).

lock_hosts(_Pid, _, Attempts) when Attempts =< 0 -> {error, locked};
lock_hosts(Pid, N, Attempts) ->
    case gen_server:call(Pid, {lock_hosts, N}) of
        {ok, U, L} when is_list(L) -> {ok, U, L};
        {error, locked} ->
            timer:sleep(1000),
            lock_hosts(Pid, N, Attempts - 1)
    end.

