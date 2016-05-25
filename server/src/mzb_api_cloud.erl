-module(mzb_api_cloud).

-behaviour(gen_server).

%% API
-export([start_link/0,
         create_cluster/4,
         destroy_cluster/1,
         list_clouds/0,
         clusters_info/0,
         remove_cluster_info/1,
         stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

create_cluster(BenchId, Cloud, N, Config) ->
    case gen_server:call(?MODULE, {get_allocator, BenchId, Cloud, N, Config}, infinity) of
        %% We don't want to block gen_server for a long time (allocation
        %% might take a while) so we allocate nodes outside the gen_server
        {ok, Allocator} -> Allocator();
        {error, not_found} ->
            erlang:error({unknown_cloud_name, Cloud})
    end.

destroy_cluster(Id) ->
    %% might take a long time, don't do it inside gen_server
    case get_cluster(Id) of
        {ok, {Provider, Cluster}} ->
            try
                ok = Provider:destroy_cluster(Cluster),
                gen_server:call(?MODULE, {deallocated, Id}, infinity),
                ok
            catch
                C:E ->
                    gen_server:call(?MODULE, {deallocation_failed, Id, E}, infinity),
                    erlang:raise(C, E, erlang:get_stacktrace())
            end;
        {error, Error} ->
            erlang:error(Error)
    end.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

list_clouds() ->
    gen_server:call(?MODULE, {get_cloud_list}).

clusters_info() ->
    dets:foldr(fun ({Id, State, Props}, Acc) -> [[{id, Id},{state, State}|Props]|Acc] end, [], ?MODULE).

remove_cluster_info(Id) ->
    gen_server:call(?MODULE, {remove_cluster_info, Id}, infinity).

% just for test purpose
stop() ->
    gen_server:call(?MODULE, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Plugins = application:get_env(mzbench_api, cloud_plugins, []),
    DetsFile = filename:join(mzb_api_server:server_data_dir(), ".allocated_clusters.dets"),
    ok = filelib:ensure_dir(DetsFile),
    {ok, _} = dets:open_file(?MODULE, [{file, DetsFile}, {type, set}]),
    gen_server:cast(self(), {init_plugins, Plugins}),
    {ok, #{cluster_id => max_id() + 1}}.

max_id() ->
    dets:foldl(fun ({Id, _, _}, Max) -> max(Id, Max) end, -1, ?MODULE).

handle_call({get_allocator, BenchId, Cloud, N, Config}, _From, State = #{cluster_id:= Id}) ->
    case get_cloud(Cloud, State) of
        {ok, {Provider, Instance}} ->
            Self = self(),
            register_cluster(Id, BenchId, Provider, N),
            F =
                fun () ->
                    try
                        {ok, Cluster, User, Hosts} = Provider:create_cluster(Instance, N, maps:put(bench_id, BenchId, Config)),
                        gen_server:call(Self, {allocated, Id, Cluster, User, Hosts}, infinity),
                        {ok, Id, User, Hosts}
                    catch
                        C:E ->
                            gen_server:call(Self, {allocation_failed, Id, E}, infinity),
                            erlang:raise(C, E, erlang:get_stacktrace())
                    end
                end,
            {reply, {ok, F}, State#{cluster_id:= Id + 1}};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({allocated, Id, Cluster, User, Hosts}, _From, State) ->
    set_cluster_allocated(Id, Cluster, User, Hosts),
    {reply, ok, State};

handle_call({allocation_failed, Id, Error}, _From, State) ->
    set_cluster_allocate_failed(Id, Error),
    {reply, ok, State};

handle_call({deallocated, Id}, _From, State) ->
    unregister_cluster(Id),
    {reply, ok, State};

handle_call({deallocation_failed, Id, Error}, _From, State) ->
    set_cluster_deallocate_failed(Id, Error),
    {reply, ok, State};

handle_call({get_cloud_list}, _From, State = #{clouds:= Clouds, default := Default}) ->
    {reply, [Default | maps:keys(maps:remove(Default, Clouds))], State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({remove_cluster_info, Id}, _From, State) ->
    catch unregister_cluster(Id),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    lager:error("Unhandled call: ~p", [_Request]),
    {noreply, State}.

handle_cast({init_plugins, Plugins}, State) ->
    [{Default, _}|_] = Clouds = [init_plugin(P) || P <- Plugins],
    {noreply, State#{default => Default, clouds => maps:from_list(Clouds)}};

handle_cast(_Msg, State) ->
    lager:error("Unhandled cast: ~p", [_Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    lager:error("Unhandled info: ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_cloud(undefined, #{clouds:= Clouds, default:= Default}) ->
    maps:find(Default, Clouds);
get_cloud(Name, #{clouds:= Clouds}) ->
    maps:find(Name, Clouds).


init_plugin({Name, Opts}) ->
    Provider =
        case Opts of
            #{application:= App} ->
                lager:info("Loading cloud plugin: ~p...", [App]),
                case application:load(App) of
                    ok -> ok;
                    {error, {already_loaded, _}} -> ok;
                    {error, Reason} ->
                        lager:error("Failed to load ~p cloud plugin with reason: ~p", [Name, Reason]),
                        erlang:error({load_failed, Name, Reason})
                end,
                ok = mzb_api_app:load_config(App),
                {ok, _} = application:ensure_all_started(App),
                App;
            #{module:= M} -> M;
            #{} ->
                lager:error("Either module or application param must be specified for ~p cloud plugin", [Name]),
                erlang:error({bad_cloud_plugin, Name})
        end,
    Instance = Provider:start(Name, Opts),
    {Name, {Provider, Instance}}.

register_cluster(Id, BenchId, Provider, N) ->
    update_cluster_state(Id, registered, [{bench_id, BenchId}, {provider, Provider}, {n, N}]).

set_cluster_allocated(Id, Cluster, User, Hosts) ->
    update_cluster_state(Id, allocated, [{cluster, Cluster}, {user, User}, {hosts, Hosts}]).

set_cluster_allocate_failed(Id, Error) ->
    update_cluster_state(Id, allocation_failed, [{reason, Error}]).

set_cluster_deallocate_failed(Id, Error) ->
    update_cluster_state(Id, deallocation_failed, [{reason, Error}]).

unregister_cluster(Id) ->
    dets:delete(?MODULE, Id).

update_cluster_state(Id, State, Params) ->
    OldParams =
        case dets:lookup(?MODULE, Id) of
            [{_, _, P}] -> P;
            [] -> []
        end,

    NewParams =
        lists:foldl(fun ({K, V}, Acc) ->
            lists:keystore(K, 1, Acc, {K, V})
        end, OldParams, [{timestamp, timestamp()}|Params]),

    ok = dets:insert(?MODULE, {Id, State, NewParams}),
    ok = dets:sync(?MODULE).

get_cluster(Id) ->
    case dets:lookup(?MODULE, Id) of
        [{_, _, Props}] ->
            case proplists:get_value(cluster, Props, undefined) of
                undefined ->
                    {error, no_cluster};
                Cluster ->
                    Provider = proplists:get_value(provider, Props, undefined),
                    {ok, {Provider, Cluster}}
            end;
        [] -> {error, not_found}
    end.

timestamp() ->
    {Mega, Secs, _} = os:timestamp(),
    Mega * 1000000 + Secs.
