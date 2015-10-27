-module(mzb_api_cloud).

-behaviour(gen_server).

%% API
-export([start_link/0,
         create_cluster/3,
         destroy_cluster/1,
         list_clouds/0,
         stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

create_cluster(Cloud, N, Config) ->
    case gen_server:call(?MODULE, {get_cloud, Cloud}, infinity) of
        {ok, {Provider, Instance}} ->
            {ok, ClusterId, User, Hosts} = Provider:create_cluster(Instance, N, Config),
            {ok, {Provider, ClusterId}, User, Hosts};
        error ->
            erlang:error({unknown_cloud_name, Cloud})
    end.

destroy_cluster({Provider, ClusterId}) ->
    Provider:destroy_cluster(ClusterId).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

list_clouds() ->
    gen_server:call(?MODULE, {get_cloud_list}).

% just for test purpose
stop() ->
    gen_server:call(?MODULE, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Plugins = application:get_env(mzbench_api, cloud_plugins, []),
    gen_server:cast(self(), {init_plugins, Plugins}),
    {ok, #{}}.

handle_call({get_cloud, undefined}, _From, State = #{clouds:= Clouds, default:= Default}) ->
    {reply, maps:find(Default, Clouds), State};

handle_call({get_cloud, Name}, _From, State = #{clouds:= Clouds}) ->
    {reply, maps:find(Name, Clouds), State};

handle_call({get_cloud_list}, _From, State = #{clouds:= Clouds}) ->
    {reply, maps:keys(Clouds), State};

handle_call(stop, _from, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
   {noreply, State}.

handle_cast({init_plugins, Plugins}, State) ->
    [{Default, _}|_] = Clouds = [init_plugin(P) || P <- Plugins],
    {noreply, State#{default => Default, clouds => maps:from_list(Clouds)}};

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

