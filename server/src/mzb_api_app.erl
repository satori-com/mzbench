-module(mzb_api_app).
-behaviour(application).

%% API.
-export([start/2, prep_stop/1, stop/1]).

%% API.

start(_Type, _Args) ->
    load_config(application:get_env(mz_bench_api, server_config, undefined)),
    load_cloud_plugin(),

    Static = fun(Filetype) ->
                 {lists:append(["/", Filetype, "/[...]"]), cowboy_static,
                     {priv_dir, mz_bench_api, [Filetype], [{mimetypes, cow_mimetypes, web}]}}
             end,

    Dispatch = cowboy_router:compile([
        {'_', [
            Static("fonts"),
            Static("css"),
            Static("img"),
            Static("js"),
            {'_', mzb_api_endpoints, []}
        ]}
    ]),
    {ok, CowboyInterfaceStr} = application:get_env(mz_bench_api, network_interface),
    {ok, CowboyInterface} = inet_parse:address(CowboyInterfaceStr),
    {ok, CowboyPort} = application:get_env(mz_bench_api, listen_port),
    lager:info("Starting cowboy listener on ~p:~p", [CowboyInterface, CowboyPort]),
    {ok, _} = cowboy:start_http(http, 100,
        [{port, CowboyPort}, {ip, CowboyInterface}],
        [{env, [{dispatch, Dispatch}]}]),
    {ok, Sup} = mzb_api_sup:start_link(),
    {ok, Sup, #{}}.

prep_stop(State) ->
    lager:warning("Server is going to shutdown!"),
    mzb_api_server:deactivate(),
    wait_deallocators_finish(_AttemptNum = 120),
    State.

stop(_State) ->
    lager:warning("Server is stopping..."),
    cowboy:stop_listener(http),
    ok.

wait_deallocators_finish(Attempts) when Attempts =< 0 -> ok;
wait_deallocators_finish(Attempts) ->
    Deallocators = supervisor:which_children(deallocators_sup),
    DeallocatorsNum = length(Deallocators),
    case DeallocatorsNum > 0 of
        true  ->
            lager:info("Waiting for: ~p", [Deallocators]),
            timer:sleep(1000),
            wait_deallocators_finish(Attempts - 1);
        false ->
            lager:info("All deallocators finished"),
            ok
    end.

load_config(File) ->
    case file:consult(File) of
        {ok, [Config]} ->
            lager:info("Reading configuration from ~s", [File]),
            lists:foreach(fun ({App, Env}) ->
                [ application:set_env(App, Key, Val) || {Key, Val} <- Env]
            end, Config);
        {error, enoent} ->
            ok
    end.

load_cloud_plugin() ->
    case application:get_env(cloud_plugin) of
        {ok, {application, Name}} -> lager:info("Loading cloud plugin: ~p...", [Name]),
                                     {ok, _} = application:ensure_all_started(Name);
        {ok, {module, _}} -> ok;
        undefined ->
            lager:error("A cloud plugin must be specified in the \"cloud_plugin\" environment variable!"),
            erlang:error(no_cloud_plugin)
    end.
