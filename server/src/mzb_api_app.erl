-module(mzb_api_app).
-behaviour(application).

%% API.
-export([start/2, prep_stop/1, stop/1]).

%% API.

start(_Type, _Args) ->
    ok = load_config(mz_bench_api),

    ok = load_cloud_plugin(),

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
    %% deactivate stops all benchmarks. we are waiting 120 secs 
    %% to be sure that benchmark's finalize are finished
    mzb_api_server:deactivate(),
    wait_benchmarks_finish(_AttemptNum = 120),
    State.

stop(_State) ->
    lager:warning("Server is stopping..."),
    ok = cowboy:stop_listener(http),
    ok.

wait_benchmarks_finish(Attempts) when Attempts =< 0 -> ok;
wait_benchmarks_finish(Attempts) ->
    Benchmarks = supervisor:which_children(benchmarks_sup),
    BenchmarksNum = length(Benchmarks),
    case BenchmarksNum > 0 of
        true  ->
            lager:info("Waiting for: ~p", [Benchmarks]),
            timer:sleep(1000),
            wait_benchmarks_finish(Attempts - 1);
        false ->
            lager:info("All benchmarks finished"),
            ok
    end.

load_config(AppName) ->
    case init:get_argument(config_file) of
        {ok, [[Config]]} ->
            load_config(Config, AppName);
        _ ->
            {ok, Configs} = application:get_env(mz_bench_api, server_configs),
            _ = lists:dropwhile(
                fun (Cfg) ->
                    try
                        load_config(Cfg, AppName),
                        false
                    catch
                        _:{config_read_error, _, enoent} -> true
                    end
                end, Configs),
            ok
    end.

load_config(File, AppName) ->
    case file:consult(mzb_file:expand_filename(File)) of
        {ok, [Config]} ->
            lager:info("Reading configuration from ~s~n~p", [File, Config]),
            lists:foreach(fun ({App, Env}) when App == AppName ->
                                [ application:set_env(App, Key, Val) || {Key, Val} <- Env];
                              (_) -> ok
                          end, Config),
            ok;
        {error, Reason} ->
            erlang:error({config_read_error, File, Reason})
    end.

load_cloud_plugin() ->
    {ok, Dir} = application:get_env(mz_bench_api, plugins_dir),
    ok = filelib:ensure_dir(filename:join(Dir, ".")),
    PluginPaths = mzb_file:wildcard(filename:join([Dir, "*", "ebin"])),
    ok = code:add_pathsa(PluginPaths),
    lager:info("PATHS: ~p", [code:get_path()]),
    case application:get_env(cloud_plugin) of
        {ok, {application, Name}} ->
            lager:info("Loading cloud plugin: ~p...", [Name]),
            ok = application:load(Name),
            ok = load_config(Name),
            {ok, _} = application:ensure_all_started(Name),
            ok;
        {ok, {module, _}} -> ok;
        undefined ->
            lager:error("A cloud plugin must be specified in the \"cloud_plugin\" environment variable!"),
            erlang:error(no_cloud_plugin)
    end.

