-module(mzb_api_bench).

-behaviour(gen_server).

%% API
-export([
    start_link/2,
    interrupt_bench/1,
    stop/1,
    get_status/1,
    start_deallocator/3,
    seconds/0,
    bench_data_link/2,
    bench_log_link/2
]).

-export([
    start_report_sender/1
]).

-ifdef(TEST).
-export([allocate_hosts/2]).
-endif.

-define(DEALLOCATOR_MAX_TIME, 3*60*1000*1000). % in us
-define(DEALLOCATE_RETRY_TIMEOUT, 10000). % in ms
-define(MAX_STOP_TIME, 3*60000). % in ms

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link(Id, Params) ->
    gen_server:start_link(?MODULE, [Id, Params], []).

interrupt_bench(Pid) ->
    gen_server:call(Pid, interrupt_bench).

stop(Pid) ->
    gen_server:call(Pid, stop, infinity).

get_status(Pid) ->
    gen_server:call(Pid, status).

init([Id, Params]) ->
    Now = os:timestamp(),
    StartTime = seconds(Now),
    % Acceptance tests assume the following purpose format: bench-<id>-<timestamp>
    % {{_, M, D}, {H, Mi, S}} = calendar:now_to_universal_time(now()),
    % Purpose = lists:flatten(io_lib:format("bench-~2.10.0B-~2.10.0B-~2.10.0B-~2.10.0B-~2.10.0B-~b", [M,D,H,Mi,S,Id])),
    Purpose = lists:flatten(io_lib:format("bench-~b-~b", [Id, StartTime])),
    BenchDataDir = init_data_dir(Id),
    RemoteBenchDir = filename:join(["/", "tmp", "mz", Purpose]),
    Includes = maps:get(includes, Params, []),
    Config = #{
        id => Id,
        remote_dir => RemoteBenchDir,
        local_dir => BenchDataDir,
        nodes_arg => maps:get(nodes, Params),
        script => generate_script_filename(maps:get(script, Params)),
        purpose => Purpose,
        node_git => application:get_env(mz_bench_api, mzbench_git, undefined),
        node_commit => maps:get(node_commit, Params),
        packages => maps:get(package, Params),
        emails => maps:get(email, Params),
        statsd_host => undefined,
        includes => [F || {F, _} <- Includes],
        env => generate_bench_env(Params),
        deallocate_after_bench => maps:get(deallocate_after_bench, Params),
        dont_provision_nodes => maps:get(dont_provision_nodes, Params),
        exclusive_node_usage => maps:get(exclusive_node_usage, Params),
        req_host => maps:get(req_host, Params),
        initial_user => maps:get(user, Params),
        director_host => undefined,
        worker_hosts => [],
        emulate_bench_crash => maps:get(emulate_bench_crash, Params)
    },
    Data = #{
        includes => Includes
    },

    InputBin = erlang:term_to_binary(Params),
    ok = file:write_file(local_path("params.bin", Config), InputBin),
    gen_server:cast(self(), start),
    erlang:process_flag(trap_exit, true),

    LogFile = filename:join(BenchDataDir, get_env(bench_log_file)),
    {ok, LogHandler} = file:open(LogFile, [write]),

    MetricsFile = filename:join(BenchDataDir, get_env(bench_metrics_file)),
    {ok, MetricsHandler} = file:open(MetricsFile, [write]),

    State = #{
        id => Id,
        start_time => StartTime,
        finish_time => undefined,
        status => init,
        config => Config,
        data => Data,
        spawned_ref => undefined,
        log_file => LogFile,
        log_file_handler => LogHandler,
        metrics_file => MetricsFile,
        metrics_file_handler => MetricsHandler,
        deallocator => undefined,
        metrics => #{}
    },
    info("Bench data dir: ~s", [BenchDataDir], State),
    {ok, State}.

handle_call(interrupt_bench, _From, #{id:= Id, status:= Status} = State) ->
    info("Interupting benchmark process, current status is ~p", [Status], State),
    State1 = State#{finish_time => seconds(), interrupted => true},
    mzb_api_server:bench_finished(Id, status(State1#{status => stopped})),
    {reply, ok, State1};

handle_call(stop, _From, #{status:= Status, spawned_ref:= Ref} = State) when Ref == undefined; Status == running ->
    info("Stopping benchmark process, current status is ~p", [Status], State),
    {stop, normal, ok, State};

handle_call(stop, _From, #{status:= Status} = State) ->
    info("Stop request for benchmark process, current status is ~p", [Status], State),
    timer:exit_after(?MAX_STOP_TIME, bench_stop_timed_out),
    {reply, ok, State#{stop_requested => true}};

handle_call(status, _From, State) ->
    {reply, status(State), State};

handle_call(_Request, _From, State) ->
    error("Unhandled call: ~p", [_Request], State),
    {noreply, State}.

handle_cast(start, #{start_time:= ST, config:= Config} = State) ->
    info("Starting benchmark at ~b ~p", [ST, Config], State),
    async(hosts_allocation, fun () ->
        allocate_hosts(Config, logger_fun(State))
    end, State);

handle_cast({_, finished, Ref, _}, #{spawned_ref:= Ref, stop_requested:= true} = State) ->
    info("Stopping benchmark process", [], State),
    {stop, normal, State#{spawned_ref:= undefined}};

handle_cast({_, finished, Ref, _}, #{spawned_ref:= Ref, interrupted:= true} = State) ->
    info("Benchmark process is waiting for stop", [], State),
    {noreply, State#{spawned_ref:= undefined}};

handle_cast({hosts_allocation, finished, Ref, {Hosts, UserName, Deallocator}}, #{spawned_ref:= Ref, config:= Config} = State) ->
    DirectorHost = hd(Hosts),
    WorkerHosts = tl(Hosts),
    info("Allocated director host: ~p,~nworker hosts: ~p", [DirectorHost, WorkerHosts], State),
    Deallocator2 =
        case maps:get(deallocate_after_bench, Config) of
            false -> fun () -> info("Skip deallocation", [], State) end;
            _ -> Deallocator
        end,
    Config2 = Config#{
                user_name => UserName,
                director_host => DirectorHost,
                worker_hosts => WorkerHosts},
    async(provisioning, fun () ->
            mzb_api_provision:provision_nodes(Config2, logger_fun(State))
        end, State#{config => Config2, deallocator => Deallocator2});

handle_cast({provisioning, finished, Ref, DirNode}, #{spawned_ref:= Ref, config:= Config} = State) ->
    async(uploading_script, fun () ->
        #{script:= Script, director_host:= DirectorHost, worker_hosts:= WorkerHosts, env:= Env} = Config,

        Environ = jiffy:encode({Env}),
        mzb_api_provision:ensure_file_content([DirectorHost|WorkerHosts], Environ, "environ.txt", Config, logger_fun(State)),
        case Script of
            #{name := _Name, body := Body, filename := FileName} ->
                mzb_api_provision:ensure_file_content([DirectorHost|WorkerHosts], Body, FileName, Config, logger_fun(State));
            _ -> ok
        end,
        ok
        end, State#{director_node => DirNode});

handle_cast({uploading_script, finished, Ref, ok}, #{spawned_ref:= Ref, config:= Config, data:= Data} = State) ->
    #{includes:= Includes} = Data,
    async(uploading_includes, fun () ->
            #{director_host:= DirectorHost, worker_hosts:= WorkerHosts} = Config,
            lists:foreach(
                fun ({Name, Content}) ->
                    mzb_api_provision:ensure_file_content([DirectorHost|WorkerHosts], Content, Name, Config, logger_fun(State))
                end, Includes),
            ok
        end, State);

handle_cast({uploading_includes, finished, Ref, ok}, #{spawned_ref:= Ref, config:= Config} = State) ->
    #{director_host:= DirectorHost, worker_hosts:= WorkerHosts} = Config,
    async(starting_collectors, fun() ->
        start_collectors(logs, [DirectorHost|WorkerHosts], get_env(bench_log_port), maps:get(log_file_handler, State)),
        start_collectors(metrics, [DirectorHost], get_env(bench_metrics_port), maps:get(metrics_file_handler, State)),
        ok
    end, State);

handle_cast({starting_collectors, finished, Ref, ok}, #{director_node:= DirNode, spawned_ref:= Ref, config:= Config} = State) ->
    #{user_name:= UserName, director_host:= DirectorHost, script:= Script} = Config,
    async(gather_metric_names, fun () ->
            [RemoteScript, RemoteEnv] = [remote_path(F, Config) || F <- [script_path(Script), "environ.txt"]],
            mzb_api_metrics:get_metrics(UserName, DirNode, DirectorHost, RemoteScript, RemoteEnv)
        end, State);

handle_cast({gather_metric_names, finished, Ref, MetricsMap}, #{director_node:= DirNode, spawned_ref:= Ref, config:= Config} = State) ->
    #{user_name:= UserName,
      director_host:= DirectorHost,
      script:= Script} = Config,
    async(running, fun () ->
            ScriptFilePath = script_path(Script),
            _ = mzb_api_provision:remote_cmd(
                UserName,
                [DirectorHost],
                "~/mz/mz_bench/bin/run.escript",
                [DirNode] ++
                [remote_path(F, Config) || F <- [ScriptFilePath, "report.txt", "environ.txt"]],
                logger_fun(State)),
            ok
        end, State#{metrics => MetricsMap});

handle_cast({running, finished, Ref, ok}, #{id:= Id, spawned_ref:= Ref, config:= Config} = State) ->
    case maps:find(emulate_bench_crash, Config) of
        {ok, true} -> erlang:error(
            {emulated_crash, nothing_to_see_here, please_move_along});
        _ -> ok
    end,
    State1 = State#{status => complete, finish_time => seconds(), spawned_ref => undefined},
    mzb_api_server:bench_finished(Id, status(State1)),
    {noreply, State1};

handle_cast({_, exception, Ref, _}, #{spawned_ref:= Ref, stop_requested:= true} = State) ->
    info("Stopping benchmark process", [], State),
    {stop, normal, State#{spawned_ref:= undefined}};

handle_cast({_, exception, Ref, _}, #{spawned_ref:= Ref, interrupted:= true} = State) ->
    info("Benchmark process is waiting for stop", [], State),
    {noreply, State#{spawned_ref:= undefined}};

handle_cast({Op, exception, Ref, {_, E, Stack}}, #{id:= Id, spawned_ref:= Ref} = State) ->
    error(format_error(Op, {E, Stack}), [], State),
    State1 = State#{status => failed, finish_time => seconds(), spawned_ref => undefined},
    mzb_api_server:bench_finished(Id, status(State1)),
    {noreply, State1};

handle_cast(_Msg, State) ->
    error("Unhandled cast: ~p", [_Msg], State),
    {noreply, State}.

handle_info({'EXIT', _, normal}, State) ->
    {noreply, State};

handle_info({'EXIT', P, Reason}, #{} = State) ->
    error("Benchmark received 'EXIT' from ~p with reason ~p, stopping", [P, Reason], State),
    {stop, Reason, State};

handle_info(_Info, State) ->
    error("Unhandled info: ~p", [_Info], State),
    {noreply, State}.

terminate(_Reason, #{id:= Id, config:= Config, deallocator:= Deallocator} = State) ->
    _ = start_report_sender_child(State),

    file:close(maps:get(log_file_handler, State)),
    file:close(maps:get(metrics_file_handler, State)),

    CleanAndDeallocate = 
        fun () ->
            catch mzb_api_provision:clean_nodes(Config, undefined),
            Deallocator == undefined orelse Deallocator()
        end,

    start_deallocator_child(Id, CleanAndDeallocate).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

start_deallocator_child(Id, DeallocatorFun) ->
    supervisor:start_child(deallocators_sup, [Id, DeallocatorFun, os:timestamp()]).

start_deallocator(Id, Deallocator, StartTime) ->
    Pid = erlang:spawn_link(fun () ->
        try
            lager:info("[ BENCH ] Started deallocator for benchmark #~b", [Id]),
            Deallocator(),
            lager:info("[ BENCH ] Deallocator for benchmark #~b has finished", [Id])
        catch
            C:E ->
                ST = erlang:get_stacktrace(),
                lager:error("[ BENCH ] Deallocation for benchmark #~b failed with reason ~p, retry later~nStacktrace: ~p", [Id, E, ST]),
                TimeSinceStart = timer:now_diff(os:timestamp(), StartTime),
                case TimeSinceStart > ?DEALLOCATOR_MAX_TIME of
                    true  ->
                        lager:error("[ BENCH ] Deallocator for benchmark #~b has finished due to time limit", [Id]);
                    false ->
                        timer:sleep(?DEALLOCATE_RETRY_TIMEOUT),
                        erlang:raise(C, E, ST)
                end
        end
    end),
    {ok, Pid}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_cloud_provider() ->
    {ok, {_Type, Name}} = application:get_env(mz_bench_api, cloud_plugin),
    Name.

add_env([], Env) -> Env;
add_env([H | T], Env) ->
    case application:get_env(H) of
        undefined -> add_env(T, Env);
        {ok, V} -> [{list_to_binary(atom_to_list(H)), list_to_binary(V)} | add_env(T, Env)]
    end.

generate_bench_env(Params) ->
    Env = maps:get(env, Params),
    Script = maps:get(script, Params),
    #{name := ScriptName} = Script,
    Env2 = [{<<"mzb_script_name">>, list_to_binary(ScriptName)} | Env],

    case proplists:get_value(<<"graphite">>, Env2) of
        undefined -> add_env([graphite, graphite_api_key, graphite_url], Env2);
        _H -> Env2
    end.

script_path(Script) ->
    case Script of
        #{filename := FileName} -> FileName;
        Pkg -> "~/mz/mz_bench_workers/" ++ Pkg ++ "/default.erl"
    end.

start_report_sender_child(State) ->
    supervisor:start_child(reports_sup, [State]).

start_report_sender(State) ->
    Pid = erlang:spawn_link(fun () -> send_email_report(State) end),
    {ok, Pid}.

send_email_report(#{id:= Id, status:= Status, config:= Config, start_time:= StartTime, metrics:= MetricsMap} = State) ->
    try
        #{emails:= Emails} = Config,
        BenchTime = seconds(os:timestamp()) - StartTime,
        Links =
            try
                mzb_api_metrics:get_graphite_image_links(MetricsMap, BenchTime)
            catch
                _:E ->
                    lager:error("Failed to get graphite image links: ~p", [E]),
                    []
            end,
        lager:info("Metrics links: ~p", [Links]),
        AttachFiles = download_images("graphite_", Links, State),
        {Subj, Body} = generate_mail_body(Id, Status, Links, Config),
        lager:info("EMail report: ~n~s~n~s~n", [Subj, Body]),
        Attachments = lists:map(
            fun (F) ->
                {ok, Bin} = file:read_file(local_path(F, Config)),
                {list_to_binary(F), <<"image/png">>, Bin}
            end, AttachFiles),
        lists:foreach(
            fun (Addr) ->
                lager:info("Sending bench results to ~s", [Addr]),
                BAddr = list_to_binary(Addr),
                mzb_api_mail:send(BAddr, Subj, Body, Attachments, application:get_env(mz_bench_api, mail, []))
            end, Emails),
        ok
    catch
        _:Error ->
            lager:error("Sending email report failed: ~p~n~p", [Error, erlang:get_stacktrace()]),
            {error, Error}
    end.

%% extract keys from state which will be persisted on disk
status(State) ->
    maps:with([status, start_time, finish_time, config, metrics, log_file, metrics_file], State).

async(Op, F, State) ->
    info("Stage '~s': started", [Op], State),
    Ref = erlang:make_ref(),
    Self = self(),
    _ = spawn_link(
        fun () ->
            try
                Res = F(),
                info("Stage '~s': finished", [Op], State),
                gen_server:cast(Self, {Op, finished, Ref, Res})
            catch
                C:E ->
                    error("Stage '~s': failed", [Op], State),
                    gen_server:cast(Self, {Op, exception, Ref, {C,E,erlang:get_stacktrace()}})
            end
        end),
    {noreply, State#{spawned_ref => Ref, status => Op}}.

allocate_hosts(#{nodes_arg:= N} = Config, _) when is_integer(N), N > 0 ->
    #{purpose:= Purpose,
      initial_user:= User,
      exclusive_node_usage:= Exclusive} = Config,
    Description = lists:flatten(io_lib:format("MZ-Bench cluster:~n~p", [Config])),
    CloudProvider = get_cloud_provider(),
    ClusterConfig = #{
        user => User, 
        description => Description, 
        exclusive_node_usage => Exclusive
    },
    % Allocate one supplementary node for the director
    {ok, ClusterId, UserName, Hosts} = CloudProvider:create_cluster(Purpose, N + 1, ClusterConfig),
    Deallocator =
        fun () ->
            CloudProvider:destroy_cluster(ClusterId)
        end,
    {Hosts, UserName, Deallocator};


allocate_hosts(#{nodes_arg:= [HostsStr]}, Logger) when is_list(HostsStr) ->
    URIs = string:tokens(HostsStr, ","),

    {Users, Hosts} = lists:foldr(fun (URI, {Users, Hosts}) ->
                                     case string:tokens(URI, "@") of
                                         [User, Host] ->
                                             {[User | Users], [Host | Hosts]};
                                         [Host] ->
                                             {Users, [Host | Hosts]};
                                         _ -> erlang:error({incorrect_hostname, URI})
                                     end
                                 end,
                                 {[], []},
                                 URIs),

    UserName = case lists:usort(Users) of
        [] -> "root";
        [User] -> User;
        Xs -> erlang:error({different_users_for_hosts, Xs})
    end,

    if erlang:length(Hosts) >= 2 ->
            Deallocator =
                fun () ->
                    mzb_api_provision:remote_cmd(UserName, Hosts, "~/mz/mz_bench/bin/mz_bench stop; true", [], Logger)
                end,
                {Hosts, UserName, Deallocator};
        true -> 
            erlang:error(not_enough_nodes)
    end.

seconds() -> seconds(os:timestamp()).

seconds({N1, N2, _N3}) ->
    N1*1000000 + N2.

remote_path(RelPath, #{remote_dir:= Root}) -> filename:join(Root, RelPath).
local_path(RelPath, #{local_dir:= Root}) -> filename:join(Root, RelPath).

init_data_dir(Id) ->
    BenchDataDir = filename:join(mzb_api_server:server_data_dir(), integer_to_list(Id)),
    case filelib:is_file(BenchDataDir) of
        false -> ok;
        true  -> erlang:error({data_dir_already_exists, BenchDataDir})
    end,
    case filelib:ensure_dir(filename:join(BenchDataDir, ".")) of
        ok -> BenchDataDir;
        {error, Reason} -> erlang:error({ensure_dir_error, BenchDataDir, Reason})
    end.

start_collectors(Purpose, Hosts, Port, FileHandler) ->
    Self = self(),
    Pids = lists:map(fun (Host) ->
        Pid = spawn_link(fun () ->
            try gen_tcp:connect(Host, Port, [{active, false}, {packet, 4}]) of
                {ok, Socket} ->
                    lager:info("Collector is started for ~p on ~s", [Purpose, Host]),
                    Self ! {self(), connected},
                    process_data(Purpose, Host, Socket, FileHandler);
                {error, Reason} ->
                    Self ! {self(), failed, Reason}
            catch
                C:E ->
                    ST = erlang:get_stacktrace(),
                    Self ! {self(), failed, {C, E, ST}}
            end
        end),
        {Pid, Purpose, Host}
    end, Hosts),
    wait_collectors(Pids),
    Pids.

wait_collectors([]) -> ok;
wait_collectors([{Pid, Purpose, Host} | Tail]) ->
    receive
        {Pid, connected} -> wait_collectors(Tail);
        {Pid, failed, Reason} ->
            lager:error("Collector '~p' is failed to start on host ~s with reason ~p", [Purpose, Host, Reason]),
            erlang:error({catch_collector_connect_failed, Host, Reason})
    after 30000 ->
        lager:error("Collector '~p' is timed-out to start on host ~s", [Purpose, Host]),
        erlang:error({catch_collector_connect_timedout, Host})
    end.

process_data(Purpose, Host, Socket, FileHandler) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            file:write(FileHandler, Data),
            process_data(Purpose, Host, Socket, FileHandler);
        {error, closed} ->
            lager:info("Collector '~p' is closed on host ~s", [Purpose, Host]);
        {error, Reason} ->
            lager:error("Collector '~p' is failed on host ~s with reason ~p", [Purpose, Host, Reason])
    end.

generate_mail_body(Id, Status, Links, Config) ->
    #{packages:= Packages,
      env:= Env,
      script:= Script
      } = Config,
    {ScriptName, ScriptBody} = case Script of
       #{name := Name, body := Body} -> {Name, Body};
       Package ->
            {Package ++ " / default scenario", "Default scenario for " ++ Package}
    end,
    Subject = io_lib:format("Bench report for ~s (~s)", [ScriptName, Status]),
    Chars = io_lib:format(
        "Status: ~s~n~n"
        "Packages:~n~s~n~n"
        "Environment:~n~s~n~n"
        "Script body:~n~s~n~n"
        "Benchmark logs:~n  ~s~n~n"
        "Metrics data:~n  ~s~n~n"
        "Graphite links for reference:~n~s~n",
        [Status,
         indent(string:join(Packages, "\n"), 2, "(no packages)"),
         indent(string:join([io_lib:format("~s = ~s", [K,V]) || {K,V} <- Env], "\n"), 2, "(no env variables)"),
         indent(ScriptBody, 2),
         bench_log_link(Id, Config),
         bench_data_link(Id, Config),
         indent(string:join(Links, "\n"), 2, "(no links available)")
         ]),
    {list_to_binary(Subject), list_to_binary(Chars)}.

bench_data_link(Id, #{req_host:= ServerAddr}) ->
    io_lib:format("http://~s/data?id=~b", [ServerAddr, Id]).

bench_log_link(Id, #{req_host:= ServerAddr}) ->
    io_lib:format("http://~s/logs?id=~b", [ServerAddr, Id]).

indent("", N, Default) -> indent(Default, N);
indent(Str, N, _) -> indent(Str, N).

indent(Binary, N) when is_binary(Binary) ->
    erlang:list_to_binary(indent(erlang:binary_to_list(Binary), N));
indent(Str, N) ->
    Spaces = [$\s || _ <- lists:seq(1, N)],
    string:join([Spaces ++ Line || Line <- string:tokens(Str, "\n")], "\n").

download_images(Prefix, URLs, #{config:= Config} = State) ->
    {Files, _} = lists:foldl(fun (URL, {Acc, N}) ->
        case httpc:request(get, {URL, []}, [{timeout, 5000}], []) of
            {ok, {_, _, Data}} ->
                FileName = Prefix ++ integer_to_list(N) ++ ".png",
                FullPath = local_path(FileName, Config),
                ok = file:write_file(FullPath, Data),
                info("Downloaded: ~s -> ~s", [URL, FileName], State),
                {[FileName|Acc], N + 1};
            {error, Reason} ->
                error("Download failed: ~s with reason: ~p", [URL, Reason], State),
                {Acc, N}
        end
    end, {[], 1}, URLs),
    Files.

logger_fun(State) -> fun (S, F, A) -> log(S, F, A, State) end.

info(Format, Args, State) ->
    log(info, Format, Args, State).

error(Format, Args, State) ->
    log(error, Format, Args, State).

log(Severity, Format, Args, #{log_file_handler:= H, id:= Id}) ->
    Format2 = "[ BENCH #~b ] " ++ Format,
    Args2 = [Id|Args],
    % We can't call lager:Severity(...) because lager uses parse_transform
    case Severity of
        debug -> lager:debug(Format2, Args2);
        info  -> lager:info(Format2, Args2);
        error -> lager:error(Format2, Args2)
    end,
    format_log(H, Severity, Format, Args).

format_log(_Handler, debug, _Format, _Args) -> ok;
format_log(Handler, Severity, Format, Args) ->
    Now = {_, _, Ms} = os:timestamp(),
    {_, {H,M,S}} = calendar:now_to_universal_time(Now),
    file:write(Handler, io_lib:format("~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0B [~s] [ API ] " ++ Format ++ "~n", [H, M, S, Ms div 1000, Severity|Args])).

get_env(K) -> application:get_env(mz_bench_api, K, undefined).

format_error(_, {{cmd_failed, Cmd, Code, Output}, _}) ->
    io_lib:format("Command returned ~b:~n ~s~nCommand output: ~s", [Code, Cmd, Output]);
format_error(Op, {E, Stack}) ->
    io_lib:format("Benchmark has failed on ~p with reason:~n~p~n~nStacktrace: ~p", [Op, E, Stack]).

generate_script_filename(#{name := _Name, body := Body} = Script) ->
    Script#{filename => io_lib:format("~s.erl", [lists:flatten(lists:map(
        fun(Num) -> erlang:integer_to_list(Num, 16) end,
        erlang:binary_to_list(crypto:hash(sha, Body))
    ))])}.
