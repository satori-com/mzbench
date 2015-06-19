-module(mzb_api_server).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    deactivate/0,
    start_bench/1,
    restart_bench/1,
    stop_bench/1,
    get_info/0,
    bench_finished/2,
    status/1,
    server_data_dir/0,
    ensure_started/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

deactivate() ->
    gen_server:call(?MODULE, deactivate, infinity).

ensure_started() ->
    case gen_server:call(?MODULE, is_ready) of
        true -> ok;
        false -> erlang:error(server_not_active)
    end.

start_bench(Params) ->
    case gen_server:call(?MODULE, {start_bench, Params}, infinity) of
        {ok, Resp} -> Resp;
        {error, {exception, {C,E,ST}}} -> erlang:raise(C,E,ST);
        {error, Reason} -> erlang:error(Reason)
    end.

restart_bench(Id) ->
    case gen_server:call(?MODULE, {restart_bench, Id}, infinity) of
        {ok, Resp} -> Resp;
        {error, {exception, {C,E,ST}}} -> erlang:raise(C,E,ST);
        {error, not_found} ->
            erlang:error({not_found, io_lib:format("Benchmark ~p is not found", [Id])});
        {error, Reason} -> erlang:error(Reason)
    end.

stop_bench(Id) ->
    case gen_server:call(?MODULE, {stop_bench, Id}, infinity) of
        ok -> ok;
        {error, not_found} ->
            erlang:error({not_found, io_lib:format("Benchmark ~p is not found", [Id])})
    end.

status(Id) ->
    case ets:lookup(benchmarks, Id) of
        [{_, B}] when is_pid(B) ->
            mzb_api_bench:get_status(B);
        [{_, Status}] ->
            Status;
        [] ->
            erlang:error({not_found, io_lib:format("Benchmark ~p is not found", [Id])})
    end.

bench_finished(Id, Status) ->
    gen_server:cast(?MODULE, {bench_finished, Id, Status}).

get_info() ->
    ets:foldl(
        fun ({Id, Pid}, Acc) when is_pid(Pid) ->
                [{Id, mzb_api_bench:get_status(Pid)}|Acc];
            ({Id, Status}, Acc) ->
                [{Id, Status}|Acc]
        end, [], benchmarks).

init([]) ->
    _ = ets:new(benchmarks, [named_table, set, protected]),
    ServerDir = server_data_dir(),
    ok = filelib:ensure_dir(filename:join(ServerDir, ".")),
    MaxId = import_data(ServerDir),
    User = sys_username(),
    lager:info("Server username: ~p", [User]),
    {ok, #{next_id => MaxId + 1,
           monitors => #{},
           status => active,
           data_dir => ServerDir,
           user => User,
           localhost_allocated => false}}.

server_data_dir() ->
    {ok, [HomeDir|_]} = init:get_argument(home),
    DataDir = application:get_env(mz_bench_api, bench_data_dir, undefined),
    filename:absname(filename:join([HomeDir, "mz", DataDir, "data"])).

handle_call({start_bench, Params}, _From, #{status:= active} = State) ->
    case start_bench_child(Params, State) of
        {ok, Id, NewState} ->
            {reply, {ok, #{id => Id, status => <<"pending">>}}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({start_bench, Params}, _From, #{status:= inactive} = State) ->
    lager:info("[ SERVER ] Start of bench failed because server is inactive ~p", [Params]),
    {reply, {error, server_inactive}, State};

handle_call({restart_bench, RestartId}, _From, #{status:= active, data_dir:= DataDir} = State) ->
    lager:info("[ SERVER ] Restarting bench #~b", [RestartId]),
    RestartIdStr = erlang:integer_to_list(RestartId),
    ParamsFile = filename:join([DataDir, RestartIdStr, "params.bin"]),
    case file:read_file(ParamsFile) of
        {ok, Binary} ->
            Params = erlang:binary_to_term(Binary),
            case start_bench_child(Params, State) of
                {ok, Id, NewState} ->
                    {reply, {ok, #{id => Id, status => <<"pending">>}}, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        {error, enoent} ->
            {reply, {error, not_found}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({restart_bench, RestartId}, _From, #{status:= inactive} = State) ->
    lager:info("[ SERVER ] Restart of bench failed because server is inactive #~p", [RestartId]),
    {reply, {error, server_inactive}, State};

handle_call(deactivate, From, #{} = State) ->
    Unfinished = ets:foldl(
        fun ({_, Pid}, Acc) when is_pid(Pid) -> [Pid | Acc];
            (_, Acc) -> Acc
        end, [], benchmarks),
    lager:info("[ SERVER ] Stopping all benchmarks due to server stop: ~p", [Unfinished]),
    [ok = mzb_api_bench:interrupt_bench(P) || P <- Unfinished],
    erlang:spawn_link(fun () ->
        Callback = fun () -> gen_server:reply(From, ok) end,
        wait_processes(Unfinished, Callback)
    end),
    {noreply, State#{status:= inactive}};

handle_call({stop_bench, Id}, _, #{} = State) ->
    lager:info("[ SERVER ] Stop bench #~b request received", [Id]),
    case ets:lookup(benchmarks, Id) of
        [{_, BenchPid}] when is_pid(BenchPid) ->
            ok = mzb_api_bench:interrupt_bench(BenchPid),
            {reply, ok, State};
        [{_, _}] ->
            {reply, ok, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call(lock_localhost, _, #{localhost_allocated:= true} = State) ->
    {reply, {error, locked}, State};

handle_call(lock_localhost, _, #{localhost_allocated:= false} = State) ->
    {reply, ok, State#{localhost_allocated => true}};

handle_call(unlock_localhost, _, State) ->
    {reply, ok, State#{localhost_allocated => false}};

handle_call(is_ready, _, #{status:= active} = State) ->
    {reply, true, State};

handle_call(is_ready, _, #{status:= inactive} = State) ->
    {reply, false, State};

handle_call(_Request, _From, State) ->
    lager:error("Unhandled call: ~p", [_Request]),
    {noreply, State}.

handle_cast({bench_finished, Id, Status}, State) ->
    lager:info("[ SERVER ] Bench #~b finished with status ~p", [Id, maps:get(status, Status)]),
    [{_, Pid}] = ets:lookup(benchmarks, Id),
    save_results(Id, Status, State),
    ok = mzb_api_bench:stop(Pid),
    {noreply, State};

handle_cast(_Msg, State) ->
    lager:error("Unhandled cast: ~p", [_Msg]),
    {noreply, State}.

handle_info({'DOWN', Ref, process, Pid, normal}, #{monitors:= Mons} = State) ->
    case maps:find(Ref, Mons) of
        {ok, _} ->
            {noreply, State#{monitors => maps:remove(Ref, Mons)}};
        error ->
            lager:error("Received DOWN from unknown process ~p", [Pid]),
            {noreply, State}
    end;

handle_info({'DOWN', Ref, process, Pid, Reason}, #{monitors:= Mons} = State) ->
    case maps:find(Ref, Mons) of
        {ok, Id} ->
            lager:error("Benchmark process #~b ~p has crashed with reason: ~p", [Id, Pid, Reason]),
            Status = #{status => failed, reason => {crashed, Reason}, config => undefined},
            save_results(Id, Status, State),
            {noreply, State#{monitors => maps:remove(Ref, Mons)}};
        error ->
            lager:error("Received DOWN from unknown process ~p", [Pid]),
            {noreply, State}
    end;

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

start_bench_child(Params, #{next_id:= Id, monitors:= Mons, user:= User} = State) ->
    lager:info("[ SERVER ] Start bench #~b", [Id]),
    case supervisor:start_child(benchmarks_sup, [Id, Params#{user => User}]) of
        {ok, Pid} ->
            Mon = erlang:monitor(process, Pid),
            true = ets:insert_new(benchmarks, {Id, Pid}),
            {ok, Id, State#{next_id => Id + 1, monitors => maps:put(Mon, Id, Mons)}};
        {error, Reason} ->
            {error, Reason}
    end.

save_results(Id, Status, #{data_dir:= Dir}) ->
    try
        Filename = filename:join([Dir, erlang:integer_to_list(Id), "status"]),
        ok = filelib:ensure_dir(Filename),
        ok = file:write_file(Filename, io_lib:format("~p.", [Status])),
        true = ets:insert(benchmarks, {Id, Status})
    catch
        _:Error ->
            lager:error("Save bench #~b results failed with reason: ~p~n~p", [Id, Error, erlang:get_stacktrace()])
    end.

import_data(Dir) ->
    bc_migrate_19_06_15(Dir),

    lager:info("Importing server data from ~s", [Dir]),
    Import = fun (File, Max) ->
        try
            ["status", IdStr | _] = lists:reverse(filename:split(File)),
            Id = erlang:list_to_integer(IdStr),
            {ok, [Status]} = file:consult(File),
            #{status:= _, start_time := _, finish_time := _, config := #{}} = Status,

            ets:insert(benchmarks, {Id, Status}),

            max(Id, Max)
        catch
            _:Error ->
                lager:error("Import from file ~s failed with reason: ~p", [File, Error]),
                Max
        end
    end,
    filelib:fold_files(Dir, "^status$", true, Import, -1).

% BC code begin
bc_migrate_19_06_15(Dir) ->
    try
        BenchmarkDir = filename:join([Dir, "..", "benchmarks"]),
        lists:foreach(fun (BDir) ->
            try
                [_, IdStr, _] = string:tokens(filename:basename(BDir), "-"),
                Files = filelib:wildcard(filename:join(BDir, "*")),
                Target = filename:join([Dir, IdStr, "."]),
                ok = filelib:ensure_dir(Target),
                _ = [file:copy(F, filename:join(Target, filename:basename(F))) || F <- Files],
                [file:delete(F) || F <- Files],
                file:del_dir(BDir),
                ok
            catch
                _:E ->
                    lager:error("Server data migration error: ~s, ~p", [BDir, E]),
                    ok
            end
        end, filelib:wildcard(filename:join(BenchmarkDir, "*"))),
        file:del_dir(BenchmarkDir),

        ServerDir = filename:join([Dir, "..", "server"]),
        lists:foreach(fun (F) ->
            try
                IdStr = filename:basename(F),
                Target = filename:join([Dir, IdStr, "status"]),
                ok = filelib:ensure_dir(Target),
                {ok, [Cfg]} = file:consult(F),
                #{log_file := LogFile} = Cfg,
                NewCfg = Cfg#{log_file => filename:join([Dir, IdStr, "log.txt"]),
                              metrics_file => filename:join([Dir, IdStr, "metrics.txt"])},
                file:write_file(Target, io_lib:format("~p.", [NewCfg])),
                file:delete(F),
                file:del_dir(filename:dirname(F))
            catch
                _:E ->
                    lager:error("Server data migration error: ~s, ~p", [F, E]),
                    ok
            end
        end, filelib:wildcard(filename:join([ServerDir, "*", "*"]))),
        file:del_dir(ServerDir)
    catch
        _:Error ->
            lager:error("Server data migration exception: ~p~n~p", [Error, erlang:get_stacktrace()])
    end.

% BC code end

wait_processes(Pids, Callback) ->
    MonRefs = [erlang:monitor(process, P) || P <- Pids],
    lists:foreach(fun (Ref) ->
        receive
            {'DOWN', Ref, _, _, _} -> ok
        end
    end, MonRefs),
    lager:info("[ SERVER ] All bechmarks have been stopped"),
    Callback().

sys_username() ->
    case os:getenv("REMOTE_USER") of
        false ->
            case os:getenv("USER") of
                false ->
                    try mzb_api_provision:exec_format("who am i | awk '{print $1}'", [], [], undefined) of
                        "" -> mzb_api_provision:exec_format("whoami", [], [], undefined);
                        User -> User
                    catch
                        _:_ -> mzb_api_provision:exec_format("whoami", [], [], undefined)
                    end;
                User -> User
            end;
        User -> User
    end.

