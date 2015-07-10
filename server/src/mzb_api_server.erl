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
    {ok, MaxBenchNum} = application:get_env(mz_bench_api, max_bench_num),
    lager:info("Server username: ~p", [User]),
    {ok, check_max_bench_num(#{next_id => MaxId + 1,
           monitors => #{},
           status => active,
           data_dir => ServerDir,
           user => User,
           localhost_allocated => false,
           max_bench_num => MaxBenchNum})}.

server_data_dir() ->
    DataDir = application:get_env(mz_bench_api, bench_data_dir, undefined),
    filename:absname(DataDir).

handle_call({start_bench, Params}, _From, #{status:= active} = State) ->
    case start_bench_child(Params, State) of
        {ok, Id, NewState} ->
            {reply, {ok, #{id => Id, status => <<"pending">>}}, NewState};
        {error, Reason, NewState} ->
            {reply, {error, Reason}, NewState}
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

            Params =
                % BC code: migration of data, convert dont_provision_nodes to provistion_nodes
                case erlang:binary_to_term(Binary) of
                    #{dont_provision_nodes:= V} = P -> P#{provision_nodes => not V};
                    P -> P
                end,

            case start_bench_child(Params, State) of
                {ok, Id, NewState} ->
                    {reply, {ok, #{id => Id, status => <<"pending">>}}, NewState};
                {error, Reason, NewState} ->
                    {reply, {error, Reason}, NewState}
            end;
        {error, enoent} ->
            {reply, {error, not_found}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({restart_bench, RestartId}, _From, #{status:= inactive} = State) ->
    lager:info("[ SERVER ] Restart of bench failed because server is inactive #~p", [RestartId]),
    {reply, {error, server_inactive}, State};

handle_call(deactivate, _From, #{} = State) ->
    Unfinished = ets:foldl(
        fun ({_, Pid}, Acc) when is_pid(Pid) -> [Pid | Acc];
            (_, Acc) -> Acc
        end, [], benchmarks),
    lager:info("[ SERVER ] Stopping all benchmarks due to server stop: ~p", [Unfinished]),
    [ok = mzb_api_bench:interrupt_bench(P) || P <- Unfinished],
    {reply, ok, State#{status:= inactive}};

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
    save_results(Id, Status, State),
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
            NewState = State#{next_id => Id + 1, monitors => maps:put(Mon, Id, Mons)},
            {ok, Id, check_max_bench_num(NewState)};
        {error, Reason} ->
            {error, Reason, State#{next_id => Id + 1}}
    end.

check_max_bench_num(#{max_bench_num:= MaxNum, next_id:= NextId, data_dir:= Dir} = State) ->
    MinId = (NextId - MaxNum),
    ets:foldl(
        fun ({_Id, Pid}, _) when is_pid(Pid) -> ok;
            ({Id, _Status}, _) when Id >= MinId -> ok;
            ({Id, _Status}, _) ->
                BenchDir = filename:join(Dir, erlang:integer_to_list(Id)),
                lager:info("Deleting bench #~b", [Id]),
                case mzbl_utility:del_dir(BenchDir) of
                    ok -> ets:delete(benchmarks, Id);
                    {error, Reason} ->
                        lager:error("Delete directory ~p failed: ~p", [BenchDir, Reason])
                end
        end, [], benchmarks),
    State.

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
    lager:info("Importing server data from ~s", [Dir]),

    Items = mzbl_utility:wildcard(filename:join(Dir, "*")),

    Import = fun (BenchFolder, Max) ->
        File = filename:join([BenchFolder, "status"]),
        try
            IdStr = filename:basename(BenchFolder),
            Id = erlang:list_to_integer(IdStr),
            import_bench_status(Id, File),
            max(Id, Max)
        catch
            _:Error ->
                lager:error("Parsing status filename ~s failed with reason: ~p~n~p", [File, Error, erlang:get_stacktrace()]),
                Max
        end
    end,
    lists:foldl(Import, -1, Items).

import_bench_status(Id, File) ->
    try
        {ok, [Status]} = file:consult(File),
        #{status:= _, start_time := _, finish_time := _, config := #{}} = Status,
        ets:insert(benchmarks, {Id, Status})
    catch _:E ->
        lager:error("Import from file ~s failed with reason: ~p~n~p", [File, E, erlang:get_stacktrace()])
    end.

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

