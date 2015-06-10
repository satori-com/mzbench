-module(mzb_api_server).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    deactivate/0,
    start_bench/1,
    stop_bench/1,
    get_info/0,
    bench_finished/2,
    status/1,
    server_data_dir/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

deactivate() ->
    gen_server:call(?MODULE, deactivate, infinity).

start_bench(Params) ->
    case gen_server:call(?MODULE, {start_bench, Params}, infinity) of
        {ok, Resp} -> Resp;
        {error, {exception, {C,E,ST}}} -> erlang:raise(C,E,ST);
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
    ServerDir = filename:join(server_data_dir(), "server"),
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
    filename:absname(filename:join([HomeDir, "mz", DataDir])).

handle_call({start_bench, Params}, _From, #{status:= active, next_id:= Id, monitors:= Mons, user:= User} = State) ->
    lager:info("[ SERVER ] Start bench #~b", [Id]),
    case supervisor:start_child(benchmarks_sup, [Id, Params#{user => User}]) of
        {ok, Pid} ->
            Mon = erlang:monitor(process, Pid),
            true = ets:insert_new(benchmarks, {Id, Pid}),
            {reply, {ok, #{id => Id, status => <<"pending">>}}, State#{next_id => Id + 1, monitors => maps:put(Mon, Id, Mons)}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({start_bench, Params}, _From, #{status:= inactive} = State) ->
    lager:info("[ SERVER ] Start of bench failed because server is inactive ~p", [Params]),
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

save_results(Id, Status, #{data_dir:= Dir}) ->
    try
        {{Y, M, _}, {_, _, _}} = calendar:now_to_universal_time(os:timestamp()),
        SubDir = lists:flatten(io_lib:format("~4.10.0B-~2.10.0B", [Y, M])),
        Filename = filename:join([Dir, SubDir, erlang:integer_to_list(Id)]),
        ok = filelib:ensure_dir(Filename),
        ok = file:write_file(Filename, io_lib:format("~p.", [Status])),
        true = ets:insert(benchmarks, {Id, Status})
    catch
        _:Error ->
            lager:error("Save bench #~b results failed with reason: ~p~n~p", [Id, Error, erlang:get_stacktrace()])
    end.

import_data(Dir) ->
    lager:info("Importing server data from ~s", [Dir]),
    Import = fun (File, Max) ->
        try
            Id = erlang:list_to_integer(filename:basename(File)),
            {ok, [Status]} = file:consult(File),

            % BC 2015-04-01 avasenin: check metrics in the files and add them if they are absent
            Metrics = maps:get(metrics, Status, #{}),
            Status1 = Status#{metrics => Metrics},
            % END BC

            #{status:= _, start_time := _, finish_time := _, config := #{}} = Status1,

            ets:insert(benchmarks, {Id, Status1}),
            case Id > Max of
                true  -> Id;
                false -> Max
            end
        catch
            _:Error ->
                lager:error("Import from file ~s failed with reason: ~p", [File, Error]),
                Max
        end
    end,
    filelib:fold_files(Dir, "", true, Import, -1).

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

