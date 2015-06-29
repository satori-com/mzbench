-module(mz_histogram).

-export([start_link/0,
         create/1,
         create/2,
         notify/2,
         get_raw_data/0,
         get_and_remove_raw_data/0,
         get_raw_data/1,
         get_bucket/2,
         merge_histograms/2]).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([benchmark/2, prun/3]).

-define(SIGNIFICANT_FIGURES, 3).
-define(HIGHEST_VALUE, 3600000000).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create(Name) ->
    gen_server:call(?MODULE, {create, Name}).

create(Nodes, Name) ->
    {Results, []} = gen_server:multi_call(Nodes, ?MODULE, {create, Name}),
    case lists:usort([R || {_, R} <- Results]) of
        [ok] -> ok;
        _ -> {error, Results}
    end.

notify(Name, Value) when is_number(Value), Value >= 0 ->
    notify_many(Name, Value, 1);
notify(_Name, Value) ->
    erlang:error({value_out_of_range, Value}).

notify_many(Name, Value, Count) when is_number(Count) ->
    try
        ets:update_counter(erlang:get({mz_hist_tid, Name}), get_bucket(?SIGNIFICANT_FIGURES, erlang:trunc(Value)), Count)
    catch
        _:_ ->
            Tid = ets:lookup_element(mz_histograms, Name, 2),
            erlang:put({mz_hist_tid, Name}, Tid),
            ets:update_counter(Tid, get_bucket(?SIGNIFICANT_FIGURES, erlang:trunc(Value)), Count)
    end.


-spec get_bucket(pos_integer(), integer()) -> integer().
get_bucket(2, V)  -> get_bucket2(V);
get_bucket(3, V)  -> get_bucket3(V);
get_bucket(SF, V) -> get_bucket_(SF, V).

-spec get_bucket2(integer()) -> integer().
get_bucket2(V) when V < 100         -> V;
get_bucket2(V) when V < 1000        -> (V div 10)        * 10;
get_bucket2(V) when V < 10000       -> (V div 100)       * 100;
get_bucket2(V) when V < 100000      -> (V div 1000)      * 1000;
get_bucket2(V) when V < 1000000     -> (V div 10000)     * 10000;
get_bucket2(V) when V < 10000000    -> (V div 100000)    * 100000;
get_bucket2(V) when V < 100000000   -> (V div 1000000)   * 1000000;
get_bucket2(V) when V < 1000000000  -> (V div 10000000)  * 10000000;
get_bucket2(V) when V < 10000000000 -> (V div 100000000) * 100000000;
get_bucket2(V) -> get_bucket_(2, V).

-spec get_bucket3(integer()) -> integer().
get_bucket3(V) when V < 1000        -> V;
get_bucket3(V) when V < 10000       -> (V div 10)       * 10;
get_bucket3(V) when V < 100000      -> (V div 100)      * 100;
get_bucket3(V) when V < 1000000     -> (V div 1000)     * 1000;
get_bucket3(V) when V < 10000000    -> (V div 10000)    * 10000;
get_bucket3(V) when V < 100000000   -> (V div 100000)   * 100000;
get_bucket3(V) when V < 1000000000  -> (V div 1000000)  * 1000000;
get_bucket3(V) when V < 10000000000 -> (V div 10000000) * 10000000;
get_bucket3(V) -> get_bucket_(3, V).

-spec get_bucket_(pos_integer(), integer()) -> integer().
get_bucket_(SF, V) ->
    F = erlang:trunc(math:log10(V)) + 1,
    case F =< SF of
        true -> V;
        false ->
            M = erlang:trunc(math:pow(10, F - SF)),
        (V div M) * M
    end.

get_raw_data() ->
    ets:foldl(
        fun ({Name, _}, Acc) ->
            [{Name, get_raw_data(Name)} | Acc]
        end, [], mz_histograms).

get_and_remove_raw_data() ->
    Data = get_raw_data(),
    lists:map(fun({Name, Datapoints}) ->
        lists:map(fun({K, V}) -> notify_many(Name, K, -V) end, Datapoints) end, Data),
    Data.

get_raw_data(Name) ->
    case ets:lookup(mz_histograms, Name) of
        [] -> erlang:error({histogram_not_found, Name});
        [{_, Tid}] -> ets:tab2list(Tid)
    end.

merge_histograms(DataList, Datapoints) ->
    {ok, Ref} = hdr_histogram:open(?HIGHEST_VALUE, ?SIGNIFICANT_FIGURES),
    try
        lists:foreach(fun (Values) -> import_hdr_data(Ref, Values) end, DataList),
        Stats = lists:map(
            fun (min) -> hdr_histogram:min(Ref);
                (max) -> hdr_histogram:max(Ref);
                (mean) -> hdr_histogram:mean(Ref);
                (median) -> hdr_histogram:median(Ref);
                (N) when N =< 100 ->
                    hdr_histogram:percentile(Ref, erlang:float(N));
                (N) when N =< 1000 ->
                    hdr_histogram:percentile(Ref, N / 10)
            end, Datapoints),
        lists:zip(Datapoints, Stats)
    after
        hdr_histogram:close(Ref)
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    _ = ets:new(mz_histograms,  [set, public, named_table, {read_concurrency, true}]),
    {ok, []}.

handle_call({create, Name}, _From, State) ->
    {reply, init_hist(Name), State};

handle_call(Req, _From, State) ->
    lager:error("Unhandled call: ~p", [Req]),
    {stop, {unhandled_call, Req}, State}.

handle_cast(Msg, State) ->
    lager:error("Unhandled cast: ~p", [Msg]),
    {stop, {unhandled_cast, Msg}, State}.

handle_info(Info, State) ->
    lager:error("Unhandled info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

init_hist(Name) ->
    Tid = ets:new(erlang:list_to_atom(Name), [set, public, {write_concurrency, true}]),
    ets:insert(mz_histograms, {Name, Tid}),
    init_hist(Tid, math:pow(10, ?SIGNIFICANT_FIGURES), 0).

init_hist(_, Max, C) when C >= Max -> ok;
init_hist(Name, Max, C) ->
    L = erlang:trunc(math:log10(?HIGHEST_VALUE) - ?SIGNIFICANT_FIGURES + 1),
    lists:foreach(
        fun (K) ->
            B = round(C * math:pow(10, K)),
            case B =< ?HIGHEST_VALUE of
                true  -> ets:insert_new(Name, {B, 0});
                false -> ok
            end
        end, lists:seq(0, L)),
    init_hist(Name, Max, C + 1).

import_hdr_data(_Ref, []) -> ok;
import_hdr_data(Ref, [{_, 0}|T]) ->
    import_hdr_data(Ref, T);
% workaround for hdr_histogram bug (record_many doesn't work for 0)
import_hdr_data(Ref, [{0, V}|T]) ->
    ok = hdr_histogram:record(Ref, 0),
    import_hdr_data(Ref, [{0, V - 1}|T]);
import_hdr_data(Ref, [{K, V}|T]) ->
    case hdr_histogram:record_many(Ref, K, V) of
        ok -> ok;
        {error, Reason} ->
            lager:error("Failed to hdr_histogram:record_many~nReason: ~p~nParams: ~p", [Reason, [Ref, K, V]]),
            erlang:error({record_many_error, [Ref, K, V]})
    end,
    import_hdr_data(Ref, T).

%%%===================================================================
%%% benchmarks
%%%===================================================================

benchmark(N, P) ->
    ok = case start_link() of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok;
        Error -> Error
    end,
    create("my_hist"),
    {Time, _} = timer:tc(?MODULE, prun, [N, P, fun (K) -> notify("my_hist", K) end]),
    io:format("Result: ~f updates/s~n", [(N * 1000000) / Time]),
    Time.

prun(N, P, F) ->
    mzbl_utility:pmap(
        fun (_) ->
            run(0, N div P, F)
        end, lists:seq(1, P)).

run(Max, Max, _) -> ok;
run(N, Max, F) ->
    F(N),
    run(N + 1, Max, F).



