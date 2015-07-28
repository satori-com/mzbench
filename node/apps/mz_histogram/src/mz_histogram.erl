-module(mz_histogram).

-export([start_link/0,
         create/1,
         create/2,
         notify/2,
         get_and_remove_raw_data/0,
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
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], [{spawn_opt, [{priority, high}]}]).

create(Name) ->
    gen_server:call(?MODULE, {create, Name}).

create(Nodes, Name) ->
    {Results, []} = gen_server:multi_call(Nodes, ?MODULE, {create, Name}),
    case lists:usort([R || {_, R} <- Results]) of
        [ok] -> ok;
        _ -> {error, Results}
    end.

get_and_remove_raw_data() ->
    ets:foldl(
        fun ({Name, Ref}, Acc) ->
            % TODO: this must be one atomic operation
            Bin = hdr_histogram:to_binary(Ref, [{compression, none}]),
            ok = hdr_histogram:reset(Ref),
            [{Name, Bin} | Acc]
        end, [], mz_histograms).


notify(Name, Value) when is_integer(Value), Value >= 0 ->
    case erlang:get({mz_hist_ref, Name}) of
        undefined -> 
            Ref = ets:lookup_element(mz_histograms, Name, 2),
            erlang:put({mz_hist_ref, Name}, Ref),
            hdr_histogram:record(Ref, Value);
        Ref ->
            hdr_histogram:record(Ref, Value)
    end;
notify(Name, Value) when is_float(Value) ->
    notify(Name, round(Value));
notify(_Name, Value) ->
    erlang:error({value_out_of_range, Value}).

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
    {ok, Ref} = hdr_histogram:open(?HIGHEST_VALUE, ?SIGNIFICANT_FIGURES),
    ets:insert(mz_histograms, {Name, Ref}),
    ok.

import_hdr_data(To, BinHdrHistData) ->
    {ok, From} = hdr_histogram:from_binary(BinHdrHistData),
    _ = hdr_histogram:add(To, From),
    ok = hdr_histogram:close(From).

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
    {Time1, _} = timer:tc(?MODULE, prun, [N, P, fun (K) -> notify("my_hist", K) end]),
    io:format("MzHistogram result: ~f updates/s~n", [(N * 1000000) / Time1]),
    {ok, Ref} = hdr_histogram:open(?HIGHEST_VALUE, ?SIGNIFICANT_FIGURES),
    {Time2, _} = timer:tc(?MODULE, prun, [N, P, fun (K) -> hdr_histogram:record(Ref, K) end]),
    io:format("HdrHistogram result: ~f updates/s~n", [(N * 1000000) / Time2]),
    ok = hdr_histogram:close(Ref),
    {Time1, Time2}.

prun(N, P, F) ->
    mzb_lists:pmap(
        fun (_) ->
            run(0, N div P, F)
        end, lists:seq(1, P)).

run(Max, Max, _) -> ok;
run(N, Max, F) ->
    F(N),
    run(N + 1, Max, F).



