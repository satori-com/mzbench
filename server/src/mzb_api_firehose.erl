-module(mzb_api_firehose).

-behaviour(gen_event).

-export([update_bench/1,
         update_metrics/2,
         transmit_metrics/2,
         notify/2]).

% gen_event
-export([init/1,
         handle_call/2,
         handle_event/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-spec update_bench(term()) -> ok.
update_bench(Status) ->
    gen_event:notify(mzb_api_firehose, {update_bench, Status}).

-spec update_metrics(non_neg_integer(), term()) -> ok.
update_metrics(BenchId, Values) ->
    gen_event:notify(mzb_api_firehose, {update_metrics, BenchId, Values}).

-spec transmit_metrics(non_neg_integer(), term()) -> ok.
transmit_metrics(BenchId, Values) ->
    gen_event:notify(mzb_api_firehose, {transmit_metrics, BenchId, Values}).

-spec notify(Severity, Msg) -> ok when
    Severity :: success | info | warning | danger,
    Msg :: string().
notify(Severity, Message) ->
    gen_event:notify(mzb_api_firehose, {notify, Severity, Message}).

init([WSPid]) -> {ok, WSPid}.

terminate(_Args, _State) -> ok.

handle_event(Message, WSPid) ->
    WSPid ! Message,
    {ok, WSPid}.

handle_call(_Request, State) -> {ok, ok, State}.

handle_info(_Info, State) -> {ok, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
