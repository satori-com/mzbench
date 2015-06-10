-module(mzb_exometer_report_apiserver).

-behaviour(exometer_report).
-behaviour(gen_event).

%% exometer
-export([exometer_init/1,
         exometer_info/2,
         exometer_cast/2,
         exometer_call/3,
         exometer_report/5,
         exometer_subscribe/5,
         exometer_unsubscribe/4,
         exometer_newentry/2,
         exometer_setopts/4,
         exometer_terminate/2]).

%% gen_event
-export([init/1,
         handle_event/2,
         terminate/2,
         code_change/3,
         handle_call/2,
         handle_info/2]).

exometer_report(Probe, DataPoint, _Extra, Value, St) ->
    gen_event:notify(metrics_event_manager, {report, [Probe, DataPoint, Value]}),
    {ok, St}.

exometer_init(_Opts) -> {ok, {}}.
exometer_subscribe(_Metric, _DataPoint, _Extra, _Interval, St) -> {ok, St }.
exometer_unsubscribe(_Metric, _DataPoint, _Extra, St) -> {ok, St }.
exometer_call(_, _From, St) -> {ok, St}.
exometer_cast(_, St) -> {ok, St}.
exometer_info(_, St) -> {ok, St}.
exometer_newentry(_Entry, St) -> {ok, St}.
exometer_setopts(_Metric, _Options, _Status, St) -> {ok, St}.
exometer_terminate(_, _) -> ignore.

% gen_event

handle_event({report, Report}, ServerPid) ->
    ok = gen_server:call(ServerPid, {report, Report}),
    {ok, ServerPid}.

init([ServerPid]) -> {ok, ServerPid}.
terminate(_Args, _State) -> ok.
handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
