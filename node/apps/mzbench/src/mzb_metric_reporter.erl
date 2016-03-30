-module(mzb_metric_reporter).

-behaviour(gen_event).

%% exometer
-export([
         report/2
         ]).

%% gen_event
-export([init/1,
         handle_event/2,
         terminate/2,
         code_change/3,
         handle_call/2,
         handle_info/2]).

report(Metric, Value) ->
    gen_event:notify(metrics_event_manager, {report, [Metric, Value]}).

% gen_event

handle_event({report, Report}, ServerPid) ->
    ok = gen_server:call(ServerPid, {report, Report}),
    {ok, ServerPid}.

init([ServerPid]) -> {ok, ServerPid}.
terminate(_Args, _State) -> ok.
handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
