-module(mzb_api_firehose_handler).

-behaviour(gen_event).

% gen_event
-export([init/1,
         handle_call/2,
         handle_event/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

init([WSPid]) -> {ok, WSPid}.

terminate(_Args, _State) -> ok.

handle_event(Message, WSPid) ->
    WSPid ! Message,
    {ok, WSPid}.

handle_call(_Request, State) -> {ok, ok, State}.

handle_info(_Info, State) -> {ok, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
