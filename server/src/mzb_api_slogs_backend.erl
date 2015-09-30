-module(mzb_api_slogs_backend).

-behaviour(gen_event).

-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2,
        code_change/3]).

-record(state, {level :: {'mask', integer()},
                formatter :: atom(),
                format_config :: any(),
                colors=[] :: list(),
                cowboy_proc :: pid()}).

-define(TERSE_FORMAT,[time, " ", color, "[", severity,"] ", pid , " ", message]).

init([Level, CowboyProc]) ->
    Formatter = lager_default_formatter,
    FormatterConfig = ?TERSE_FORMAT ++ [eol()],
    {ok, #state{level=lager_util:config_to_mask(Level),
            formatter=Formatter,
            format_config=FormatterConfig,
            colors=[],
            cowboy_proc = CowboyProc}}.

handle_call(get_loglevel, #state{level=Level} = State) ->
    {ok, Level, State};
handle_call({set_loglevel, Level}, State) ->
   try lager_util:config_to_mask(Level) of
        Levels ->
            {ok, ok, State#state{level=Levels}}
    catch
        _:_ ->
            {ok, {error, bad_log_level}, State}
    end;
handle_call(_Request, State) ->
    {ok, ok, State}.

handle_event({log, Message},
    #state{cowboy_proc = P, level=L,formatter=Formatter,format_config=FormatConfig,colors=Colors} = State) ->
    case lager_util:is_loggable(Message, L, ?MODULE) of
        true ->
            P ! {log, Formatter:format(Message,FormatConfig,Colors)},
            {ok, State};
        false ->
            {ok, State}
    end;
handle_event(_Event, State) ->
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

eol() -> "\r\n".

