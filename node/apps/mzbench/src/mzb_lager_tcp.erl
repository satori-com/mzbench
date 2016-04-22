-module(mzb_lager_tcp).

-behaviour(gen_event).

-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2,
        code_change/3]).

-record(state, {level :: {'mask', integer()},
                formatter :: atom(),
                format_config :: any(),
                colors=[] :: list(),
                socket,
                n = 0,
                skip_messages = 0,
                msg_q_len_limit = undefined,
                last_timestamp = os:timestamp(),
                last_n = 0,
                limit = 0,
                is_active = true,
                dropped = 0}).

-define(TERSE_FORMAT,[time, " ", color, "[", severity,"] ", pid , " ", message, "\n"]).
-define(INTERVAL, 100). % in ms

init([Level, Sock, MessageQLenLimit, RateLimit]) ->
    system_log:info("Started tcp lager backend for ~p ~p", [Level, Sock]),
    erlang:process_flag(trap_exit, true),
    Formatter = lager_default_formatter,
    FormatterConfig = ?TERSE_FORMAT,
    RateLimit > 0 andalso erlang:send_after(?INTERVAL, self(), trigger_rate_limiter),
    mzb_metrics:notify("errors", 0),
    {ok, #state{level=lager_util:config_to_mask(Level),
            formatter=Formatter,
            format_config=FormatterConfig,
            colors=[],
            socket = Sock,
            msg_q_len_limit = MessageQLenLimit,
            limit = RateLimit}}.

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

handle_event({log, _}, #state{is_active = false, dropped = N} = State) ->
    {ok, State#state{dropped = N + 1}};

handle_event({log, _}, #state{skip_messages = N} = State) when N > 0 ->
    {ok, State#state{skip_messages = N - 1}};

handle_event({log, Message},
    #state{socket = Socket, level=L,formatter=Formatter,format_config=FormatConfig,colors=Colors, n = N, msg_q_len_limit = MaxQ} = State) ->

    case (MaxQ /= undefined) andalso (N rem 10 == 0) andalso erlang:process_info(self(), message_queue_len) of
        {_, Len} when Len > MaxQ ->
            send_direct_warning("Dropped ~b log messages (mailbox overflow) on ~p", [Len div 2, node()], State),
            mzb_metrics:notify({"logs.dropped.mailbox_overflow", counter}, Len div 2),
            {ok, State#state{skip_messages = Len div 2, n = N + 1}};
        _ ->
            _ = case lager_msg:severity(Message) of
                error -> mzb_metrics:notify("errors", 1);
                _ -> ok
            end,
            mzb_metrics:notify({"logs.written", counter}, 1),

            case lager_util:is_loggable(Message, L, ?MODULE) of
                true ->
                    _ = gen_tcp:send(Socket, Formatter:format(Message,FormatConfig,Colors)),
                    {ok, State#state{n = N + 1}};
                false ->
                    {ok, State#state{n = N + 1}}
            end
    end;
handle_event(_Event, State) ->
    {ok, State}.

handle_info(activate, State = #state{n = N, dropped = Dropped}) ->
    case Dropped > 0 of
        true ->
            mzb_metrics:notify({"logs.dropped.rate_limiter", counter}, Dropped),
            send_direct_warning("Dropped ~b log messages (rate limiter) on ~p", [Dropped, node()], State);
        false ->
            ok
    end,
    erlang:send_after(?INTERVAL, self(), trigger_rate_limiter),
    {ok, State#state{is_active = true, last_n = N, last_timestamp = os:timestamp(), dropped = 0}};

handle_info(trigger_rate_limiter, State = #state{limit = LimitPerMinute, n = N, last_n = LastN, last_timestamp = LastTimestamp}) ->
    Timestamp = os:timestamp(),
    T = timer:now_diff(Timestamp, LastTimestamp),
    K = max(0, (N - LastN) - ((LimitPerMinute * T) div 1000000)),
    case K > 0 of
        true ->
            {_, QLen} = erlang:process_info(self(), message_queue_len),
            erlang:send_after(((K + QLen) * 1000) div LimitPerMinute, self(), activate),
            {ok, State#state{is_active = false}};
        false ->
            erlang:send_after(?INTERVAL, self(), trigger_rate_limiter),
            {ok, State#state{last_n = N, last_timestamp = Timestamp}}
    end;
handle_info(_Info, State) ->
    {ok, State}.

terminate(Reason, #state{socket = S} = _State) ->
    system_log:info("Terminated tcp lager backend for ~p with reason ~p", [S, Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

send_direct_warning(F, A, #state{socket = Socket, formatter = Formatter, format_config = FormatConfig, colors = Colors}) ->
    Str = io_lib:format(F, A),
    NewMsg = lager_msg:new(Str, warning, [{pid, self()}], []),
    _ = gen_tcp:send(Socket, Formatter:format(NewMsg, FormatConfig, Colors)),
    ok.
