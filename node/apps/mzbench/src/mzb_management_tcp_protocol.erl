-module(mzb_management_tcp_protocol).

-behaviour(ranch_protocol).
-behaviour(gen_server).

%% API
-export([start_link/4]).

%% gen_server
-export([init/1,
         init/4,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         get_port/0]).

-define(TIMEOUT, 60000).

-record(state, {
    socket :: any(),
    transport :: module()
    }).

%% API.

start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

dispatch({request, Ref, Msg}, State) ->
    system_log:info("Received request: ~p", [Msg]),
    try
        ReplyFun = fun (Reply) ->  send_message({response, Ref, Reply}, State) end,
        case handle_message(Msg, ReplyFun) of
            {reply, Reply} -> ReplyFun(Reply);
            noreply -> ok
        end
    catch
        _:Error ->
            system_log:error("Api server message handling exception: ~p~n~p", [Error, erlang:get_stacktrace()])
    end,
    {noreply, State};

dispatch(close_req, #state{socket = Socket, transport = Transport} = State) ->
    Transport:close(Socket),
    {stop, normal, State};

dispatch(Unhandled, State) ->
    system_log:error("Unhandled tcp message: ~p", [Unhandled]),
    {noreply, State}.

get_port() ->
    ranch:get_port(management_tcp_server).

handle_message({start_benchmark, ScriptPath, Env}, _) ->
    {reply, mzb_bench_sup:run_bench(ScriptPath, Env)};

handle_message(get_results, ReplyFun) ->
    _ = erlang:spawn(fun () -> ReplyFun(mzb_bench_sup:get_results()) end),
    noreply;

handle_message({metric_names, ScriptPath, Env}, _) ->
    try
        case mzb_script_validator:read_and_validate(ScriptPath, mzbl_script:normalize_env(Env)) of
            {ok, _Warnings, _Body0, Env0} ->
                {reply, {ok, mzb_script_metrics:metrics(ScriptPath, Env0)}};
            {error, _, _, _, Errors} ->
                {reply, {error, Errors}}
        end
    catch
        _:E -> {reply, {error, [mzb_string:format("Unexpected exception on metrics gathering: ~p~n~p", [E, erlang:get_stacktrace()])]}}
    end;

handle_message({change_env, Env}, _) ->
    {reply, mzb_director:change_env(Env)};

handle_message({get_log_port, Node}, _) ->
    case rpc:call(Node, ranch, get_port, [lager_tcp_server]) of
        {badrpc, Reason} -> {reply, {error, {badrpc, Node, Reason}}};
        Port -> {reply, {ok, Port}}
    end;

handle_message({get_log_user_port, Node}, _) ->
    case rpc:call(Node, ranch, get_port, [lager_tcp_server_user]) of
        {badrpc, Reason} -> {reply, {error, {badrpc, Node, Reason}}};
        Port -> {reply, {ok, Port}}
    end;

handle_message(Msg, _) ->
    erlang:error({unhandled, Msg}).

init([State]) -> {ok, State}.

init(Ref, Socket, Transport, _Opts = []) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, true}, {packet, 4}, binary]),

    gen_event:add_handler(metrics_event_manager, {mzb_metric_reporter, self()}, [self()]),
    gen_server:enter_loop(?MODULE, [], #state{socket=Socket, transport=Transport}, ?TIMEOUT).

handle_info({tcp_closed, _Socket}, State) ->
    mzb_director:notify(server_connection_closed),
    {stop, normal, State};

handle_info(timeout, State) ->
    {stop, normal, State};

handle_info({tcp, Socket, Msg}, State = #state{socket = Socket}) ->
    dispatch(erlang:binary_to_term(Msg), State);

handle_info({tcp_error, _, Reason}, State) ->
    system_log:warning("~p was closed with error: ~p", [?MODULE, Reason]),
    {stop, Reason, State};

handle_info(Info, State) ->
    system_log:error("~p has received unexpected info: ~p", [?MODULE, Info]),
    {stop, normal, State}.

handle_cast(Msg, State) ->
    system_log:error("~p has received unexpected cast: ~p", [?MODULE, Msg]),
    {noreply, State}.

handle_call({report, [Name, Value]}, _From, State = #state{}) ->
    %Name = string:join(Probe, "."),
    Metric = io_lib:format("~B\t~p~n", [unix_time(), Value]),
    send_message({metric_value, Name, Metric}, State),
    {reply, ok, State};

handle_call(Request, _From, State) ->
    system_log:error("~p has received unexpected call: ~p", [?MODULE, Request]),
    {reply, ignore, State}.

terminate(_Reason, _State) ->
    lager:info("Management tcp connection terminated: ~p", [_Reason]),
    gen_event:delete_handler(metrics_event_manager, {mzb_metric_reporter, self()}, []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

unix_time() ->
    {Mega, Secs, _} = os:timestamp(),
    Mega * 1000000 + Secs.

send_message(Msg, #state{socket = Socket, transport = Transport}) ->
    Transport:send(Socket, erlang:term_to_binary(Msg)).
