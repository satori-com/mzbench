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

handle_message({read_and_validate, Path, Env}, _) ->
    {reply, mzb_bench_sup:read_and_validate(Path, Env)};

handle_message({connect_nodes, Hosts}, ReplyFun) ->
    {ok, Port} = application:get_env(mzbench, node_interconnect_port),
    try
        mzb_interconnect:set_director([{Host, Port} || Host <- Hosts]),
        fun Wait (0) -> ReplyFun({error, timeout});
            Wait (N) ->
                case (length(mzb_interconnect:nodes()) == length(Hosts)) of
                    true  -> ReplyFun(ok);
                    false ->
                        timer:sleep(_Timeout = 2000),
                        Wait(N - 1)
                end
        end (_Retries = 10),
        noreply
    catch
        _:E ->
            system_log:error("Connecting nodes error: ~p~n~p", [E, erlang:get_stacktrace()]),
            {reply, {error, E}}
    end;

handle_message({change_env, Env}, _) ->
    {reply, mzb_director:change_env(Env)};

handle_message({get_log_port, Node}, _) ->
    case mzb_interconnect:call(Node, get_system_log_port) of
        {badrpc, Reason} -> {reply, {error, {badrpc, Node, Reason}}};
        Port -> {reply, {ok, Port}}
    end;

handle_message({get_log_user_port, Node}, _) ->
    case mzb_interconnect:call(Node, get_user_log_port) of
        {badrpc, Reason} -> {reply, {error, {badrpc, Node, Reason}}};
        Port -> {reply, {ok, Port}}
    end;

handle_message({call_worker, WorkerType, Method, Env}, _) ->
    {Provider, Worker} = mzbl_script:resolve_worker_provider(WorkerType),
    ok = Provider:load(Worker),
    {reply, Provider:apply(Method, [Env], Worker)};

handle_message(get_metrics, _) ->
    try
        {reply, {ok, mzb_metrics:get_metrics()}}
    catch
        _:E -> {{error, E}}
    end;

handle_message(get_cumulative_histograms, _) ->
    try
        {reply, {ok, mzb_metrics:get_histogram_data()}}
    catch
        _:E -> {reply, {error, E}}
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

handle_call({new_metrics, Metrics}, _From, State = #state{}) ->
    send_message({new_metrics, #{ groups => mzb_script_metrics:build_metric_groups_json(Metrics) }}, State),
    {reply, ok, State};

handle_call({report, [Name, Value]}, _From, State = #state{}) ->
    Metric = io_lib:format("~B\t~p~n", [unix_time(), Value]),
    send_message({metric_value, Name, Metric}, State),
    {reply, ok, State};

handle_call(Request, _From, State) ->
    system_log:error("~p has received unexpected call: ~p", [?MODULE, Request]),
    {reply, ignore, State}.

terminate(_Reason, _State) ->
    system_log:info("Management tcp connection terminated: ~p", [_Reason]),
    gen_event:delete_handler(metrics_event_manager, {mzb_metric_reporter, self()}, []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

unix_time() ->
    {Mega, Secs, _} = os:timestamp(),
    Mega * 1000000 + Secs.

send_message(Msg, #state{socket = Socket, transport = Transport}) ->
    Transport:send(Socket, erlang:term_to_binary(Msg)).
