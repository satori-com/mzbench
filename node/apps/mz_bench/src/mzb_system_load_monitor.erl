-module(mzb_system_load_monitor).

-behaviour(gen_server).

%% API
-export([start_link/0,
    metric_names/1
    ]).

%% gen_server callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3
		]).

-record(state, {}).

interval() -> 10000. % ten seconds

%% API functions

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

metric_names(Nodes) ->
    [
       [{metric_name(T, N), gauge} || N <- Nodes]
       || T <- ["la1", "cpu", "ram"]
    ].

%% gen_server callbacks

init([]) ->
    lager:info("~p started on node ~p", [?MODULE, node()]),
    erlang:send_after(interval(), self(), trigger),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(trigger, State) ->

    case cpu_sup:avg1() of
        {error, LAFailedReason} ->
            lager:info("cpu_sup:avg1() failed with reason ~p", [LAFailedReason]);
        La1 ->
            ok = mzb_metrics:notify({metric_name("la1"), gauge}, La1 / 256)
    end,

    {TotalMem, AllocatedMem, _} = memsup:get_memory_data(),
    ok = mzb_metrics:notify({metric_name("ram"), gauge}, AllocatedMem / TotalMem),

    case os:type() of
        {unix, linux} ->
            case cpu_sup:util() of
                {error, UtilFailedReason} ->
                    lager:info("cpu_sup:util() failed with reason ~p", [UtilFailedReason]);
                CpuUtil ->
                    ok = mzb_metrics:notify({metric_name("cpu"), gauge}, CpuUtil)
            end;
        % TODO: solaris supports cpu_sup:util too
        _ -> ok
    end,

    %lager:info("System load at ~p: cpu ~p, la ~p, ram ~p", [node(), Cpu, La1, AllocatedMem / TotalMem]),
    erlang:send_after(interval(), self(), trigger),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions


metric_name(GaugeName) ->
    metric_name(GaugeName, atom_to_list(node())).

metric_name(GaugeName, Node) when is_atom(Node) ->
    metric_name(GaugeName, atom_to_list(Node));
metric_name(GaugeName, Node) ->
    "systemload." ++ GaugeName ++ "." ++ string:join(string:tokens(Node, "@"), ".").
