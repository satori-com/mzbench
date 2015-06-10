-module(mzb_bench_sup).
-export([start_link/0, is_ready/0, connect_nodes/1, run_script/1,
         run_script/2, run_script/3, get_results/1, start_pool/1]).

-behaviour(supervisor).
-export([init/1]).

-include("mzb_types.hrl").
-include("mzb_ast.hrl").

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

run_script(ScriptFileName) ->
    run_script(ScriptFileName, []).
run_script(ScriptFileName, Env) ->
    run_script(ScriptFileName, Env, undefined).
run_script(ScriptFileName, Env, ReportFile) ->
    Nodes = retrieve_worker_nodes(),
    Env2 = normalize_env(Env),
    case mzb_script:read_and_validate(ScriptFileName, Env2) of
        {ok, Body, Env3} -> 
            ok = case start_director(Body, Nodes, Env3, ReportFile) of
                {ok, _, _} -> ok;
                {ok, _} -> ok;
                Error -> Error
            end,
            {ok, whereis(?MODULE)};
        {error, _, _, _, _} = E -> E
    end.

is_ready() ->
    try
        Apps = application:which_applications(),
        false =/= lists:keyfind(mz_bench, 1, Apps)
    catch
        _:Error ->
            lager:error("is_ready exception: ~p~nStacktrace: ~p", [Error, erlang:get_stacktrace()]),
            false
    end.

connect_nodes(Nodes) ->
    lists:filter(
        fun (N) ->
            pong == net_adm:ping(N)
        end, Nodes).

get_director(Sup) ->
    case lists:keyfind(director, 1, supervisor:which_children(Sup)) of
        {_, Pid, _, _} -> Pid;
        false -> erlang:error(no_director)
    end.

get_results(Pid) ->
    try
        D = get_director(Pid),
        mzb_director:attach(D)
    catch
        _:E ->
            ST = erlang:get_stacktrace(),
            Str = io_lib:format("Unexpected error: ~p~n~p", [E, ST]),
            {error, {unexpected_error, E, ST}, lists:flatten(Str)}
    end.

start_pool(Args) ->
    supervisor:start_child(?MODULE, child_spec(make_ref(), mzb_pool, Args, transient)).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->
    lager:info("[ mzb_bench_sup ] I'm at ~p", [self()]),
    {ok, {{one_for_all, 0, 1}, [
        child_spec(signaler, mzb_signaler, [], permanent)
    ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
child_spec(Name, Module, Args, Restart) ->
    {Name, {Module, start_link, Args}, Restart, 5000, worker, [Module]}.

start_director(Body, Nodes, Env, ReportFile) ->
    BenchName = mzb_script:get_benchname(mzb_script:get_real_script_name(Env)),
    lager:info("[ mzb_bench_sup ] Loading ~p Nodes: ~p", [BenchName, Nodes]),
    supervisor:start_child(?MODULE, 
                            child_spec(director, mzb_director, 
                                       [whereis(?MODULE), BenchName, Body, Nodes, Env, ReportFile],
                                       transient)).

retrieve_worker_nodes() ->
    Nodes = erlang:nodes(),
    case erlang:length(Nodes) of
        0   ->  [erlang:node()];    % If no worker node is available, use the director node
        _ -> Nodes
    end.

normalize_env(Env) ->
    lists:map(
        fun ({K, V}) -> {normalize_env_(K), normalize_env_(V)}
        end, Env).

normalize_env_(V) when is_binary(V) -> erlang:binary_to_list(V);
normalize_env_(V) when is_list(V) -> V;
normalize_env_(V) when is_integer(V) -> V;
normalize_env_(U) ->
    Msg = lists:flatten(io_lib:format("Env value of unknown type: ~p", [U])),
    erlang:error({error, {validation, [Msg]}}).
