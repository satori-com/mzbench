-module(mzb_bench_sup).
-export([start_link/0, is_ready/0, connect_nodes/1, run_bench/2, start_pool/1]).

-behaviour(supervisor).
-export([init/1]).

-include_lib("mzbench_language/include/mzbl_types.hrl").

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

run_bench(ScriptPath, DefaultEnv) ->
    try
        {Body0, Env0} = read_and_validate(ScriptPath, DefaultEnv),
        Env1 = mzb_script_hooks:pre_hooks(Body0, Env0),
        {Body2, Env2} = read_and_validate(ScriptPath, Env1),
        Result = run_director(Body2, Env2),
        mzb_script_hooks:post_hooks(Body2, Env2),
        {ok, Result}
    catch _C:{error, Errors} = E when is_list(Errors) -> E;
          C:E ->
              ST = erlang:get_stacktrace(),
              lager:error("Failed to run benchmark ~p:~p~n~p", [C, E, ST]),
              {error, [mzb_string:format("Failed to run benchmark", [])]}
    end.

read_and_validate(Path, Env) ->
    case mzb_script_validator:read_and_validate(Path, Env) of
        {ok, Body, NewEnv} -> {Body, NewEnv};
        {error, _, _, _, Errors} -> erlang:error({error, Errors})
    end.

run_director(Body, Env) ->
    Nodes = retrieve_worker_nodes(),

    case start_director(Body, Nodes, Env) of
        {ok, _, _} -> ok;
        {ok, _} -> ok;
        {error, _Error} -> erlang:error({error, ["Unable to start director supervisor"]})
    end,

    case get_results() of
        {error, _, Error} -> erlang:error({error, [Error]});
        {ok, Result} -> Result
    end.

is_ready() ->
    try
        Apps = application:which_applications(),
        false =/= lists:keyfind(mzbench, 1, Apps)
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

get_results() ->
    try
        Pid = whereis(?MODULE),
        D = get_director(Pid),
        mzb_director:attach(D)
    catch
        _:E ->
            ST = erlang:get_stacktrace(),
            Str = mzb_string:format("Unexpected error: ~p~n~p", [E, ST]),
            {error, {unexpected_error, E, ST}, Str}
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

start_director(Body, Nodes, Env) ->
    BenchName = mzbl_script:get_benchname(mzbl_script:get_real_script_name(Env)),
    lager:info("[ mzb_bench_sup ] Loading ~p Nodes: ~p", [BenchName, Nodes]),
    supervisor:start_child(?MODULE,
                            child_spec(director, mzb_director,
                                       [whereis(?MODULE), BenchName, Body, Nodes, Env],
                                       transient)).

retrieve_worker_nodes() ->
    Nodes = erlang:nodes(),
    case erlang:length(Nodes) of
        0   ->  [erlang:node()];    % If no worker node is available, use the director node
        _ -> Nodes
    end.
