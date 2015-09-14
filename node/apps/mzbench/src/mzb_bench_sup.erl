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

run_bench(ScriptFileName, Env) ->
    Stages = [read_and_validate, pre_hook, read_and_validate, start_director, get_result, post_hook],
    DefaultParams = #{path => ScriptFileName, env => mzbl_script:normalize_env(Env)},
    Ret = lists:foldl(fun safely_run/2, DefaultParams, Stages),
    case Ret of
        #{result := Result} -> {ok, Result};
        {error, _} = E -> E
    end.

safely_run(_, {error, _} = E) -> E;
safely_run(Stage, Params) ->
    try
        run(Stage, Params)
    catch C:E ->
        ST = erlang:get_stacktrace(),
        lager:error("Unable to run bench stage ~p ~p:~p~n~p", [Stage, C, E, ST]),
        {error, [mzb_string:format("Unable to run bench stage ~p", [Stage])]}
    end.

run(read_and_validate, #{path:=Path, env:=Env} = Params) ->
    case mzb_script_validator:read_and_validate(Path, Env) of
        {ok, Body, NewEnv} -> Params#{body => Body, env => NewEnv};
        {error, _, _, _, Errors} -> {error, Errors}
    end;
run(HookKind, #{body:= Body, env:=Env} = Params)
        when HookKind == pre_hook; HookKind == post_hook ->
    {ok, NewEnv} =  mzb_script_hooks:process_hooks_on_nodes(HookKind, Body, Env),
    Params#{env => NewEnv};
run(start_director, #{body:= Body, env:=Env} = Params) ->
    Nodes = retrieve_worker_nodes(),
    case start_director(Body, Nodes, Env) of
        {ok, _, _} -> Params;
        {ok, _} -> Params;
        {error, Error} -> {error, ["Unable to start director supervisor"]}
    end;
run(get_result, Params) ->
    case get_results() of
        {error, _, Error} -> {error, [Error]};
        {ok, Result} ->
            Params#{result => Result}
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
                                       [whereis(?MODULE), BenchName, Body, Nodes, Env, undefined],
                                       transient)).

retrieve_worker_nodes() ->
    Nodes = erlang:nodes(),
    case erlang:length(Nodes) of
        0   ->  [erlang:node()];    % If no worker node is available, use the director node
        _ -> Nodes
    end.
