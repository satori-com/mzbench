-module(mzb_script_hooks).

-export([validate/1, process_hooks_on_nodes/3, process_hooks/4]).

-include_lib("mzbench_language/include/mzbl_types.hrl").

run_hook(#operation{name=exec, args=[Cmd]}, Env) ->
    lager:info("Run worker hook ~p", [Cmd]),
    Logger = fun (S, F, A) -> lager:log(S, self(), F, A) end,
    mzb_subprocess:exec_format(Cmd, [], [], Logger),
    Env;
run_hook(#operation{name=worker_call, args=[Method | WorkerType]}, Env) ->
    {Provider, Worker} = mzbl_script:resolve_worker_provider(WorkerType),
    lager:info("Run worker hook ~p:~p", [Worker, Method]),
    ok = Provider:load(Worker),
    case Provider:apply(Method, [Env], Worker) of 
        {ok,   NewEnv} -> NewEnv;
        % special case for languages without atom/keywork type
        {"ok", NewEnv} -> NewEnv;
        IncorrectReturn ->
            lager:info("Incorrect return value from worker hook ~p:~p", [Worker, Method]),
            erlang:error({incorrect_hook_return, Worker, Method, IncorrectReturn})
    end;
run_hook(Operation, _Env) ->
    erlang:error({unknown_hook_format, Operation}).

process_hooks_on_nodes(HookKind, Body, Env) ->
    Nodes = [node()|nodes()],

    [NewEnv | _] = mzb_lists:pmap(fun (Node) ->
        IsDirector = (node() == Node),
        case rpc:call(Node, mzb_script_hooks, process_hooks, [HookKind, Body, Env, IsDirector]) of
            {ok, NewEnv} -> NewEnv;
            Error ->
                lager:error("Unable to run ~p on node ~p~nError: ~p", [HookKind, Node, Error]),
                erlang:error({error, mzb_string:format("Unable to run ~p on node ~p", [HookKind, Node]), Error})
        end
    end, Nodes),

    {ok, NewEnv}.

process_hooks(HookKind, Body, Env, IsDirector) ->
    Hooks = [Args || #operation{name = Name, args = Args} <- Body, HookKind == Name],

    NewEnv = lists:foldl(fun ([Args], AccEnv) ->
                    [Target] = mzbl_ast:find_operation_and_extract_args(target, Args, [all]),
                    [Command] = mzbl_ast:find_operation_and_extract_args(command, Args),
                    case {Target, IsDirector} of
                        {director, true} ->
                            run_hook(Command, AccEnv);
                        {all, _} ->
                            run_hook(Command, AccEnv);
                        _ -> erlang:error({unknown_hook_target, Target})
                    end
                end, Env, Hooks),

    {ok, NewEnv}.


validate(#operation{args = [Args]}) ->
    Target = mzbl_ast:find_operation_and_extract_args(target, Args, [all]),
    Command = mzbl_ast:find_operation_and_extract_args(command, Args),
    validate_target(Target) ++ validate_command(Command).

validate_target([director]) -> [];
validate_target([all]) -> [];
validate_target(Target) ->
    [mzb_string:format("Invalid hook target: ~s (valid values are 'director' or 'all')", [Target])].

validate_command([#operation{name=exec, args=[Cmd]}]) when is_list(Cmd) -> [];
validate_command([#operation{name=worker_call, args=[_Method | WorkerType]}]) ->
    {Provider, Worker} = mzbl_script:resolve_worker_provider(WorkerType),
    Provider:validate(Worker);
validate_command(Command) ->
    [mzb_string:format("Invalid hook command ~p", [Command])].
