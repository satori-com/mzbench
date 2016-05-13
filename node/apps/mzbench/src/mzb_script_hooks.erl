-module(mzb_script_hooks).

-export([validate/1, pre_hooks/2, post_hooks/2, exec/1]).

-include_lib("mzbench_language/include/mzbl_types.hrl").

pre_hooks(Body, Env) -> process_hooks(pre_hook, Body, Env).

post_hooks(Body, Env) -> process_hooks(post_hook, Body, Env).

process_hooks(HookKind, Body, Env) ->
    Hooks = [Args || #operation{name = Name, args = Args} <- Body, HookKind == Name],

    NewEnv = lists:foldl(fun ([Operations], AccEnv) ->
                             lists:foldl(fun run_hook/2, AccEnv, Operations)
                         end, Env, Hooks),
    NewEnv.

validate(#operation{name = Name, args = [Args]}) when Name == pre_hook; Name == post_hook ->
    lists:flatmap(fun validate/1, Args);
validate(#operation{name=exec, args=[Target, Cmd]}) ->
    TargetErr = case Target of
        T when T == all; T == director -> [];
        _ -> [mzb_string:format("Invalid hook target: '~s' (valid values are 'director' or 'all')", [Target])]
    end,

    CmdErr = case Cmd of
        C when is_list(C) -> [];
        _ -> [mzb_string:format("Invalid exec command target: '~s' (should be string)", [Cmd])]
    end,

    TargetErr ++ CmdErr;
validate(#operation{name=worker_call, args=[_Method | WorkerType]}) ->
    {Provider, Worker} = mzbl_script:resolve_worker_provider(WorkerType),
    Provider:validate(Worker);
validate(Command) ->
    [mzb_string:format("Invalid hook command ~p", [Command])].

exec(Cmd) ->
    Logger = fun (S, F, A) -> lager:log(system_log_lager_event, S, self(), F, A) end,
    mzb_subprocess:exec_format(Cmd, [], [stderr_to_stdout], Logger),
    ok.

run_hook(#operation{name=exec, args=[Target, Cmd]}, Env) ->
    system_log:info("Run worker hook ~p", [Cmd]),

    Nodes = case Target of
        director -> [node()];
        all -> [node() | mzb_interconnect:nodes()]
    end,

    mzb_lists:pmap(fun (Node) ->
        case mzb_interconnect:call(Node, {mzb_script_hooks, exec, [Cmd]}) of
            ok -> ok;
            Error ->
                system_log:error("Unable to run exec hook ~p on node ~p~nError: ~p", [Cmd, Node, Error]),
                erlang:error({error, mzb_string:format("Unable to run exec hook ~p on node ~p", [Cmd, Node]), Error})
        end
    end, Nodes),
    Env;
run_hook(#operation{name=worker_call, args=[Method | WorkerType]}, Env) ->
    {Provider, Worker} = mzbl_script:resolve_worker_provider(WorkerType),
    system_log:info("Run worker hook ~p:~p", [Worker, Method]),
    ok = Provider:load(Worker),
    case Provider:apply(Method, [Env], Worker) of 
        {ok,   NewEnv} -> NewEnv;
        % special case for languages without atom/keywork type
        {"ok", NewEnv} -> NewEnv;
        IncorrectReturn ->
            system_log:info("Incorrect return value from worker hook ~p:~p", [Worker, Method]),
            erlang:error({incorrect_hook_return, Worker, Method, IncorrectReturn})
    end;
run_hook(Operation, _Env) ->
    erlang:error({unknown_hook_format, Operation}).
