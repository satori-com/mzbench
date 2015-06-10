#!/usr/bin/env escript
%%! -kernel error_logger false

-mode(compile).

main([Script | Params]) ->
    Args = parse_args(Params, []),
    Env = proplists:get_all_values(env, Args),
    Pa = proplists:get_all_values(pa, Args),
    Validate = proplists:get_value(validate, Args, false),

    ScriptDir = filename:dirname(filename:absname(escript:script_name())),

    LocalCodePaths = lists:foldl(
                    fun (P, Acc) ->
                        filelib:wildcard(filename:join(ScriptDir, P)) ++ Acc
                    end, [],
                    ["../apps/*/ebin/",
                     "../../workers/*/ebin/",
                     "../deps/*/ebin/"]),

    RpmCodePaths = filelib:wildcard("/mz/mz_bench/lib/*/ebin/"),

    code:add_pathsa(RpmCodePaths ++ LocalCodePaths ++ Pa),

    case Validate of
        true -> validate(Script);
        _    -> run_script(Script, Env)
    end;

main(_) ->
    usage().

run_script(Script, Env) ->
    ok = application:start(inets),
    {ok, _} = net_kernel:start([nodename_gen(), shortnames]),
    {ok, _} = ensure_all_started(mz_bench),
    
    Env2 = [{"mzb_script_name", Script} | Env],

    Ref =
        case mzb_bench_sup:run_script(filename:absname(Script), Env2) of
            {ok, R} -> R;
            {error, _, _E, _ST, Messages} ->
                terminate_node(1, string:join(Messages, "\n"))
        end,

    case mzb_bench_sup:get_results(Ref) of
        {ok, Str} ->
            terminate_node(0, Str);
        {error, _, Str} ->
            terminate_node(1, Str)
    end.
parse_args([], Res) -> Res;
parse_args(["--validate"|T], Res) -> parse_args(T, [{validate, true}|Res]);
parse_args(["--env", KV | T], Res) ->
    {Key, [_Eq | Value]} = lists:splitwith(fun(A) -> A =/= $= end, KV),
    parse_args(T, [{env, {Key, Value}}|Res]);
parse_args(["--pa", P | T], Res) ->
    parse_args(T, [{pa, P}|Res]).

validate(Script) ->
    case mzb_script:read_and_validate(filename:absname(Script), []) of
        {ok, _, _} ->
            terminate_node(0, "ok");
        {error, _, _, _, Messages} ->
            terminate_node(1, string:join(Messages, "\n"))
    end.

nodename_gen() ->
    {N1,N2,N3} = erlang:now(),
    Str = lists:flatten(io_lib:format("~p-~p~p", [N1,N2,N3])),
    erlang:list_to_atom(Str).

usage() ->
    io:format("Usage: ~s ScriptName [--validate] [--env name=value...]~n", [escript:script_name()]).

%% backported from R17

ensure_all_started(Application) ->
    ensure_all_started(Application, temporary).

ensure_all_started(Application, Type) ->
    case ensure_all_started(Application, Type, []) of
        {ok, Started} ->
            {ok, lists:reverse(Started)};
        {error, Reason, Started} ->
            _ = [application:stop(App) || App <- Started],
            {error, Reason}
    end.

ensure_all_started(Application, Type, Started) ->
    case application:start(Application, Type) of
        ok ->
            {ok, [Application | Started]};
        {error, {already_started, Application}} ->
            {ok, Started};
        {error, {not_started, Dependency}} ->
            case ensure_all_started(Dependency, Type, Started) of
                {ok, NewStarted} ->
                    ensure_all_started(Application, Type, NewStarted);
                Error ->
                    Error
            end;
        {error, Reason} ->
            {error, {Application, Reason}, Started}
    end.

terminate_node(ExitCode, Message) ->
    application:stop(lager),
    case ExitCode == 0 of
        true  -> io:format("~s~n", [Message]);
        false -> io:format(standard_error, "~s~n", [Message])
    end,
    erlang:halt(ExitCode).
