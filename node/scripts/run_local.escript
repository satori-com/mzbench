#!/usr/bin/env escript
%%! -kernel error_logger false

-mode(compile).

main([Script | Params]) ->
    _ = os:cmd("epmd -daemon"),
    Args = parse_args(Params, []),

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

    BinDir = filename:dirname(escript:script_name()),
    RpmCodePaths = filelib:wildcard(filename:join(BinDir, "../lib/*/ebin/")),

    code:add_pathsa(RpmCodePaths ++ LocalCodePaths ++ Pa),

    Env = proplists:get_all_values(env, Args),
    Env1 = [{"mzb_script_name", Script} | Env],

    case Validate of
        true -> validate(Script, Env1);
        _    -> run_script(Script, Env1)
    end;

main(_) ->
    usage().

run_script(Script, Env) ->
    ok = application:start(inets),
    {ok, _} = net_kernel:start([nodename_gen(), shortnames]),

    setup_logger([{lager_console_backend, info}]),

    % setup is exometer dependency.
    ok = application:load(setup),
    ok = application:set_env(setup, data_dir, "."),
    ok = application:set_env(setup, log_dir, "."),
    {ok, _} = ensure_all_started(setup),

    {ok, _} = ensure_all_started(mzbench),

    Ref =
        case mzb_bench_sup:run_script(filename:absname(Script), Env) of
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

validate(Script, Env) ->
    setup_logger([]),

    ok = application:load(mzbench),

    case mzb_script_validator:read_and_validate(filename:absname(Script), Env) of
        {ok, _, _} ->
            terminate_node(0, "ok");
        {error, _, _, _, Messages} ->
            terminate_node(1, string:join(Messages, "\n"))
    end.

nodename_gen() ->
    {N1,N2,N3} = os:timestamp(),
    Str = lists:flatten(io_lib:format("~p-~p~p", [N1,N2,N3])),
    erlang:list_to_atom(Str).

usage() ->
    io:format("Usage: ~s ScriptName [--validate] [--env name=value...]~n", [escript:script_name()]).

setup_logger(Handlers) ->
    ok = application:load(lager),
    ok = application:set_env(lager, handlers, Handlers),
    ok = application:set_env(lager, crash_log, undefined),
    {ok, _} = ensure_all_started(lager),

    application:load(sasl),
    application:set_env(sasl, sasl_error_logger, {file, "/dev/null"}),
    {ok, _} = ensure_all_started(sasl).

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
