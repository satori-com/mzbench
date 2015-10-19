-module(mzb_script_validator).

-export([validate/1, read_and_validate/2]).

-include_lib("mzbench_language/include/mzbl_types.hrl").

read_and_validate(ScriptFileName, Env) ->
    try
        {ok, WorkerDirs} = application:get_env(mzbench, workers_dirs),
        Nodes = [node()|nodes()],
        AutoEnv = [{"nodes_num", length(Nodes)},
                   {"bench_hosts", [mzbl_script:hostname(N) || N <- Nodes]},
                   {"bench_script_dir", filename:dirname(ScriptFileName)},
                   {"bench_workers_dir", WorkerDirs}],
        Body = mzbl_script:read(ScriptFileName),
        ok = validate(Body),
        {ok, Body, AutoEnv ++ Env}
    catch
        C:{read_file_error, File, E} = Error ->
            Message = mzb_string:format(
                "Failed to read file ~s: ~s",
                [File, file:format_error(E)]),
            {error, C, Error, erlang:get_stacktrace(), [Message]};
        C:{parse_error, {LineNumber, erl_parse, E}} = Error ->
            Message = mzb_string:format(
                "Failed to parse script ~s:~nline ~p: ~s",
                [ScriptFileName, LineNumber, [E]]),
            {error, C, Error, erlang:get_stacktrace(), [Message]};
        C:{invalid_operation_name, Name} = Error ->
            {error, C, Error, erlang:get_stacktrace(),
                [mzb_string:format("Script ~s is invalid:~nInvalid operation name ~p~n",
                    [ScriptFileName, Name])]};
        C:{error, {validation, VM}} = Error when is_list(VM) ->
            Messages = [mzb_string:format("Script ~s is invalid:~n", [ScriptFileName]) | VM],
            {error, C, Error, erlang:get_stacktrace(), Messages};
        C:Error ->
            ST = erlang:get_stacktrace(),
            Message = mzb_string:format(
                "Script ~s is invalid:~nError: ~p~n~nStacktrace for the curious: ~p",
                [ScriptFileName, Error, ST]),
            {error, C, Error, ST, [Message]}
    end.

validate(Script) ->
    Script2 = mzbl_script:enumerate_pools(Script),

    case mzbl_typecheck:check(Script2, list) of
        {false, Reason, undefined} ->
            erlang:error({error, {validation, [mzb_string:format("Type error ~p", [Reason])]}});
        {false, Reason, Location} ->
            erlang:error({error, {validation, [mzb_string:format("~sType error ~p", [Location, Reason])]}});
        _ -> []
    end,

    Errors = lists:foldl(
        fun (#operation{name = include_resource, args = [_Name, Path]}, Acc) ->
                validate_resource_filename(Path) ++ Acc;
            (#operation{name = include_resource, args = [_Name, Path, _Type]}, Acc) ->
                validate_resource_filename(Path) ++ Acc;
            (#operation{name = assert} = Op, Acc) ->
                mzb_asserts:validate(Op) ++ Acc;
            (#operation{name = make_install}, Acc) -> Acc;
            (#operation{name = pre_hook} = Op, Acc) ->
                mzb_script_hooks:validate(Op) ++ Acc;
            (#operation{name = post_hook} = Op, Acc) ->
                mzb_script_hooks:validate(Op) ++ Acc;
            (#operation{name = use_graphite, args = _}, Acc) -> 
                ["use_graphite is deprecated and shouldn't be used anymore."
                    ++ " Use \"graphite\" environment variable instead."] 
                ++ Acc;
            (#operation{name = pool} = Pool, Acc) -> validate_pool(Pool) ++ Acc;
            (#operation{name = F, args = A, meta = M}, Acc) ->
                [mzb_string:format("~sUnknown function: ~p/~p",
                    [mzbl_script:meta_to_location_string(M), F, erlang:length(A)])|Acc];
            (T, Acc) ->
                [mzb_string:format("Unexpected top-level term ~p", [T])|Acc]
        end, [], Script2),

    case Errors of
        [] -> ok;
        _ -> erlang:error({error, {validation, lists:reverse(Errors)}})
    end,

    mzb_signal_validation:validate(Script2),
    ok.

validate_resource_filename(#operation{name = 'var'}) -> [];
validate_resource_filename(Filename) ->
    case filename:split(Filename) of
        [_] -> [];
        [".", _] -> [];
        [S | _] ->
            case re:run(S, "^https?:", [{capture, first}, caseless]) of
                nomatch -> [mzb_string:format("Invalid resource filename: ~s", [Filename])];
                {match, _} -> []
            end
    end.

-spec validate_pool(#operation{}) -> [string()].
validate_pool(#operation{name = pool, args = [Opts, Script]} = Op) ->
    Name = proplists:get_value(pool_name, Op#operation.meta),
    {Provider, Worker} = mzbl_script:extract_worker(Opts),
    #operation{args = [Size]} = lists:keyfind(size, #operation.name, Opts),
    WorkerStartType =
        case lists:keyfind(worker_start, #operation.name, Opts) of
            #operation{args = [V]} -> V;
            false -> undefined
        end,
    lists:map(
      fun(Msg) -> Name ++ ": " ++ Msg end,
      case Provider:validate(Worker) of
          [] ->
              SizeErr = case Size of
                #operation{name = Var} when Var == var; Var == numvar -> [];
                #operation{name = N, args = A} ->
                    [mzb_string:format("can't use operation ~p with args ~p as pool size.", [N, A])];
                _ -> [mzb_string:format(
                          "size option: expected something integer-like but got ~p.", [Size])
                          || mzb_utility:to_integer_with_default(Size, fail) == fail] ++
                      ["zero size is not allowed." || mzb_utility:to_integer_with_default(Size, fail) == 0]
              end,
              ScriptErr = case mzb_worker_script_validator:validate_worker_script(Script, {Provider, Worker}) of
                  ok -> [];
                  {invalid_script, Errors} -> Errors
              end,
              StartTypeErr = validate_worker_start_type(WorkerStartType),
              SizeErr ++ ScriptErr ++ StartTypeErr;
          Messages -> Messages
      end).

validate_worker_start_type(undefined) -> [];
validate_worker_start_type(#operation{name = pow, args = [Y, W, #constant{value = N, units = ms}]})
    when is_number(N), N > 0, is_number(Y), Y > 0, is_number(W), W > 0 -> [];
validate_worker_start_type(#operation{name = pow}) ->
    [mzb_string:format("Invalid pow parameters (should be two positive numbers followed by time constant)", [])];
validate_worker_start_type(#operation{name = exp, args = [X, #constant{value = N, units = ms}]})
    when is_number(N), N > 0, is_number(X), X > 1 -> [];
validate_worker_start_type(#operation{name = exp}) ->
    [mzb_string:format("Invalid exp parameters (should be X > 1 followed by time constant)", [])];
validate_worker_start_type(#operation{name = poisson, args = [#constant{value = N, units = rps}]}) when is_number(N), N > 0 -> [];
validate_worker_start_type(#operation{name = poisson, args = [#constant{value = N, units = rps}]}) ->
    [mzb_string:format("Invalid poisson parameter lambda (should be positive number): ~p", [N])];
validate_worker_start_type(#operation{name = poisson, args = [N]}) ->
    [mzb_string:format("Invalid poisson parameter lambda (should be {<N>, rps}): ~p", [N])];
validate_worker_start_type(#operation{name = poisson, args = Args}) ->
    [mzb_string:format("Invalid poisson arguments (only one arg is allowed, ~p were given)", [erlang:length(Args)])];
validate_worker_start_type(#operation{name = linear, args = [#constant{value = N, units = rps}]}) when is_integer(N); N > 0 -> [];
validate_worker_start_type(#operation{name = linear, args = [#constant{value = N, units = rps}]}) ->
    [mzb_string:format("Invalid worker start rate (should be positive integer): ~b", [N])];
validate_worker_start_type(#operation{name = linear, args = [Arg]}) ->
    [mzb_string:format("Invalid worker start rate: (should be like {<N>, rps}, but '~p' was given)", [Arg])];
validate_worker_start_type(#operation{name = linear, args = Args}) ->
    [mzb_string:format("Invalid worker start rate arguments: only one arg is allowed, ~p were given", [erlang:length(Args)])];
validate_worker_start_type(Unknown) ->
    [mzb_string:format("Unknown worker start type: ~p", [Unknown])].
