-module(mzb_script_validator).

-export([validate/1, read_and_validate/2]).

-include_lib("mz_bench_language/include/mzbl_types.hrl").

read_and_validate(ScriptFileName, Env) ->
    try
        {ok, WorkerDirs} = application:get_env(mz_bench, workers_dirs),
        Nodes = [node()|nodes()],
        AutoEnv = [{"nodes_num", length(Nodes)},
                   {"bench_hosts", [mzbl_script:hostname(N) || N <- Nodes]},
                   {"bench_script_dir", filename:dirname(ScriptFileName)},
                   {"bench_workers_dir", WorkerDirs}],
        Body = mzbl_script:read(ScriptFileName, AutoEnv ++ Env),
        ok = validate(Body),
        {ok, Body, AutoEnv ++ Env}
    catch
        C:{read_file_error, File, E} = Error ->
            Message = io_lib:format(
                "Failed to read file ~s: ~s",
                [File, file:format_error(E)]),
            {error, C, Error, erlang:get_stacktrace(), [lists:flatten(Message)]};
        C:{parse_error, {LineNumber, erl_parse, E}} = Error ->
            Message = io_lib:format(
                "Failed to parse script ~s:~nline ~p: ~s",
                [ScriptFileName, LineNumber, [E]]),
            {error, C, Error, erlang:get_stacktrace(), [lists:flatten(Message)]};
        C:{error, {validation, VM}} = Error when is_list(VM) ->
            Messages = [lists:flatten(io_lib:format("Script ~s is invalid:~n", [ScriptFileName])) 
                        | VM],
            {error, C, Error, erlang:get_stacktrace(), Messages};
        C:Error ->
            ST = erlang:get_stacktrace(),
            Message = io_lib:format(
                "Script ~s is invalid:~nError: ~p~n~nStacktrace for the curious: ~p",
                [ScriptFileName, Error, ST]),
            {error, C, Error, ST, [lists:flatten(Message)]}
    end.

validate(Script) ->
    Script2 = mzbl_script:enumerate_pools(Script),
    Errors = lists:foldl(
        fun (#operation{name = include_resource, args = [_Name, Path]}, Acc) ->
                validate_resource_filename(Path) ++ Acc;
            (#operation{name = include_resource, args = [_Name, Path, _Type]}, Acc) ->
                validate_resource_filename(Path) ++ Acc;
            (#operation{name = assert} = Op, Acc) ->
                mzb_asserts:validate(Op) ++ Acc;
            (#operation{name = make_install}, Acc) -> Acc;
            (#operation{name = use_graphite, args = _}, Acc) -> 
                ["use_graphite is deprecated and shouldn't be used anymore."
                    ++ " Use \"graphite\" environment variable instead."] 
                ++ Acc;
            (#operation{name = pool} = Pool, Acc) -> validate_pool(Pool) ++ Acc;
            (#operation{name = F, args = A, meta = M}, Acc) ->
                [lists:flatten(io_lib:format("~sUnknown function: ~p/~p",
                    [mzbl_script:meta_to_location_string(M), F, erlang:length(A)]))|Acc];
            (T, Acc) ->
                [lists:flatten(io_lib:format("Unexpected top-level term ~p",
                    [T]))|Acc]
        end, [], Script2),

    case Errors of
        [] -> ok;
        _ -> erlang:error({error, {validation, lists:reverse(Errors)}})
    end,

    mzb_signal_validation:validate(Script2),
    ok.

validate_resource_filename(Filename) ->
    case filename:split(Filename) of
        [_] -> [];
        [".", _] -> [];
        _ -> [lists:flatten(io_lib:format("Invalid resource filename: ~s", [Filename]))]
    end.

-spec validate_pool(#operation{}) -> [string()].
validate_pool(#operation{name = pool, args = [Opts, Script]} = Op) ->
    Name = proplists:get_value(pool_name, Op#operation.meta),
    {Provider, Worker} = mzbl_script:extract_worker(Opts),
    [Size] = mzbl_ast:find_operation_and_extract_args(size, Opts),
    [WorkerStartType] = mzbl_ast:find_operation_and_extract_args(worker_start, Opts, [undefined]),
    lists:map(
      fun(Msg) -> Name ++ ": " ++ Msg end,
      case Provider:validate(Worker) of
          [] ->
              case Size of
                #operation{name = N, args = A} ->
                    lists:flatten(io_lib:format(
                    "can't use operation ~p with args ~p as pool size.", [N, A]));
                _ -> [lists:flatten(io_lib:format(
                          "size option: expected something integer-like but got ~p.", [Size]))
                          || mzbl_utility:to_integer_with_default(Size, fail) == fail] ++
                      ["zero size is not allowed." || mzbl_utility:to_integer_with_default(Size, fail) == 0]
              end ++
              case mzb_worker_script_validator:validate_worker_script(Script, {Provider, Worker}) of
                  ok -> [];
                  {invalid_script, Errors} -> Errors
              end ++
              validate_worker_start_type(WorkerStartType);
          Messages -> Messages
      end).

validate_worker_start_type(undefined) -> [];
validate_worker_start_type(#operation{name = poisson, args = [#constant{value = N, units = rps}]}) when is_number(N), N > 0 -> [];
validate_worker_start_type(#operation{name = poisson, args = [#constant{value = N, units = rps}]}) ->
    [lists:flatten(io_lib:format("Invalid poisson parameter lambda (should be positive number): ~p", [N]))];
validate_worker_start_type(#operation{name = poisson, args = [N]}) ->
    [lists:flatten(io_lib:format("Invalid poisson parameter lambda (should be {<N>, rps}): ~p", [N]))];
validate_worker_start_type(#operation{name = poisson, args = Args}) ->
    [lists:flatten(io_lib:format("Invalid poisson arguments (only one arg is allowed, ~p were given)", [erlang:length(Args)]))];
validate_worker_start_type(#operation{name = linear, args = [#constant{value = N, units = rps}]}) when is_integer(N); N > 0 -> [];
validate_worker_start_type(#operation{name = linear, args = [#constant{value = N, units = rps}]}) ->
    [lists:flatten(io_lib:format("Invalid worker start rate (should be positive integer): ~b", [N]))];
validate_worker_start_type(#operation{name = linear, args = [Arg]}) ->
    [lists:flatten(io_lib:format("Invalid worker start rate: (should be like {<N>, rps}, but '~p' was given)", [Arg]))];
validate_worker_start_type(#operation{name = linear, args = Args}) ->
    [lists:flatten(io_lib:format("Invalid worker start rate arguments: only one arg is allowed, ~p were given", [erlang:length(Args)]))];
validate_worker_start_type(Unknown) ->
    [lists:flatten(io_lib:format("Unknown worker start type: ~p", [Unknown]))].
