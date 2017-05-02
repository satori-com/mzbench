-module(mzb_api_endpoints).

-export([init/2, info/3, terminate/3, format_results/1]).

-include_lib("kernel/include/file.hrl").

-spec init(cowboy_req:req(), any()) -> {ok, cowboy_req:req(), term()}.
init(Req, _Opts) ->
    try
        lager:debug("REQUEST: ~p", [Req]),
        Path = cowboy_req:path(Req),
        Method = cowboy_req:method(Req),
        lager:info("[ ~s ] ~s", [Method, Path]),
        UserInfo = authorize(Method, Path, Req),
        handle(Method, Path, UserInfo, Req)
    catch
        error:{not_found, Reason} ->
            Req2 = reply_error(404, <<"not_found">>, Reason, Req),
            {ok, Req2, #{}};

        error:{not_supported, Reason} ->
            Req2 = reply_error(501, <<"not_supported">>, Reason, Req),
            {ok, Req2, #{}};

        error:not_running ->
            Req2 = reply_error(400, <<"not_running">>, "Benchmark is not running yet", Req),
            {ok, Req2, #{}};

        error:{badarg, Reason} ->
            Req2 = reply_error(400, <<"badarg">>, Reason, Req),
            {ok, Req2, #{}};

        error:server_inactive ->
            Description = "Server is going to shutdown",
            Req2 = reply_error(503, <<"service_unavailable">>, Description, Req),
            {ok, Req2, #{}};

        error:forbidden ->
            Description = "Operation is forbidden",
            Req2 = reply_error(403, <<"forbidden">>, Description, Req),
            {ok, Req2, #{}};

        _:E ->
            Description = io_lib:format("Server Internal Error: ~p~n~nReq: ~p~n~nStacktrace: ~p", [E, Req, erlang:get_stacktrace()]),
            Req2 = reply_error(500, <<"internal_error">>, Description, Req),
            lager:error(Description),
            {ok, Req2, #{}}
    end.

authorize(Method, Path, Req) ->
    Cookies = cowboy_req:parse_cookies(Req),
    #{id:= BenchId} = cowboy_req:match_qs([{id, int, 0}], Req),
    Ref = case proplists:get_value(mzb_api_auth:cookie_name(), Cookies, undefined) of
            undefined ->
                <<"Bearer ", Token/binary>> = cowboy_req:header(<<"authorization">>, Req, <<"Bearer ">>),
                {clitoken, Token};
            Cookie ->
                Token = cowboy_req:header(<<"csrf-stoken">>, Req, <<"">>),
                {cookie, Cookie, Token}
        end,
    mzb_api_auth:auth_api_call(Method, Path, Ref, BenchId).

handle(<<"GET">>, <<"/github_auth">>, _, Req) ->
    #{code:= Code, url:= URL} = cowboy_req:match_qs([{code, nonempty}, {url, nonempty}], Req),
    Req2 =
        case mzb_api_auth:auth_connection(undefined, "github", Code) of
            {ok, Ref, _UserInfo} ->
                cowboy_req:set_resp_cookie(mzb_api_auth:cookie_name(), Ref,
                    [{http_only, true}], Req);
            {error, Reason} ->
                lager:error("Authentication error: ~p", [Reason]),
                Req
        end,
    % we have to return redirect in any case because user is being redirected from github to ./github_auth
    % and we want the user to get back to the dashboard
    {ok, reply_redirect(303, URL, Req2), #{}};

handle(<<"POST">>, <<"/auth">>, _, Req) ->
    Type =
        try
            #{type:= T} = cowboy_req:match_qs([{type, nonempty}], Req),
            T
        catch
            error:bad_key ->
                erlang:error({badarg, "Missing type argument"})
        end,

    AuthList = maps:from_list(lists:map(
        fun ({AType, Opts}) ->
            {list_to_binary(AType), maps:from_list([{K, list_to_binary(V)} || {K, V} <- Opts])}
        end, mzb_api_auth:get_auth_methods())),

    case Type of
        <<"ref">> ->
            Cookies = cowboy_req:parse_cookies(Req),
            Ref = proplists:get_value(mzb_api_auth:cookie_name(), Cookies, undefined),
            case mzb_api_auth:auth_connection_by_ref(undefined, Ref) of
                {ok, #{login:= Login, login_type:= LoginType, name:= UserName, picture_url:= UserPic}, _} ->
                    {ok, reply_json(200,
                        #{res => <<"ok">>,
                          user_info => #{
                            login => list_to_binary(Login),
                            login_type => list_to_binary(LoginType),
                            name => list_to_binary(UserName),
                            picture_url => list_to_binary(UserPic)
                          }}, Req), #{}};
                {error, Reason} ->

                    Req2 = cowboy_req:set_resp_cookie(mzb_api_auth:cookie_name(), <<>>,
                            [{http_only, true}, {max_age, 0}], Req),
                    ReplyReason =
                        case Reason of
                            user_not_found -> <<"User not found">>;
                            _ -> <<"expired">>
                        end,
                    {ok, reply_json(200, #{res => <<"error">>, reason => ReplyReason, use => AuthList}, Req2), #{}}
            end;

        _ ->
            {ok, Code, Req2} = cowboy_req:body(Req),

            case mzb_api_auth:auth_connection(undefined, binary_to_list(Type), Code) of
                {ok, Ref, UserInfo} ->
                    Req3 = cowboy_req:set_resp_cookie(mzb_api_auth:cookie_name(), Ref,
                            [{http_only, true}], Req2),
                    {ok, reply_json(200,
                            #{res => <<"ok">>,
                              user_info => #{
                                login => list_to_binary(maps:get(login, UserInfo)),
                                login_type => list_to_binary(binary_to_list(Type)),
                                name => list_to_binary(maps:get(name, UserInfo)),
                                picture_url => list_to_binary(maps:get(picture_url, UserInfo))
                             }}, Req3), #{}};
                {error, Reason} ->
                    lager:error("Authentication error: ~p", [Reason]),
                    erlang:error(forbidden)
            end
    end;

handle(<<"POST">>, <<"/sign-out">>, _, Req) ->
    Cookies = cowboy_req:parse_cookies(Req),
    Ref = proplists:get_value(mzb_api_auth:cookie_name(), Cookies, undefined),
    mzb_api_auth:sign_out_connection(Ref),
    Req2 = cowboy_req:set_resp_cookie(mzb_api_auth:cookie_name(), <<>>,
                            [{http_only, true}, {max_age, 0}], Req),
    {ok, reply_json(200, #{}, Req2), #{}};

handle(<<"POST">>, <<"/typecheck">>, _, Req) ->
    case cowboy_req:parse_header(<<"content-type">>, Req) of
        {<<"multipart">>, <<"form-data">>, _} ->
            {Files, _Req2} = multipart(Req, []),
            [{_ScriptName, ScriptBody}] = proplists:get_all_values(<<"bench">>, Files),
            AST = mzbl_script:read_from_string(binary_to_list(ScriptBody)),
            case mzbl_typecheck:check(AST, list) of
                {false, Reason, Location} ->
                    reply_error(400, <<"error">>, Location ++ format_typecheck_reason(Reason), Req);
                _ -> reply_json(200, #{result => <<"ok">>}, Req)
            end;
        _ ->
            erlang:error({badarg, "Missing script file"})
    end;

handle(<<"POST">>, <<"/start">>, UserInfo, Req) ->
    Params = parse_start_params(Req),
    RequestedHost = cowboy_req:header(<<"host">>, Req, undefined),
    case cowboy_req:parse_header(<<"content-type">>, Req) of
        {<<"multipart">>, <<"form-data">>, _} ->
            {Files, Req2} = multipart(Req, []),
            [{ScriptName, ScriptBody}] = proplists:get_all_values(<<"bench">>, Files),
            Includes = proplists:get_all_values(<<"include">>, Files),
            Resp = mzb_api_server:start_bench(
                    Params#{script => #{name => ScriptName, body => ScriptBody},
                            includes => Includes,
                            req_host => RequestedHost,
                            author => maps:get(login, UserInfo),
                            author_name => maps:get(name, UserInfo)}),
            {ok, reply_json(200, Resp, Req2), #{}};
        _ ->
            erlang:error({badarg, "Missing script file"})
    end;

handle(<<"GET">>, <<"/restart">>, UserInfo, Req) ->
    with_bench_id(Req, fun (Id) ->
        Resp = mzb_api_server:restart_bench(Id, UserInfo),
        {ok, reply_json(200, Resp, Req), #{}}
    end);

handle(<<"GET">>, <<"/stop">>, _, Req) ->
    with_bench_id(Req, fun(Id) ->
        ok = mzb_api_server:stop_bench(Id),
        {ok, reply_json(200, #{status => <<"stopped">>}, Req), #{}}
    end);

handle(<<"GET">>, <<"/change_env">>, _, Req) ->
    with_bench_id(Req, fun (Id) ->
        NewEnv = cowboy_req:parse_qs(Req),
        ok = mzb_api_server:change_env(Id, proplists:delete(<<"id">>, NewEnv)),
        {ok, reply_json(200, #{status => <<"set">>}, Req), #{}}
    end);

handle(<<"GET">>, <<"/run_command">>, _, Req) ->
    with_bench_id(Req, fun (Id) ->
        try
            #{command:= Command, pool:= Pool, percent:= Percent} =
                cowboy_req:match_qs([{command, nonempty},
                    {pool, int}, {percent, int}], Req),
            ok = mzb_api_server:run_command(Id, Pool, Percent, Command)
        catch
            error:bad_key ->
                erlang:error({badarg, "Command, pool and percent are mandatory arguments"});
            error:{case_clause, _} ->
                erlang:error({badarg, "Provided percent or pool is not an int"})
        end,
        {ok, reply_json(200, #{status => <<"ok">>}, Req), #{}}
    end);

handle(<<"GET">>, <<"/status">>, _UserInfo, Req) ->
    with_bench_id(Req, fun(Id) ->
        {ok, reply_json(200, format_status(mzb_api_server:status(Id)), Req), #{}}
    end);

handle(<<"GET">>, <<"/results">>, _UserInfo, Req) ->
    with_bench_id(Req, fun(Id) ->
        {ok, reply_json(200, format_results(mzb_api_server:status(Id)), Req), #{}}
    end);

handle(<<"GET">>, <<"/log">>, _UserInfo, Req) ->
    with_bench_id(Req, fun(Id) ->
        #{config:= Config} = mzb_api_server:status(Id),
        #{log_compression:= Compression} = Config,
        Filename = mzb_api_bench:log_file(Config),
        {ok, stream_from_file(Filename, Compression, Id, Req), #{}}
    end);

handle(<<"GET">>, <<"/userlog">>, _UserInfo, Req) ->
    with_bench_id(Req, fun(Id) ->
        #{config:= Config} = mzb_api_server:status(Id),
        #{log_compression:= Compression} = Config,
        Filename = mzb_api_bench:log_user_file(Config),
        {ok, stream_from_file(Filename, Compression, Id, Req), #{}}
    end);

handle(<<"GET">>, <<"/data">>, _UserInfo, Req) ->
    with_bench_id(Req, fun(Id) ->
        #{config:= Config, metrics:= Metrics} =
            fun WaitMetricsCreations() ->
                #{status:= S} = Status = mzb_api_server:status(Id),
                case lists:member(S, [running, stopped, complete, crashed, zombie]) of
                    true -> Status;
                    false ->
                        timer:sleep(1000),
                        WaitMetricsCreations()
                end
            end (),
        MetricNames = mzb_api_metrics:extract_metric_names(Metrics),
        Filenames = [{N, mzb_api_bench:metrics_file(N, Config)} || N <- MetricNames],
        {ok, stream_metrics_from_files(Filenames, Id, Req), #{}}
    end);

handle(<<"GET">>, <<"/email_report">>, _UserInfo, Req) ->
    with_bench_id(Req, fun (Id) ->
        #{addr:= Addrs} = cowboy_req:match_qs([{addr, fun check_string_multi_param/1}], Req),
        ok = mzb_api_server:email_report(Id, Addrs),
        {ok, reply_json(200, #{}, Req), #{}}
    end);

handle(<<"GET">>, <<"/graphs">>, _UserInfo, Req) ->
    with_bench_id(Req, fun(Id) ->
        Location = list_to_binary(mzb_string:format("/#/bench/~p/overview", [Id])),
        Headers = [{<<"Location">>, Location}],
        {ok, cowboy_req:reply(302, Headers, <<>>, Req), #{}}
    end);

handle(<<"GET">>, <<"/clusters_info">>, _UserInfo, Req) ->
    List = mzb_api_cloud:clusters_info(),
    Keys = [id, state, n, bench_id, timestamp, provider, hosts, reason],
    F = fun (D) ->
            lists:map(
            fun ({state, S}) -> {state, erlang:atom_to_binary(S, latin1)};
                ({provider, P}) -> {provider, erlang:atom_to_binary(P, latin1)};
                ({hosts, Hosts}) -> {hosts, [erlang:list_to_binary(H) || H <- Hosts]};
                ({reason, Reason}) -> {reason, erlang:iolist_to_binary(io_lib:format("~p", [Reason]))};
                ({K, V}) -> {K, V}
            end, D)
        end,
    Info = [maps:from_list(F([{K,V} || {K,V} <- P, lists:member(K, Keys)])) || P <- List],
    Sorted = lists:usort(
        fun (#{timestamp:= T, id:= Id1}, #{timestamp:= T, id:=Id2}) -> Id1 =< Id2;
            (#{timestamp:= T1}, #{timestamp:= T2}) -> T1 =< T2
        end, Info),
    {ok, reply_json(200, Sorted, Req), #{}};

handle(<<"GET">>, <<"/deallocate_cluster">>, _UserInfo, Req) ->
    ClusterId =
        try
            #{id:= Id} = cowboy_req:match_qs([{id, int}], Req),
            Id
        catch
            error:bad_key ->
                erlang:error({badarg, "Missing id argument"});
            error:{case_clause, _} ->
                % case_clause exception is cowboy's way of saying that
                % provided id is not an int
                erlang:error({badarg, "Provided id is not an int"})
        end,

    try
        mzb_api_cloud:destroy_cluster(ClusterId),
        {ok, reply_json(200, #{}, Req), #{}}
    catch
        _:not_found -> erlang:error({not_found, "Cluster not found"});
        _:no_cluster -> erlang:error({not_found, "Cluster is not allocated"})
    end;

handle(<<"GET">>, <<"/remove_cluster_info">>, _UserInfo, Req) ->
    ClusterId =
        try
            #{id:= Id} = cowboy_req:match_qs([{id, int}], Req),
            Id
        catch
            error:bad_key ->
                erlang:error({badarg, "Missing id argument"});
            error:{case_clause, _} ->
                % case_clause exception is cowboy's way of saying that
                % provided id is not an int
                erlang:error({badarg, "Provided id is not an int"})
        end,

    try
        mzb_api_cloud:remove_cluster_info(ClusterId),
        {ok, reply_json(200, #{}, Req), #{}}
    catch
        _:not_found -> erlang:error({not_found, "Cluster not found"})
    end;

handle(<<"GET">>, <<"/add_tags">>, _UserInfo, Req) ->
    with_bench_id(Req, fun(Id) ->
        Tags =
            try
                #{tags:= TagsStr} = cowboy_req:match_qs([{tags, nonempty}], Req),
                parse_tags(TagsStr)
            catch
                error:bad_key ->
                    erlang:error({badarg, "Missing tags argument"})
            end,

        ok = mzb_api_server:add_tags(Id, Tags),
        {ok, reply_json(200, #{}, Req), #{}}
    end);

handle(<<"GET">>, <<"/remove_tags">>, _UserInfo, Req) ->
    with_bench_id(Req, fun(Id) ->
        Tags =
            try
                #{tags:= TagsStr} = cowboy_req:match_qs([{tags, nonempty}], Req),
                parse_tags(TagsStr)
            catch
                error:bad_key ->
                    erlang:error({badarg, "Missing tags argument"})
            end,

        ok = mzb_api_server:remove_tags(Id, Tags),
        {ok, reply_json(200, #{}, Req), #{}}
    end);

handle(Method, Path, UserInfo, Req) ->
    lager:error("Unknown request from ~p: ~p ~p~n~p", [maps:get(login, UserInfo), Method, Path, Req]),
    erlang:error({not_found, io_lib:format("Wrong endpoint: ~p ~p", [Method, Path])}).

with_bench_id(Req, Action) ->
    Id2 =
        try
            #{id:= Id} = cowboy_req:match_qs([{id, int}], Req),
            Id
        catch
            error:bad_key ->
                erlang:error({badarg, "Missing id argument"});
            error:{case_clause, _} ->
                % case_clause exception is cowboy's way of saying that
                % provided id is not an int
                erlang:error({badarg, "Provided id is not an int"})
        end,
    Action(Id2).

info({log, Msg}, Req, State) ->
    % this code is executed when you write something to log
    % so please don't log inside the function
    ok = cowboy_req:chunk([Msg], Req),
    {ok, Req, State}.

terminate(_Reason, _Req, #{lager_backend_id:= Id}) ->
    gen_event:delete_handler(lager_event, Id, []);
terminate(_Reason, _Req, _State) ->
    ok.

multipart(Req, Res) ->
    case cowboy_req:part(Req) of
        {ok, Headers, Req2} ->
            {ok, Body, Req3} = read_big_file(Req2),
            {file, Field, Filename, _CT, _Enc} = cow_multipart:form_data(Headers),
            multipart(Req3, [{Field, {binary_to_list(Filename), Body}}|Res]);
        {done, Req2} ->
            {Res, Req2}
    end.

read_big_file(Req) ->
    read_big_file(Req, <<>>).

read_big_file(Req, Acc) ->
    case cowboy_req:part_body(Req) of
        {ok, Body, Req2} -> {ok, <<Acc/binary, Body/binary>>, Req2};
        {more, Body, Req2} -> read_big_file(Req2, <<Acc/binary, Body/binary>>)
    end.

reply_json(Code, Map, Req) ->
    case Code of
        200 -> lager:info( "[ RESPONSE ] : ~p ~p", [Code, Map]);
        _   -> lager:error("[ RESPONSE ] : ~p ~p~n~p", [Code, Map, Req])
    end,
    cowboy_req:reply(Code, [{<<"content-type">>, <<"application/json">>}], jiffy:encode(Map), Req).

reply_redirect(Code, URI, Req) ->
    lager:info("[ REDIRECT ] ~p -> ~p", [Code, URI]),
    cowboy_req:reply(Code, [{<<"Location">>, iolist_to_binary(URI)}], <<"Authenticated">>, Req).

reply_error(HttpCode, Code, Description, Req) ->
    reply_json(HttpCode,
       #{
            reason_code => Code,
            reason => list_to_binary(Description)
        }, Req).

format_status(#{status:= failed, reason:= {crashed, _Reason}, config:= undefined}) ->
    #{status => failed, reason => crashed};
format_status(#{status:= Status, start_time:= StartTime, finish_time:= FinishTime}) ->
    Data = #{status => Status, start_time => list_to_binary(iso_8601_fmt(StartTime))},
    Data1 = case FinishTime of
        undefined -> Data;
        _ -> Data#{finish_time => list_to_binary(iso_8601_fmt(FinishTime))}
    end,
    Data1.

format_results(#{results:= undefined}) ->
    #{};
% BC code start
format_results(#{results:= [{_, _}|_] = Results}) ->
    maps:from_list([{list_to_binary(Name), #{type => undefined, value => Value}} || {Name, Value} <- Results, Value /= undefined]);
% BC code end
format_results(#{results:= Results}) ->
    Formated = lists:map(
        fun ({Name, counter, {undefined, Percentiles}}) ->
            {list_to_binary(Name), #{type => counter, rps => format_percentiles(Percentiles)}};
            ({Name, counter, {Val, Percentiles}}) ->
            {list_to_binary(Name), #{type => counter, value => Val, rps => format_percentiles(Percentiles)}};
            ({Name, Type, Percentiles}) ->
            {list_to_binary(Name), #{type => Type, percentiles => format_percentiles(Percentiles)}}
        end, Results),
    maps:from_list(Formated);
format_results(#{}) ->
    #{}.

format_percentiles(Percentiles) ->
    maps:from_list([{list_to_binary(Name), Value} || {Name, Value} <- Percentiles]).

check_string_multi_param(List) when is_list(List) -> {true, [binary_to_list(E)|| E <- List]};
check_string_multi_param(Bin) when is_binary(Bin) -> {true, [binary_to_list(Bin)]};
check_string_multi_param(_) -> false.

parse_update_interval(Bin) ->
    N = erlang:binary_to_integer(Bin),
    case N >= 1000 of
        true -> N;
        false -> erlang:error(invalid_update_interval)
    end.

check_nodes([Nodes]) ->
    List = binary_to_list(Nodes),
    try
        {true, list_to_integer(List)}
    catch
        _:_ -> {true, [binary_to_list(Nodes)]}
    end;
check_nodes(Nodes) when is_list(Nodes) -> {true, [binary_to_list(N) || N <- Nodes]}.


iso_8601_fmt(Seconds) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_universal_time({Seconds div 1000000, Seconds rem 1000000, 0}),
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
        [Year, Month, Day, Hour, Min, Sec]).

binary_to_bool(<<"true">>) -> true;
binary_to_bool(<<"false">>) -> false.

parse_start_params(Req) ->
    % List of parameters definition format:
    %   {ParamName,                 single_value,   binary_to_value conversion function,                        DefaultValue},
    %   {ParamName,                 list,           list_of_binaries_to_value conversion function,              DefaultValue}
    ParamsDefs = [
        {nodes,                     list,           fun(List) ->
                                                        {true, List2} = check_nodes(List),
                                                        List2
                                                    end,                                                        1},
        {email,                     list,           fun(List) ->
                                                        {true, List2} = check_string_multi_param(List),
                                                        List2
                                                    end,                                                        []},
        {node_git,                  single_value,   fun erlang:binary_to_list/1,                                undefined},
        {node_commit,               single_value,   fun erlang:binary_to_list/1,                                undefined},
        {emulate_bench_crash,       single_value,   fun binary_to_bool/1,                                       false},
        {deallocate_after_bench,    single_value,   fun binary_to_bool/1,                                       true},
        {provision_nodes,           single_value,   fun binary_to_bool/1,                                       true},
        {benchmark_name,            single_value,   fun erlang:binary_to_list/1,                                undefined},
        {cloud,                     single_value,   fun (N) -> erlang:binary_to_atom(N, latin1) end,            undefined},
        {exclusive,                 single_value,   fun erlang:binary_to_list/1,                                []},
        {vm_args,                   list,           fun (List) ->
                                                        {true, List2} = check_string_multi_param(List),
                                                        List2
                                                    end,                                                        []},
        {metric_update_interval_ms, single_value,   fun parse_update_interval/1,                                undefined},
        {tags,                      single_value,   fun parse_tags/1,                                           []},
        {parent,                    single_value,   fun erlang:binary_to_integer/1,                             undefined}
    ],

    {Params, Env} = lists:mapfoldl(
        fun (K, Acc) ->
            K1 = erlang:atom_to_binary(K, latin1),
            V = proplists:get_all_values(K1, Acc),
            {{K, V}, proplists:delete(K1, Acc)}
        end,
        cowboy_req:parse_qs(Req),
        [ParamName || {ParamName, _, _, _} <- ParamsDefs]),

    Params2 = lists:map(
        fun({ParamName, ValuesList}) ->
            {ParamName,
                case lists:keyfind(ParamName, 1, ParamsDefs) of
                    {_, single_value, BinaryToValueFun, DefaultValue} ->
                        case ValuesList of
                            [Value|_] ->
                                try
                                    BinaryToValueFun(Value)
                                catch
                                    _:_ ->
                                        erlang:error({badarg, io_lib:format("Invalid value \"~s\" for ~s", [Value, ParamName])})
                                end;
                            [] -> DefaultValue
                        end;

                    {_, list, ListOfBinariesToValueFun, DefaultValue} ->
                        case  ValuesList of
                            [] -> DefaultValue;
                            L ->
                                try
                                    ListOfBinariesToValueFun(L)
                                catch
                                    _:_ ->
                                        erlang:error({badarg, io_lib:format("Invalid value \"~p\" for ~s", [L, ParamName])})
                                end
                        end
                end
            }
        end,
        Params),

    Env2 = lists:usort(fun ({K1, _}, {K2, _}) -> K1 =< K2 end, Env),

    maps:from_list([{env, Env2}|Params2]).

stream_from_file(File, Compression, BenchId, Req) ->
    ContentEncoding =
        case Compression of
            none -> <<"identity">>;
            deflate -> <<"deflate">>
        end,
    Headers = [{<<"content-type">>, <<"text/plain">>},
               {<<"content-encoding">>, ContentEncoding}],
    IsFinished =
        fun () ->
            mzb_api_server:is_datastream_ended(BenchId)
        end,

    case IsFinished() of
        true ->
            {ok, #file_info{size = FileSize}} = file:read_file_info(File),
            F = fun (Socket, Transport) -> Transport:sendfile(Socket, File) end,
            Req2 = cowboy_req:set_resp_body_fun(FileSize, F, Req),
            cowboy_req:reply(200, Headers, Req2);
        false ->
            Req2 = cowboy_req:chunked_reply(200, Headers, Req),
            Streamer = fun (Bin) -> cowboy_req:chunk(Bin, Req2) end,
            ReadAtOnce = application:get_env(mzbench_api, bench_read_at_once, undefined),
            {ok, H} = file:open(File, [raw, read, binary, {read_ahead, ReadAtOnce}]),
            try
                PollTimeout = application:get_env(mzbench_api, bench_poll_timeout, undefined),
                stream_data_from_file(H, Streamer, IsFinished, PollTimeout),
                Req2
            after
                file:close(H)
            end
    end.

stream_data_from_file(H, Streamer, IsFinished, Timeout) ->
    IsLastTime = IsFinished(),
    case file:read(H, 1024) of
        {ok, D} ->
            Streamer(D),
            stream_data_from_file(H, Streamer, IsFinished, Timeout);

        eof ->
            case IsLastTime of
                true  ->
                    ok;
                false ->
                    timer:sleep(Timeout),
                    stream_data_from_file(H, Streamer, IsFinished, Timeout)
            end;

        {error, Reason} ->
            erlang:error({log_read_error, Reason})
    end.

stream_metrics_from_files(Files, BenchId, Req) ->
    Headers = [{<<"content-type">>, <<"text/plain">>},
               {<<"content-encoding">>, <<"identity">>}],
    IsFinished =
        fun () ->
            mzb_api_server:is_datastream_ended(BenchId)
        end,

    Req2 = cowboy_req:chunked_reply(200, Headers, Req),
    Streamer = fun (Bin) -> cowboy_req:chunk(Bin, Req2) end,
    ReadAtOnce = application:get_env(mzbench_api, bench_read_at_once, undefined),
    hd(mzb_lists:pmap(
        fun ({Name, File}) ->
            case file:open(File, [raw, read, binary, {read_ahead, ReadAtOnce}]) of
                {ok, H} ->
                    try
                        PollTimeout = application:get_env(mzbench_api, bench_poll_timeout, undefined),
                        fun R() ->
                            IsLastTime = IsFinished(),
                            case file:read_line(H) of
                                {ok, <<>>} ->
                                    R();
                                {ok, D} ->
                                    [Timestamp, Value] = binary:split(D, <<"\t">>),
                                    Streamer(<<Timestamp/binary, "\t", (erlang:list_to_binary(Name))/binary, "\t", Value/binary>>),
                                    R();
                                eof when IsLastTime ->
                                    ok;
                                eof ->
                                    timer:sleep(PollTimeout),
                                    R();
                                {error, Reason} ->
                                    erlang:error({metrics_read_error, Reason})
                            end
                        end (),
                        Req2
                    after
                        file:close(H)
                    end;
                {error, enoent} -> Req
            end
        end, Files)).

parse_tags(Binary) when is_binary(Binary) -> parse_tags(erlang:binary_to_list(Binary));
parse_tags(Str) -> string:tokens(Str, ", ").

format_typecheck_reason(R) when is_atom(R) ->
    atom_to_list(R);
format_typecheck_reason(T) when is_tuple(T) ->
    string:join(lists:map(fun format_typecheck_reason/1, tuple_to_list(T)), " ");
format_typecheck_reason(I) when is_integer(I) ->
    integer_to_list(I);
format_typecheck_reason(F) when is_float(F) ->
    float_to_list(F);
format_typecheck_reason(L) when is_list(L) -> L.
