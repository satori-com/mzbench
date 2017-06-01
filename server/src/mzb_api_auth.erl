-module(mzb_api_auth).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    auth_connection/3,
    auth_connection_by_ref/2,
    sign_out_connection/1,
    auth_api_call/4,
    generate_token/3,
    get_auth_methods/0,
    get_user_table/0 % for debug only
]).

-ifdef(TEST).
-export([check_admin_listed/1, check_black_white_listed/1]).
-endif.

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, cookie_name/0]).

-record(s, {start_id = undefined :: binary()}).

-define(REF_SIZE, 32).
-define(DEFAULT_EXPIRATION_TIME_S, 1814400). % 21 days
-define(VALIDATE_TOKENS_TIMEOUT_MS, 5000).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

cookie_name() -> <<"mzbench-token">>.

get_auth_methods() ->
    List = get_methods(),
    [{Type, [{id, mzb_bc:maps_get(client_id, Opts, "")}, {caption, mzb_bc:maps_get(caption, Opts, Type)}] ++
            [{url, URL} || URL <- [mzb_bc:maps_get(url, Opts, undefined)], URL /= undefined]} || {Type, Opts} <- List].

auth_connection(ConnectionPid, Type, Code) ->
    case auth(Type, Code) of
        {ok, UserInfo} ->
            Ref = add_connection(ConnectionPid, UserInfo),
            {ok, Ref, UserInfo};
        {error, Reason} ->
            {error, Reason}
    end.

auth("github" = Type, Code) ->
    try
        Tokens = github_api_token(Code, get_opts(Type)),
        AccessToken = maps:get(<<"access_token">>, Tokens),
        Info = github_api_user(AccessToken, get_opts(Type)),
        case maps:find(<<"login">>, Info) of
            {ok, Login} ->
                Name = case mzb_bc:maps_get(<<"name">>, Info, <<"">>) of
                    null -> <<"">>;
                    N -> N
                end,
                Email = case mzb_bc:maps_get(<<"email">>, Info, <<"">>) of
                    null -> <<"">>;
                    E -> E
                end,
                Picture = case mzb_bc:maps_get(<<"avatar_url">>, Info, <<"">>) of
                    null -> <<"">>;
                    P -> P
                end,
                UserInfo =
                    #{
                        login => binary_to_list(Login),
                        login_type => Type,
                        name => binary_to_list(Name),
                        picture_url => binary_to_list(Picture),
                        email => binary_to_list(Email)
                    },
                {ok, UserInfo};
            error -> {error, "no_user_email"}
        end
    catch
        {github_api, Method, Error} ->
            {error, mzb_string:format("GitHub ~p return error ~p", [Method, Error])}
    end;

auth("google" = Type, Code) ->
    try
        Tokens = google_tokens(Code, get_opts(Type)),
        IdToken = maps:get(<<"id_token">>, Tokens),
        Info = google_token_info("id_token", IdToken, get_opts(Type)),
        case maps:find(<<"email">>, Info) of
            {ok, Email} ->
                Name = mzb_bc:maps_get(<<"name">>, Info, <<"">>),
                Picture = mzb_bc:maps_get(<<"picture">>, Info, <<"">>),
                UserInfo =
                    #{
                        login => binary_to_list(Email),
                        login_type => Type,
                        name => binary_to_list(Name),
                        picture_url => binary_to_list(Picture),
                        email => binary_to_list(Email)
                    },
                {ok, UserInfo};
            error -> {error, "no_user_email"}
        end
    catch
        {google_api, Method, Error} ->
            {error, mzb_string:format("Google ~p return error ~p", [Method, Error])}
    end.

auth_connection_by_ref(ConnectionPid, Ref) ->
    gen_server:call(?MODULE, {auth_connection_by_ref, ConnectionPid, Ref}).

auth_api_call(<<"POST">>, <<"/auth">>, _Ref, _BenchId) -> #{};
auth_api_call(<<"GET">>, <<"/github_auth">>, _Ref, _BenchId) -> #{};
auth_api_call(_Method, Path, Ref, BenchId) ->
    case get_methods() of
        [] -> anon_info();
        _ -> auth_api_call_ref(Path, Ref, BenchId)
    end.

auth_api_call_ref(Path, {login, UserInfo}, BenchId) ->
    auth_login_access(Path, maps:get(login, UserInfo), BenchId),
    UserInfo;
auth_api_call_ref(Path, {clitoken, Token}, BenchId) ->
    case get_user_info(Token) of
        {ok, UserInfo} ->
            auth_login_access(Path, maps:get(login, UserInfo), BenchId),
            UserInfo;
        {error, unknown_ref} ->
            erlang:error(forbidden)
    end;
auth_api_call_ref(Path, {cookie, Cookie, CSRFToken}, BenchId) ->
    case dets:lookup(auth_tokens, Cookie) of
        [{_, #{user_info:= #{login := Login} = UserInfo, connection_pids:= Pids}}] ->
            case (not must_check_csrf_for(Path)) orelse lists:member(CSRFToken, maps:values(Pids)) of
                true ->
                    auth_login_access(Path, Login, BenchId),
                    UserInfo;
                false ->
                    erlang:error(forbidden)
            end;
        [] -> erlang:error(forbidden)
    end.

must_check_csrf_for(<<"/data">>) -> false;
must_check_csrf_for(<<"/log">>) -> false;
must_check_csrf_for(<<"/userlog">>) -> false;
must_check_csrf_for(_) -> true.

auth_login_access(Path, Login, BenchId) when
                    (Path == <<"/stop">>) or (Path == <<"/change_env">>) or
                    (Path == <<"/add_tag">>) or (Path == <<"/remove_tag">>) or
                    (Path == <<"/update_name">>) or (Path == <<"/run_command">>) ->
    case check_admin_listed(Login) of
        true -> ok;
        false -> _ = check_listed_or_fail(Login),
                #{config:= #{author := Author}} = mzb_api_server:status(BenchId),
                if Author == Login -> ok;
                    true ->  erlang:error(forbidden)
                end
    end;
auth_login_access(_, Login, _) -> check_listed_or_fail(Login).

check_listed_or_fail(Login) ->
    case check_black_white_listed(Login) of
        ok -> ok;
        error -> erlang:error(forbidden)
    end.

sign_out_connection(Ref) ->
    gen_server:call(?MODULE, {remove_connection, Ref}).

add_connection(ConnectionPid, UserInfo) ->
    Ref = generate_ref(),
    ok = gen_server:call(?MODULE, {add_connection, ConnectionPid, Ref, UserInfo}),
    Ref.

generate_token(Name, Lifetime, UserInfo) ->
    case gen_server:call(?MODULE, {generate_token, Name, Lifetime, UserInfo}) of
        {ok, Token} -> Token;
        {error, Reason} -> erlang:error(Reason)
    end.

get_user_table() ->
    dets:foldr(fun (E, Acc) -> [E|Acc] end, [], auth_tokens).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    DetsFile = filename:join(mzb_api_server:server_data_dir(), ".tokens.dets"),
    {ok, _} = dets:open_file(auth_tokens, [{file, DetsFile}, {type, set}]),
    erlang:send_after(?VALIDATE_TOKENS_TIMEOUT_MS, self(), validate),
    _ = inets:start(httpc, [{profile, auth_profile}]),
    _ = set_proxy(proxy, read_env_var("http_proxy"), read_env_var("no_proxy")),
    _ = set_proxy(https_proxy, read_env_var("https_proxy"), read_env_var("no_proxy")),
    {ok, #s{start_id = generate_ref()}}.

set_proxy(_Type, false, _) -> false;
set_proxy(Type, Value, NoProxy) ->
    NoProxyList =
        case NoProxy of
            false -> [];
            _ -> [parse_no_proxy(Str) || Str <- string:tokens(NoProxy, ",")]
        end,
    {ok, {_, _, Host, Port, _, _}} = http_uri:parse(Value),
    lager:info("Using ~p:~p as ~p for auth (exceptions: ~p)", [Host, Port, Type, NoProxyList]),
    httpc:set_options([{Type, {{Host, Port}, NoProxyList}}], auth_profile).

parse_no_proxy(Str) ->
    Str2 = string:strip(Str),
    case Str2 of
        "." ++ _ -> "*" ++ Str2;
       _ -> Str2
    end.

read_env_var(Var) ->
    case os:getenv(Var) of
        false -> os:getenv(string:to_upper(Var));
        Value -> Value
    end.

handle_call({add_connection, ConnectionPid, Ref, UserInfo}, _From, State = #s{start_id = StartId}) ->
    insert(ConnectionPid, Ref, UserInfo, StartId),
    {reply, ok, State};

handle_call({auth_connection_by_ref, ConnectionPid, Ref}, _From, State = #s{start_id = StartId}) ->
    case get_methods() of
        [] ->
            {reply, {ok, anon_info(), <<>>}, State};
        _ ->
            case get_user_info(Ref) of
                {ok, UserInfo} ->
                    case check_black_white_listed(maps:get(login, UserInfo)) of
                        ok ->
                            EditToken = insert(ConnectionPid, Ref, UserInfo, StartId),
                            {reply, {ok, UserInfo, EditToken}, State};
                        error -> {reply, {error, user_not_found}, State}
                    end;
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({remove_connection, Ref}, _From, State) ->
    dets:delete(auth_tokens, Ref),
    dets:sync(auth_tokens),
    {reply, ok, State};

handle_call({generate_token, Name, Lifetime, UserInfo}, _From, State = #s{start_id = StartId}) ->
    Token = generate_ref(),
    NewUserInfo =
        case Name of
            "" -> UserInfo;
            undefined -> UserInfo;
            _ -> UserInfo#{name => Name}
        end,
    insert(cli, Token, NewUserInfo, StartId, Lifetime),
    {reply, {ok, Token}, State};

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(validate, State = #s{start_id = StartId}) ->
    CurrentTime = mzb_api_bench:seconds(),
    ExpiredRefs = dets:foldl(
        fun ({Ref, #{expiratioin_ts:= Expiration}}, Acc) when (CurrentTime > Expiration) and (Expiration /= 0) -> [Ref|Acc];
            ({_, _}, Acc) -> Acc
        end, [], auth_tokens),
    _ = [remove(Ref, StartId) || Ref <- ExpiredRefs],
    erlang:send_after(?VALIDATE_TOKENS_TIMEOUT_MS, self(), validate),
    {noreply, State};

handle_info({'DOWN', _, process, Pid, _}, State) ->
    remove_connection(Pid, dets:first(auth_tokens)),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_user_info(Ref) ->
    case dets:lookup(auth_tokens, Ref) of
        [{_, #{user_info:= UserInfo}}] -> {ok, UserInfo};
        [] -> {error, unknown_ref}
    end.

remove(Ref, StartId) ->
    case dets:lookup(auth_tokens, Ref) of
        [] -> ok;
        [{_, #{connection_pids:= Pids, incarnation_id:= Id}}] ->
            [(Id == StartId) andalso mzb_api_ws_handler:reauth(Pid) || Pid <- maps:keys(Pids)],
            dets:delete(auth_tokens, Ref),
            dets:sync(auth_tokens)
    end.

insert(ConnectionPid, Ref, UserInfo, StartId) ->
    insert(ConnectionPid, Ref, UserInfo, StartId, ?DEFAULT_EXPIRATION_TIME_S).
insert(ConnectionPid, Ref, UserInfo, StartId, Lifetime) ->
    CreationTS = mzb_api_bench:seconds(),
    ExpirationTS = if Lifetime == 0 -> 0; true -> CreationTS + Lifetime end,
    AllMaps = [EC || {_, #{connection_pids:= EC,
                incarnation_id := Incarnation}} <- dets:lookup(auth_tokens, Ref),
                Incarnation == StartId],
    ExistingConnections = case AllMaps of
                            [] -> #{};
                            [Map | _] -> Map
                        end,
    NewConnections =
        case is_pid(ConnectionPid) of
            true ->
                erlang:monitor(process, ConnectionPid),
                maps:put(ConnectionPid, generate_ref(), ExistingConnections);
            false -> ExistingConnections
        end,
    dets:insert(auth_tokens, {Ref, #{user_info => UserInfo,
                                     connection_pids => NewConnections,
                                     creation_ts => CreationTS,
                                     expiratioin_ts => ExpirationTS,
                                     incarnation_id => StartId}}),
    dets:sync(auth_tokens),
    case is_pid(ConnectionPid) of
        true -> maps:get(ConnectionPid, NewConnections);
        false -> ok
    end.

generate_ref() ->
    base64:encode(crypto:strong_rand_bytes(?REF_SIZE)).

google_tokens(Code, Opts) ->
    URL = "https://www.googleapis.com/oauth2/v4/token",
    ClientID = maps:get(client_id, Opts),
    ClientSecret = maps:get(client_secret, Opts),
    RedirectURL = maps:get(redirect_url, Opts),
    Data = "code=" ++ edoc_lib:escape_uri(binary_to_list(Code)) ++
           "&client_id=" ++ edoc_lib:escape_uri(ClientID) ++
           "&client_secret=" ++ edoc_lib:escape_uri(ClientSecret) ++
           "&redirect_uri=" ++ edoc_lib:escape_uri(RedirectURL) ++
           "&grant_type=authorization_code",
    case httpc:request(post, {URL, [], "application/x-www-form-urlencoded", Data}, [], [], auth_profile) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}}->
            Reply = jiffy:decode(Body, [return_maps]),
            Reply;
        {ok, {{_Version, ErrorCode, _ReasonPhrase}, _Headers, Body}}->
            erlang:error({google_api, tokens, {ErrorCode, Body}});
        {error,Error}->
            erlang:error({google_api, tokens, Error})
    end.

google_token_info(Type, Token, _Opts) ->
    URL = "https://www.googleapis.com/oauth2/v3/tokeninfo",
    Data = mzb_string:format("~s=~s", [Type, Token]),
    case httpc:request(post, {URL, [], "application/x-www-form-urlencoded", Data}, [], [], auth_profile) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}}->
            Reply = jiffy:decode(Body, [return_maps]),
            Reply;
        {ok, {{_Version, ErrorCode, _ReasonPhrase}, _Headers, Body}}->
            erlang:error({google_api, token_info, {ErrorCode, Body}});
        {error,Error}->
            erlang:error({google_api, token_info, Error})
    end.

github_api_token(Code, Opts) ->
    GitHubUrl = mzb_bc:maps_get(url, Opts, "https://github.com"),
    ClientID = maps:get(client_id, Opts),
    ClientSecret = maps:get(client_secret, Opts),
    URL = GitHubUrl ++ "/login/oauth/access_token",
    Data = "client_id=" ++ edoc_lib:escape_uri(ClientID) ++
           "&client_secret=" ++ edoc_lib:escape_uri(ClientSecret) ++
           "&code=" ++ edoc_lib:escape_uri(binary_to_list(Code)),
    case httpc:request(post, {URL, [{"accept", "application/json"}], "application/x-www-form-urlencoded", Data}, [], [], auth_profile) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
            Reply = jiffy:decode(Body, [return_maps]),
            Reply;
        {ok, {{_Version, ErrorCode, _ReasonPhrase}, _Headers, Body}} ->
            erlang:error({github_api, access_token, {ErrorCode, Body}});
        {error,Error} ->
            erlang:error({github_api, access_token, Error})
    end.

github_api_user(Token, Opts) ->
    GitHubAPI = mzb_bc:maps_get(api_url, Opts, "https://api.github.com"),
    URL = GitHubAPI ++ "/user",

    case httpc:request(get, {URL, [{"authorization", "token " ++ binary_to_list(Token)}]}, [], [], auth_profile) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
            Reply = jiffy:decode(Body, [return_maps]),
            Reply;
        {ok, {{_Version, ErrorCode, _ReasonPhrase}, _Headers, Body}} ->
            erlang:error({github_api, token_info, {ErrorCode, Body}});
        {error,Error} ->
            erlang:error({github_api, token_info, Error})
    end.

get_methods() ->
    List = application:get_env(mzbench_api, user_authentication, []),
    [{Type, maps:from_list(Opts)} || {Type, Opts} <- List].

get_opts(Type) ->
    case proplists:get_value(Type, get_methods(), undefined) of
        undefined -> erlang:error({unknown_auth_method, Type});
        Opts -> Opts
    end.

check_admin_listed(Login) ->
    list_regexp_check(Login, application:get_env(mzbench_api, admin_list, [])).

check_black_white_listed(Login) ->
    BlackList = application:get_env(mzbench_api, black_list, []),
    WhiteList = application:get_env(mzbench_api, white_list, []),
    case list_regexp_check(Login, BlackList) of
        true -> error;
        false -> if WhiteList == [] -> ok;
                    true ->
                        case list_regexp_check(Login, WhiteList) of
                            true -> ok;
                            false -> error
                        end
                 end
    end.

list_regexp_check(Name, List) ->
    lists:any(fun(X) -> case re:run(Name, mzb_string:wildcard_to_regexp(X)) of
        nomatch -> false;
        _ -> true end end, List).

remove_connection(_, '$end_of_table') -> ok;
remove_connection(Pid, NextId) ->
    case dets:lookup(auth_tokens, NextId) of
        [] -> remove_connection(Pid, dets:next(auth_tokens, NextId));
        [{_, #{connection_pids:= Connections} = Info}] ->
            case maps:is_key(Pid, Connections) of
                true  ->
                    NewInfo = maps:put(connection_pids, maps:remove(Pid, Connections), Info),
                    dets:insert(auth_tokens, {NextId, NewInfo}),
                    dets:sync(auth_tokens);
                false ->
                    remove_connection(Pid, dets:next(auth_tokens, NextId))
            end
    end.

anon_info() ->
    #{
        login => "anonymous",
        login_type => "undefined",
        name => "anonymous",
        picture_url => "",
        email => ""
    }.
