-module(mzb_api_auth).

-behaviour(gen_server).

%% API
-export([start_link/0, auth_connection/2, get_user_info/1, sign_out_connection/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(s, {}).

-define(REF_SIZE, 32).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

auth_connection("anonymous", _) ->
    case get_methods() of
        [] ->
            UserInfo =
                #{
                    login => "anonymous",
                    login_type => "undefined",
                    name => "anonymous",
                    picture_url => "",
                    email => ""
                },
            Ref = add_connection(UserInfo),
            {ok, Ref};
        List when is_list(List) ->
            AuthTypes = [{Type, mzb_bc:maps_get(client_id, Opts, undefined)} || {Type, Opts} <- List],
            {error, {forbidden, {use, maps:from_list(AuthTypes)}}}
    end;

auth_connection("google" = Type, Code) ->
    try
        Tokens = google_tokens(Code, get_opts(Type)),
        IdToken = maps:get(<<"id_token">>, Tokens),
        Info = google_token_info("id_token", IdToken, get_opts(Type)),
        case maps:find(<<"email">>, Info) of
            {ok, Email} ->
                Name = mzb_bc:maps_get(<<"name">>, Info, ""),
                Picture = mzb_bc:maps_get(<<"picture">>, Info, ""),
                UserInfo =
                    #{
                        login => binary_to_list(Email),
                        login_type => Type,
                        name => binary_to_list(Name),
                        picture_url => binary_to_list(Picture),
                        email => binary_to_list(Email)
                    },
                Ref = add_connection(UserInfo),
                {ok, Ref};
            error -> {error, "no_user_email"}
        end
    catch
        {google_api, Method, Error} ->
            {error, mzb_string:format("Google ~p return error ~p", [Method, Error])}
    end.

sign_out_connection(Ref) ->
    gen_server:call(?MODULE, {remove_connection, Ref}).

add_connection(UserInfo) ->
    Ref = generate_connection_ref(),
    ok = gen_server:call(?MODULE, {add_connection, Ref, UserInfo}),
    Ref.

get_user_info(Ref) ->
    case ets:lookup(auth_connections, Ref) of
        [{_, UserInfo}] -> {ok, UserInfo};
        [] -> {error, unknown_ref}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    auth_connections = ets:new(auth_connections, [protected, named_table]),
    {ok, #s{}}.

handle_call({add_connection, Ref, UserInfo}, _From, State) ->
    ets:insert_new(auth_connections, {Ref, UserInfo}),
    {reply, ok, State};

handle_call({remove_connection, Ref}, _From, State) ->
    ets:delete(auth_connections, Ref),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

generate_connection_ref() ->
    base64:encode(crypto:rand_bytes(?REF_SIZE)).

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
    case httpc:request(post, {URL, [], "application/x-www-form-urlencoded", Data}, [], []) of
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
    case httpc:request(post, {URL, [], "application/x-www-form-urlencoded", Data}, [], []) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}}->
            Reply = jiffy:decode(Body, [return_maps]),
            Reply;
        {ok, {{_Version, ErrorCode, _ReasonPhrase}, _Headers, Body}}->
            erlang:error({google_api, token_info, {ErrorCode, Body}});
        {error,Error}->
            erlang:error({google_api, token_info, Error})
    end.

get_methods() ->
    List = application:get_env(mzbench_api, user_authentication, []),
    [{Type, maps:from_list(Opts)} || {Type, Opts} <- List].

get_opts(Type) ->
    case proplists:get_value(Type, get_methods(), undefined) of
        undefined -> erlang:error({unknown_auth_method, Type});
        Opts -> Opts
    end.

