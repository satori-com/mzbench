-module(mz_counter).

-export([start_link/0,
         create/1,
         notify/2,
         get_value/1,
         create_raw/0,
         notify_raw/2,
         get_value_raw/1,
         reset/1]).

-behaviour(gen_server).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-on_load(init/0).

init() ->
    SoName = filename:join(
        case code:priv_dir(?MODULE) of
            {error, bad_name} ->
                Dir = code:which(?MODULE),
                filename:join([filename:dirname(Dir),"..","priv"]);
            Dir -> Dir
        end, atom_to_list(?MODULE) ++ "_nif"),
    erlang:load_nif(SoName, 0).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create(Name) ->
    gen_server:call(?MODULE, {create, Name}).

notify(Name, Value) ->
    case erlang:get({mz_counter_ref, Name}) of
        undefined ->
            Ref =
                try ets:lookup_element(?MODULE, Name, 2) of
                    R -> R
                catch
                    _:_ ->
                        ok = create(Name),
                        ets:lookup_element(?MODULE, Name, 2)
                end,
            erlang:put({mz_counter_ref, Name}, Ref),
            update_counter(Ref, Value);
        Ref ->
            update_counter(Ref, Value)
    end.

get_value(Name) ->
    Ref =
        case erlang:get({mz_counter_ref, Name}) of
            undefined ->
                case ets:lookup(?MODULE, Name) of
                    [{_, R}] ->
                        erlang:put({mz_counter_ref, Name}, R),
                        R;
                    [] -> erlang:error(not_found)
                end;
            R -> R
    end,
    get_counter_value(Ref).

reset(Name) ->
    case erlang:get({mz_counter_ref, Name}) of
        undefined ->
            Ref = ets:lookup_element(?MODULE, Name, 2),
            erlang:put({mz_counter_ref, Name}, Ref),
            reset_counter(Ref);
        Ref ->
            reset_counter(Ref)
    end.

create_raw() ->
    create_counter().

notify_raw(Ref, Value) ->
    update_counter(Ref, Value).

get_value_raw(Ref) ->
    get_counter_value(Ref).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    _ = ets:new(?MODULE,  [set, public, named_table, {read_concurrency, true}]),
    {ok, []}.

handle_call({create, Name}, _From, State) ->
    {ok, Ref} = create_counter(),
    _ = ets:insert_new(?MODULE, {Name, Ref}),
    {reply, ok, State};

handle_call(Req, _From, State) ->
    lager:error("Unhandled call: ~p", [Req]),
    {stop, {unhandled_call, Req}, State}.

handle_cast(Msg, State) ->
    lager:error("Unhandled cast: ~p", [Msg]),
    {stop, {unhandled_cast, Msg}, State}.

handle_info(Info, State) ->
    lager:error("Unhandled info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% internal
%%%===================================================================

create_counter() ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

update_counter(_Ref, _Value) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

reset_counter(_Ref) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

get_counter_value(_Ref) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).
