-module(mzb_multicloud_plugin).

-export([
    start/2,
    create_cluster/3,
    destroy_cluster/1
]).

%%%===================================================================
%%% API
%%%===================================================================

start(_Name, Opts) ->
    Opts.

create_cluster(#{clouds:= Clouds}, N, Config) ->
    Sum = lists:sum([W || {_, W} <- Clouds]),
    Parts = [{Name, round(N * W / Sum)} || {Name, W} <- tl(Clouds)],
    [{Name, _}|_] = Clouds,
    allocate([{Name, N - lists:sum([M || {_, M} <- Parts])}|Parts], Config).

allocate(Clouds, Config) ->
    BenchId = maps:get(bench_id, Config),
    CreateRes = mzb_lists:pmap(
        fun ({Name, N}) ->
            try
                mzb_api_cloud:create_cluster(BenchId, Name, N, Config)
            catch
                _:Error -> {error, {Name, N, Error}}
            end
        end, Clouds),

    Res = lists:all(
        fun ({ok, _, _, _}) -> true;
            ({error, {Name, N, Error}}) ->
                lager:error("Can't allocate ~p nodes in ~p because of ~p", [N, Name, Error]),
                false
        end, CreateRes),

    case Res of
        true  ->
            {Ids, User, Hosts} = lists:foldr(
                fun ({ok, Id, User, Hosts}, {IdAcc, _, HostsAcc}) ->
                    {[Id|IdAcc], User, Hosts ++ HostsAcc}
                end, {[], undefined, []}, CreateRes),
            {ok, Ids, User, Hosts};
        false ->
            lager:info("Unsuccessful multicluster creation: ~p~nResult: ~p", [Clouds, CreateRes]),
            _ = mzb_lists:pmap(
                fun ({error, _}) -> ok;
                    ({ok, Id, _, _}) ->
                    try
                        mzb_api_cloud:destroy_cluster(Id)
                    catch
                        _:_ -> ok
                    end
                end, CreateRes),

            erlang:error({multicluster_error, [E || {error, E} <- CreateRes]})
    end.

destroy_cluster(Ids) ->
    _ = mzb_lists:pmap(
        fun (Id) ->
            mzb_api_cloud:destroy_cluster(Id)
        end, Ids),
    ok.

