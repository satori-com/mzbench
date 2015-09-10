-module(mzb_bc).
% Backward compatibility module which implements functions required to run on Erlang 17.0

-export([
    maps_with/2,
    maps_without/2,
    maps_get/3
]).

-spec maps_get(Key, Map, Default) -> Value | Default when
        Key :: term(),
        Map :: map(),
        Value :: term(),
        Default :: term().

maps_get(Key,Map,Default) when is_map(Map) ->
    case maps:find(Key, Map) of
        {ok, Value} ->
            Value;
        error ->
            Default
    end;
maps_get(Key,Map,Default) ->
    erlang:error({badmap,Map},[Key,Map,Default]).


-spec maps_without(Ks,Map1) -> Map2 when
    Ks :: [K],
    Map1 :: map(),
    Map2 :: map(),
    K :: term().

maps_without(Ks,M) when is_list(Ks), is_map(M) ->
    maps:from_list([{K,V}||{K,V} <- maps:to_list(M), not lists:member(K, Ks)]);
maps_without(Ks,M) ->
    erlang:error(error_type(M),[Ks,M]).


-spec maps_with(Ks, Map1) -> Map2 when
    Ks :: [K],
    Map1 :: map(),
    Map2 :: map(),
    K :: term().

maps_with(Ks,M) when is_list(Ks), is_map(M) ->
    maps:from_list([{K,V}||{K,V} <- maps:to_list(M), lists:member(K, Ks)]);
maps_with(Ks,M) ->
    erlang:error(error_type(M),[Ks,M]).


error_type(M) when is_map(M) -> badarg;
error_type(V) -> {badmap, V}.