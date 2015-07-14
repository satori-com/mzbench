-module(mzb_lists).

-export(
   [
    choose/1,
    choose/2,
    pmap/2,
    enumerate/1
   ]).

choose([]) -> erlang:error(badarg);
choose(List) -> lists:nth(crypto:rand_uniform(1, length(List) + 1), List).

choose(N, List) -> taken(List, N).

taken(L, N) ->
     Len = length(L),
     PropL = lists:zip(lists:seq(1, Len), L),
     taken(PropL, N, Len, []).

taken([], _, _, L) -> L;
taken(_, 0, _, L) -> L;
taken([{_,V}], _, _, L) -> [V | L];
taken(Ps, N, Len, L) ->
    {K1, V1} = lists:nth(crypto:rand_uniform(1, Len), Ps),
    taken(proplists:delete(K1, Ps), N-1, Len-1, [V1 | L]).

pmap(Fun, List) ->
    Self = self(),
    Monitors = lists:map(fun (Element) ->
        Ref = erlang:make_ref(),
        {_, Mon} = erlang:spawn_monitor(fun () ->
            Res = try
                {Ref, {ok, Fun(Element)}}
            catch
                C:E -> {Ref, {exception, {C,E,erlang:get_stacktrace()}}}
            end,
            Self ! Res
        end),
        {Mon, Ref}
    end, List),
    pmap_results(Monitors, []).

pmap_results([], Res) -> lists:reverse(Res);
pmap_results([{Mon, Ref}|T], Res) ->
    receive
        {Ref, {ok, R}} ->
            erlang:demonitor(Mon, [flush]),
            pmap_results(T, [R|Res]);
        {Ref, {exception, {C,E,ST}}} ->
            erlang:raise(C, E, ST);
        {'DOWN', Mon, process, _, Reason} ->
            erlang:error({pmap_crash_child, Reason})
    end.

enumerate(List) when is_list(List) ->
    lists:zip(lists:seq(0, length(List) - 1), List).

