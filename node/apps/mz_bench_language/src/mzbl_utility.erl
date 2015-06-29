-module(mzbl_utility).

-export(
   [
    choose/1,
    choose/2,
    fold_interval/4,
    random_binary/1,
    random_list/1,
    random_number/1,
    random_number/2,
    to_integer_with_default/2,
    int_ceil/1,
    pmap/2,
    any_to_num/1,
    expand_filename/1,
    wildcard/1
   ]).

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

choose([]) -> erlang:error(badarg);
choose(List) -> lists:nth(crypto:rand_uniform(1, length(List) + 1), List).

choose(N, List) -> taken(List, N).

random_binary(N) -> crypto:rand_bytes(N).

random_list(N) -> erlang:binary_to_list(crypto:rand_bytes(N)).

random_number(N) -> crypto:rand_uniform(0, N).

random_number(N, M) -> crypto:rand_uniform(N, M).

fold_interval(_, Acc, Start, End) when Start > End -> Acc;
fold_interval(Fun, Acc, Start, End) ->
    fold_interval(Fun, Fun(Start, Acc), Start + 1, End).

to_integer_with_default(N, _) when is_integer(N) ->
    N;
to_integer_with_default(S, Default) when is_binary(S) ->
    try
        list_to_integer(binary_to_list(S))
    catch _ ->
        Default
    end;
to_integer_with_default(S, Default) when is_list(S) ->
    try
        list_to_integer(S)
    catch _ ->
        Default
    end;
to_integer_with_default(_, Default) ->
    Default.

int_ceil(X) ->
    T = trunc(X),
    case (X - T) of
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

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

any_to_num(Value) when is_integer(Value) or is_float(Value) -> Value;
any_to_num(Value) when is_binary(Value) ->
    any_to_num(binary_to_list(Value)); 
any_to_num(Value) when is_list(Value) -> 
    case string:to_float(Value) of
        {error,no_float} -> list_to_integer(Value);
        {F,_Rest} -> F
    end.

expand_filename("~/" ++ Filename) ->
    case init:get_argument(home) of
        {ok, [[HomeDir|_]|_]} ->
            filename:join(HomeDir, Filename);
        Error ->
            lager:error("Can't get home directory: ~p", [Error]),
            erlang:error({get_homedir_error, Error})
    end;
expand_filename(Filename) -> Filename.

wildcard(Wildcard) ->
    filelib:wildcard(expand_filename(Wildcard)).

