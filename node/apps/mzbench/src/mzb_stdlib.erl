-module(mzb_stdlib).

-export([wait/4,
         choose/5,
         choose/4,
         error/4,
         term_to_binary/4,
         random_binary/4,
         random_list/4,
         random_number/4,
         random_number/5,
         sprintf/5,
         var/4,
         numvar/4,
         seq/5,
         resource/4,
         round_robin/4,
         set_signal/4,
         set_signal/5,
         wait_signal/4,
         wait_signal/5,
         wait_signal/6,
         dump/4
         ]).

-include_lib("mzbench_language/include/mzbl_types.hrl").

wait(State, _Env, _Meta, C) ->
    #constant{value = Time, units = ms} = mzbl_literals:convert(C),
    timer:sleep(Time),
    {nil, State}.

choose(State, _Env, _Meta, N, List) ->
    {mzb_lists:choose(N, List), State}.

choose(State, _Env, _Meta, List) ->
    {mzb_lists:choose(List), State}.

-spec error(any(), [proplists:property()], meta(), term()) -> no_return().
error(_State, _Env, _Meta, Reason) ->
    erlang:error(Reason).

-spec term_to_binary(any(), [proplists:property()], meta(), term()) -> {binary(), any()}.
term_to_binary(State, _Env, _Meta, Term) ->
    {erlang:term_to_binary(Term), State}.

random_binary(State, _Env, _Meta, N) ->
    {mzb_utility:random_binary(N), State}.

random_list(State, _Env, _Meta, N) ->
    {mzb_utility:random_list(N), State}.

random_number(State, _Env, _Meta, N) ->
    {mzb_utility:random_number(N), State}.

random_number(State, _Env, _Meta, N, M) ->
    {mzb_utility:random_number(N, M), State}.

sprintf(State, _Env, _Meta, Str, Vars) ->
    {lists:flatten(io_lib:format(Str, Vars)), State}.

numvar(State, Env, Meta, Name) ->
    {Value, NewState} = var(State, Env, Meta, Name),
    {mzb_utility:any_to_num(Value), NewState}.

var(State, Env, _Meta, Name) ->
    case proplists:is_defined(Name, Env) of
        false -> erlang:error({var_is_undefined, Name});
        true -> {proplists:get_value(Name, Env), State}
    end.

seq(State, _Env, _Meta, From, To) ->
    {lists:seq(From, To), State} .

resource(State, Env, _Meta, Resource) ->
    {proplists:get_value({resource, Resource}, Env, []), State}.

round_robin(_State, _Env, _Meta, []) ->
    erlang:error(empty_round_robin_list);
round_robin(State, _Env, Meta, List) ->
    Len = erlang:length(List),
    Id = proplists:get_value(worker_id, Meta),
    {lists:nth(Id rem Len + 1, List), State}.

wait_signal(State, _Env, _Meta, Name) ->
    {mzb_signaler:check_signal(Name), State}.

wait_signal(State, _Env, _Meta, Name, Count) ->
    {mzb_signaler:check_signal(Name, Count), State}.

wait_signal(State, _Env, _Meta, Name, Count, #constant{value = Timeout, units = ms}) ->
    {mzb_signaler:check_signal(Name, Count, Timeout), State}.

set_signal(State, _Env, _Meta, Name) ->
    {mzb_signaler:add_signal(Name), State}.

set_signal(State, _Env, _Meta, Name, Count) ->
    {mzb_signaler:add_signal(Name, Count), State}.

dump(State, _Env, _Meta, Text) ->
    lager:info("~p", [Text]),
    {ok, State}.
