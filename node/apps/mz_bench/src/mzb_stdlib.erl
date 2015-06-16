-module(mzb_stdlib).

-export([is_std_function/2,
         wait/4,
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
         dump/4
         ]).

-include("mzb_types.hrl").
-include("mzb_ast.hrl").

% ?MODULE:module_info(exports) is too expensive to call
% on every evaluation step, so we hardcode stdlib table here.
% Calling module_info once and storing the result would work too.
is_std_function(t, _) -> true;
is_std_function(loop, 2) -> true;
is_std_function(ignore_failure, 1) -> true;
is_std_function(wait, 1) -> true;
is_std_function(choose, 1) -> true;
is_std_function(choose, 2) -> true;
is_std_function(error, 1) -> true;
is_std_function(term_to_binary, 1) -> true;
is_std_function(random_binary, 1) -> true;
is_std_function(random_list, 1) -> true;
is_std_function(random_number, 1) -> true;
is_std_function(random_number, 2) -> true;
is_std_function(sprintf, 2) -> true;
is_std_function(var, 1) -> true;
is_std_function(numvar, 1) -> true;
is_std_function(seq, 2) -> true;
is_std_function(resource, 1) -> true;
is_std_function(round_robin, 1) -> true;
is_std_function(set_signal, 1) -> true;
is_std_function(set_signal, 2) -> true;
is_std_function(wait_signal, 1) -> true;
is_std_function(wait_signal, 2) -> true;
is_std_function(dump, 1) -> true;
is_std_function(_, _) -> false.

wait(State, _Env, _Meta, C) ->
    #constant{value = Time, units = ms} = mzb_literals:convert(C),
    timer:sleep(Time),
    {nil, State}.

choose(State, _Env, _Meta, N, List) ->
    {mzb_utility:choose(N, List), State}.

choose(State, _Env, _Meta, List) ->
    {mzb_utility:choose(List), State}.

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

set_signal(State, _Env, _Meta, Name) ->
    {mzb_signaler:add_signal(Name), State}.

set_signal(State, _Env, _Meta, Name, Count) ->
    {mzb_signaler:add_signal(Name, Count), State}.

dump(State, _Env, _Meta, Text) ->
    lager:info("~p", [Text]),
    {ok, State}.
