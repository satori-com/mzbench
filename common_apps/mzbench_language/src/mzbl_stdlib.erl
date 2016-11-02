-module(mzbl_stdlib).

-export([wait/4,
         choose/5,
         choose/4,
         error/4,
         term_to_binary/4,
         random_binary/4,
         random_list/4,
         random_number/4,
         random_number/5,
         random_string/4,
         sprintf/5,
         var/4,
         var/5,
         numvar/4,
         numvar/5,
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

-include("mzbl_types.hrl").

-spec wait(State, Env, Meta, Time) -> {nil, State}
    when State :: any(),
         Env :: [proplists:property()],
         Meta :: meta(),
         Time :: #constant{}.
wait(State, _Env, _Meta, C) ->
    #constant{value = Time, units = ms} = mzbl_literals:convert(C),
    timer:sleep(Time),
    {nil, State}.

-spec choose(State, Env, Meta, N, List) -> {ChoosenList, State}
    when State :: any(),
         Env :: [proplists:property()],
         Meta :: meta(),
         N :: pos_integer(),
         List :: [any()],
         ChoosenList :: [any()].
choose(State, _Env, _Meta, N, List) ->
    {mzb_lists:choose(N, List), State}.

-spec choose(State, Env, Meta, List) -> {Choosen, State}
    when State :: any(),
         Env :: [proplists:property()],
         Meta :: meta(),
         List :: [any()],
         Choosen :: any().
choose(State, _Env, _Meta, List) ->
    {mzb_lists:choose(List), State}.

-spec error(any(), [proplists:property()], meta(), term()) -> no_return().
error(_State, _Env, _Meta, Reason) ->
    erlang:error(Reason).

-spec term_to_binary(any(), [proplists:property()], meta(), term()) -> {binary(), any()}.
term_to_binary(State, _Env, _Meta, Term) ->
    {erlang:term_to_binary(Term), State}.

-spec random_binary(State, Env, Meta, N) -> {Binary, State}
    when State :: any(),
         Env :: [proplists:property()],
         Meta :: meta(),
         N :: pos_integer(),
         Binary :: binary().
random_binary(State, _Env, _Meta, N) ->
    {mzb_utility:random_binary(N), State}.

-spec random_list(State, Env, Meta, N) -> {List, State}
    when State :: any(),
         Env :: [proplists:property()],
         Meta :: meta(),
         N :: pos_integer(),
         List :: [integer()].
random_list(State, _Env, _Meta, N) ->
    {mzb_utility:random_list(N), State}.

-spec random_string(State, Env, Meta, N) -> {String, State}
    when State :: any(),
         Env :: [proplists:property()],
         Meta :: meta(),
         N :: pos_integer(),
         String :: string().
random_string(State, _Env, _Meta, N) ->
    {mzb_utility:random_string(N), State}.

-spec random_number(State, Env, Meta, N) -> {Res, State}
    when State :: any(),
         Env :: [proplists:property()],
         Meta :: meta(),
         N :: pos_integer(),
         Res :: pos_integer().
random_number(State, _Env, _Meta, N) ->
    {mzb_utility:random_number(N), State}.

-spec random_number(State, Env, Meta, N, M) -> {Res, State}
    when State :: any(),
         Env :: [proplists:property()],
         Meta :: meta(),
         N :: pos_integer(),
         M :: pos_integer(),
         Res :: pos_integer().
random_number(State, _Env, _Meta, N, M) ->
    {mzb_utility:random_number(N, M), State}.

-spec sprintf(State, Env, Meta, Str, [Var]) -> {Res, State}
    when State :: any(),
         Env :: [proplists:property()],
         Meta :: meta(),
         Str :: string(),
         Var :: any(),
         Res :: string().
sprintf(State, _Env, _Meta, Str, Vars) ->
    {mzb_string:format(Str, Vars), State}.

-spec numvar(State, Env, Meta, Name) -> {Res, State}
    when State :: any(),
         Env :: [proplists:property()],
         Meta :: meta(),
         Name :: string(),
         Res :: number().
numvar(State, Env, _Meta, Name) ->
    {mzbl_ast:var_eval(numvar, Env, [Name]), State}.

-spec numvar(State, Env, Meta, Name, Default) -> {Res, State}
    when State :: any(),
         Env :: [proplists:property()],
         Meta :: meta(),
         Name :: string(),
         Default :: any(),
         Res :: number().
numvar(State, Env, _Meta, Name, Default) ->
    {mzbl_ast:var_eval(numvar, Env, [Name, Default]), State}.

-spec var(State, Env, Meta, Name) -> {Res, State}
    when State :: any(),
         Env :: [proplists:property()],
         Meta :: meta(),
         Name :: string(),
         Res :: any().
var(State, Env, _Meta, Name) ->
    {mzbl_ast:var_eval(var, Env, [Name]), State}.

-spec var(State, Env, Meta, Name, Default) -> {Res, State}
    when State :: any(),
         Env :: [proplists:property()],
         Meta :: meta(),
         Name :: string(),
         Default :: any(),
         Res :: number().
var(State, Env, _Meta, Name, Default) ->
    {mzbl_ast:var_eval(var, Env, [Name, Default]), State}.

-spec seq(State, Env, Meta, From, To) -> {Res, State}
    when State :: any(),
         Env :: [proplists:property()],
         Meta :: meta(),
         From :: integer(),
         To :: integer(),
         Res :: [number()].
seq(State, _Env, _Meta, From, To) ->
    {lists:seq(From, To), State} .

-spec resource(State, Env, Meta, Resource) -> {Res, State}
    when State :: any(),
         Env :: [proplists:property()],
         Meta :: meta(),
         Resource :: any(),
         Res :: [number()].
resource(State, Env, _Meta, Resource) ->
    {proplists:get_value({resource, Resource}, Env, []), State}.

-spec round_robin(State, Env, Meta, [El]) -> {El, State}
    when State :: any(),
         Env :: [proplists:property()],
         Meta :: meta(),
         El :: any().
round_robin(_State, _Env, _Meta, []) ->
    erlang:error(empty_round_robin_list);
round_robin(State, _Env, Meta, List) ->
    Len = erlang:length(List),
    Id = proplists:get_value(worker_id, Meta),
    {lists:nth((Id - 1) rem Len + 1, List), State}.

-spec wait_signal(State, Env, Meta, Name) -> {ok, State}
    when State :: any(),
         Env :: [proplists:property()],
         Meta :: meta(),
         Name :: any().
wait_signal(State, _Env, _Meta, Name) ->
    {mzb_signaler:check_signal(Name), State}.

-spec wait_signal(State, Env, Meta, Name, Count) -> {ok, State}
    when State :: any(),
         Env :: [proplists:property()],
         Meta :: meta(),
         Name :: any(),
         Count :: pos_integer().
wait_signal(State, _Env, _Meta, Name, Count) ->
    {mzb_signaler:check_signal(Name, Count), State}.

-spec wait_signal(State, Env, Meta, Name, Count, Timeout) -> {ok, State}
    when State :: any(),
         Env :: [proplists:property()],
         Meta :: meta(),
         Name :: any(),
         Count :: pos_integer(),
         Timeout :: #constant{}.
wait_signal(State, _Env, _Meta, Name, Count, #constant{value = Timeout, units = ms}) ->
    {mzb_signaler:check_signal(Name, Count, Timeout), State}.

-spec set_signal(State, Env, Meta, Name) -> {ok, State}
    when State :: any(),
         Env :: [proplists:property()],
         Meta :: meta(),
         Name :: any().
set_signal(State, _Env, _Meta, Name) ->
    {mzb_signaler:add_signal(Name), State}.

-spec set_signal(State, Env, Meta, Name, Count) -> {ok, State}
    when State :: any(),
         Env :: [proplists:property()],
         Meta :: meta(),
         Name :: any(),
         Count :: pos_integer().
set_signal(State, _Env, _Meta, Name, Count) ->
    {mzb_signaler:add_signal(Name, Count), State}.

-spec dump(State, Env, Meta, Text) -> {ok, State}
    when State :: any(),
         Env :: [proplists:property()],
         Meta :: meta(),
         Text :: term().
dump(State, _Env, _Meta, Text) ->
    lager:info("~p", [Text]),
    {ok, State}.

