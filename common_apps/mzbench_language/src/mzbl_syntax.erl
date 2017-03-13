%%% This file is auto-generated from mzbl_syntax.peg, run make rebuild-parser if you wish to change it, do not edit directly

-module(mzbl_syntax).
-export([parse/1,file/1]).
-define(p_anything,true).
-define(p_assert,true).
-define(p_charclass,true).
-define(p_choose,true).
-define(p_label,true).
-define(p_not,true).
-define(p_one_or_more,true).
-define(p_optional,true).
-define(p_scan,true).
-define(p_seq,true).
-define(p_string,true).
-define(p_zero_or_more,true).



-spec file(file:name()) -> any().
file(Filename) -> case file:read_file(Filename) of {ok,Bin} -> parse(Bin); Err -> Err end.

-spec parse(binary() | list()) -> any().
parse(List) when is_list(List) -> parse(unicode:characters_to_binary(List));
parse(Input) when is_binary(Input) ->
  _ = setup_memo(),
  Result = case 'scenario'(Input,{{line,1},{column,1}}) of
             {AST, <<>>, _Index} -> AST;
             Any -> Any
           end,
  release_memo(), Result.

-spec 'scenario'(input(), index()) -> parse_result().
'scenario'(Input, Index) ->
  p(Input, Index, 'scenario', fun(I,D) -> (p_seq([fun '__'/2, p_zero_or_more(fun 'operation'/2), p_not(p_anything())]))(I,D) end, fun(Node, _Idx) ->{scenario, lists:nth(2, Node)} end).

-spec 'operation'(input(), index()) -> parse_result().
'operation'(Input, Index) ->
  p(Input, Index, 'operation', fun(I,D) -> (p_seq([p_choose([fun 'multiline'/2, fun 'single'/2]), fun '__'/2]))(I,D) end, fun(Node, _Idx) ->lists:nth(1, Node) end).

-spec 'multiline'(input(), index()) -> parse_result().
'multiline'(Input, Index) ->
  p(Input, Index, 'multiline', fun(I,D) -> (p_seq([fun 'id'/2, fun '__'/2, p_string(<<"(">>), p_optional(fun 'args'/2), p_string(<<")">>), fun '__'/2, p_string(<<":">>), fun '__'/2, fun 'indent'/2, fun '__'/2, p_one_or_more(fun 'operation'/2), fun '__'/2, fun 'dedent'/2, fun '__'/2]))(I,D) end, fun(Node, Idx) ->case length(lists:nth(4, Node)) of
  0 -> {call, lists:nth(1, Node), [lists:nth(11, Node)], Idx};
  _ -> {call, lists:nth(1, Node), [lists:nth(1, lists:nth(4, Node)), lists:nth(11, Node)], Idx}
end end).

-spec 'single'(input(), index()) -> parse_result().
'single'(Input, Index) ->
  p(Input, Index, 'single', fun(I,D) -> (p_seq([fun 'id'/2, fun '__'/2, p_string(<<"(">>), p_optional(fun 'args'/2), p_string(<<")">>)]))(I,D) end, fun(Node, Idx) ->{call, lists:nth(1, Node), lists:nth(4, Node), Idx} end).

-spec 'args'(input(), index()) -> parse_result().
'args'(Input, Index) ->
  p(Input, Index, 'args', fun(I,D) -> (p_choose([fun 'kargs'/2, fun 'pargs'/2]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'kargs'(input(), index()) -> parse_result().
'kargs'(Input, Index) ->
  p(Input, Index, 'kargs', fun(I,D) -> (p_seq([fun '__'/2, p_label('head', fun 'pair'/2), p_label('tail', p_zero_or_more(p_seq([fun '__'/2, p_string(<<",">>), fun '__'/2, fun 'pair'/2]))), fun '__'/2]))(I,D) end, fun(Node, _Idx) ->
    Head = proplists:get_value(head, Node),
    Rest = [lists:nth(4, I) || I <- proplists:get_value(tail, Node)],
    [[Head|Rest]]
 end).

-spec 'pargs'(input(), index()) -> parse_result().
'pargs'(Input, Index) ->
  p(Input, Index, 'pargs', fun(I,D) -> (p_seq([fun '__'/2, p_label('head', fun 'value'/2), p_label('tail', p_zero_or_more(p_seq([fun '__'/2, p_string(<<",">>), fun '__'/2, fun 'value'/2]))), fun '__'/2]))(I,D) end, fun(Node, _Idx) ->
    Head = proplists:get_value(head, Node),
    Rest = [lists:nth(4, I) || I <- proplists:get_value(tail, Node)],
    [Head|Rest]
 end).

-spec 'pair'(input(), index()) -> parse_result().
'pair'(Input, Index) ->
  p(Input, Index, 'pair', fun(I,D) -> (p_seq([fun '__'/2, p_label('key', p_choose([fun 'id'/2, fun 'string'/2])), fun '__'/2, p_string(<<"=">>), fun '__'/2, p_label('value', fun 'value'/2), fun '__'/2]))(I,D) end, fun(Node, Idx) ->{call, t, [proplists:get_value(key, Node), proplists:get_value(value, Node)], Idx} end).

-spec 'value'(input(), index()) -> parse_result().
'value'(Input, Index) ->
  p(Input, Index, 'value', fun(I,D) -> (p_choose([fun 'unumber'/2, fun 'logic_exp'/2, fun 'single'/2, fun 'list'/2, fun 'id'/2, fun 'number'/2, fun 'string'/2]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'logic_exp'(input(), index()) -> parse_result().
'logic_exp'(Input, Index) ->
  p(Input, Index, 'logic_exp', fun(I,D) -> (p_choose([fun 'logic_priority'/2, fun 'logic_unary'/2, fun 'logic_plain'/2]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'logic_priority'(input(), index()) -> parse_result().
'logic_priority'(Input, Index) ->
  p(Input, Index, 'logic_priority', fun(I,D) -> (p_seq([p_string(<<"(">>), fun '__'/2, p_label('head', fun 'logic_exp'/2), p_string(<<")">>), p_label('tail', p_zero_or_more(p_seq([fun '__'/2, fun 'logic_binary'/2, fun '__'/2, fun 'logic_exp'/2])))]))(I,D) end, fun(Node, Idx) ->
    lists:foldl(fun(Logic, E) -> {call, lists:nth(2, Logic), [E, lists:nth(4, Logic)], Idx} end,
      proplists:get_value(head, Node), proplists:get_value(tail, Node))
 end).

-spec 'logic_plain'(input(), index()) -> parse_result().
'logic_plain'(Input, Index) ->
  p(Input, Index, 'logic_plain', fun(I,D) -> (p_seq([p_label('head', fun 'logic_op'/2), p_label('tail', p_zero_or_more(p_seq([fun '__'/2, fun 'logic_binary'/2, fun '__'/2, fun 'logic_exp'/2])))]))(I,D) end, fun(Node, Idx) ->
    lists:foldl(fun(Logic, E) -> {call, lists:nth(2, Logic), [E, lists:nth(4, Logic)], Idx} end,
      proplists:get_value(head, Node), proplists:get_value(tail, Node))
 end).

-spec 'logic_binary'(input(), index()) -> parse_result().
'logic_binary'(Input, Index) ->
  p(Input, Index, 'logic_binary', fun(I,D) -> (p_choose([p_string(<<"and">>), p_string(<<"or">>)]))(I,D) end, fun(Node, _Idx) ->
case Node of
  <<"and">> -> 'and';
  <<"or">> -> 'or'
end
 end).

-spec 'logic_unary'(input(), index()) -> parse_result().
'logic_unary'(Input, Index) ->
  p(Input, Index, 'logic_unary', fun(I,D) -> (p_seq([p_string(<<"not">>), fun '__'/2, fun 'logic_exp'/2]))(I,D) end, fun(Node, Idx) ->{call, 'not', [lists:nth(3, Node)], Idx} end).

-spec 'logic_op'(input(), index()) -> parse_result().
'logic_op'(Input, Index) ->
  p(Input, Index, 'logic_op', fun(I,D) -> (p_seq([p_choose([fun 'number'/2, fun 'string'/2]), fun '__'/2, p_choose([p_string(<<"<=">>), p_string(<<">=">>), p_string(<<"<">>), p_string(<<">">>), p_string(<<"==">>), p_string(<<"!=">>), p_string(<<"\/=">>), p_string(<<"<>">>)]), fun '__'/2, p_choose([fun 'number'/2, fun 'string'/2])]))(I,D) end, fun(Node, Idx) ->{call, case lists:nth(3, Node) of
  <<"<=">> -> lte;
  <<">=">> -> gte;
  <<"<">> -> lt;
  <<">">> -> gt;
  <<"!=">> -> ne;
  <<"/=">> -> ne;
  <<"<>">> -> ne;
  <<"==">> -> eq
end, [lists:nth(1, Node), lists:nth(5, Node)], Idx} end).

-spec 'list'(input(), index()) -> parse_result().
'list'(Input, Index) ->
  p(Input, Index, 'list', fun(I,D) -> (p_seq([p_string(<<"[">>), fun 'pargs'/2, p_string(<<"]">>)]))(I,D) end, fun(Node, _Idx) ->lists:nth(2, Node) end).

-spec 'number'(input(), index()) -> parse_result().
'number'(Input, Index) ->
  p(Input, Index, 'number', fun(I,D) -> (p_seq([fun 'int'/2, p_optional(fun 'frac'/2), p_optional(fun 'mult'/2)]))(I,D) end, fun(Node, _Idx) ->
case Node of
  [Int, [], _] -> list_to_integer(binary_to_list(iolist_to_binary(Int)));
  [Int, Frac, _] -> list_to_float(binary_to_list(iolist_to_binary([Int, Frac])));
  _ -> list_to_float(binary_to_list(iolist_to_binary(Node)))
end * case lists:nth(3, Node) of
  [] -> 1;
  ['K'] -> 1000;
  ['M'] -> 1000000;
  ['G'] -> 1000000000
end
 end).

-spec 'unumber'(input(), index()) -> parse_result().
'unumber'(Input, Index) ->
  p(Input, Index, 'unumber', fun(I,D) -> (p_seq([p_choose([fun 'number'/2, fun 'single'/2]), fun '__'/2, p_choose([p_string(<<"b">>), p_string(<<"kb">>), p_string(<<"mb">>), p_string(<<"gb">>), p_string(<<"tb">>), p_string(<<"ms">>), p_string(<<"sec">>), p_string(<<"min">>), p_string(<<"h">>), p_string(<<"rpm">>), p_string(<<"rph">>), p_string(<<"rps">>)])]))(I,D) end, fun(Node, Idx) ->{value, lists:nth(1, Node), binary_to_atom(lists:nth(3, Node), latin1), Idx} end).

-spec 'frac'(input(), index()) -> parse_result().
'frac'(Input, Index) ->
  p(Input, Index, 'frac', fun(I,D) -> (p_seq([p_string(<<".">>), p_one_or_more(fun 'digit'/2)]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'int'(input(), index()) -> parse_result().
'int'(Input, Index) ->
  p(Input, Index, 'int', fun(I,D) -> (p_choose([p_seq([p_optional(p_string(<<"-">>)), p_seq([fun 'non_zero_digit'/2, p_one_or_more(fun 'digit'/2)])]), fun 'digit'/2]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'mult'(input(), index()) -> parse_result().
'mult'(Input, Index) ->
  p(Input, Index, 'mult', fun(I,D) -> (p_choose([p_string(<<"K">>), p_string(<<"M">>), p_string(<<"G">>)]))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'non_zero_digit'(input(), index()) -> parse_result().
'non_zero_digit'(Input, Index) ->
  p(Input, Index, 'non_zero_digit', fun(I,D) -> (p_charclass(<<"[1-9]">>))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'digit'(input(), index()) -> parse_result().
'digit'(Input, Index) ->
  p(Input, Index, 'digit', fun(I,D) -> (p_charclass(<<"[0-9]">>))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'string'(input(), index()) -> parse_result().
'string'(Input, Index) ->
  p(Input, Index, 'string', fun(I,D) -> (p_seq([p_string(<<"\"">>), p_label('chars', p_zero_or_more(p_seq([p_not(p_string(<<"\"">>)), p_choose([p_string(<<"\\\\">>), p_string(<<"\\\"">>), p_anything()])]))), p_string(<<"\"">>)]))(I,D) end, fun(Node, _Idx) ->mzb_string:unescape_ascii(binary_to_list(iolist_to_binary(proplists:get_value(chars, Node)))) end).

-spec 'id'(input(), index()) -> parse_result().
'id'(Input, Index) ->
  p(Input, Index, 'id', fun(I,D) -> (p_seq([p_charclass(<<"[a-z]">>), p_zero_or_more(p_charclass(<<"[a-z0-9_]">>))]))(I,D) end, fun(Node, _Idx) ->binary_to_atom(iolist_to_binary(Node), latin1) end).

-spec '__'(input(), index()) -> parse_result().
'__'(Input, Index) ->
  p(Input, Index, '__', fun(I,D) -> (p_zero_or_more(p_choose([p_seq([p_assert(p_string(<<"#">>)), p_zero_or_more(p_charclass(<<"[^\n]">>)), p_string(<<"\n">>)]), p_one_or_more(p_charclass(<<"[\s\t\n\s\r]">>))])))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'indent'(input(), index()) -> parse_result().
'indent'(Input, Index) ->
  p(Input, Index, 'indent', fun(I,D) -> (p_string(<<"_INDENT_">>))(I,D) end, fun(Node, _Idx) ->Node end).

-spec 'dedent'(input(), index()) -> parse_result().
'dedent'(Input, Index) ->
  p(Input, Index, 'dedent', fun(I,D) -> (p_string(<<"_DEDENT_">>))(I,D) end, fun(Node, _Idx) ->Node end).



-file("peg_includes.hrl", 1).
-type index() :: {{line, pos_integer()}, {column, pos_integer()}}.
-type input() :: binary().
-type parse_failure() :: {fail, term()}.
-type parse_success() :: {term(), input(), index()}.
-type parse_result() :: parse_failure() | parse_success().
-type parse_fun() :: fun((input(), index()) -> parse_result()).
-type xform_fun() :: fun((input(), index()) -> term()).

-spec p(input(), index(), atom(), parse_fun(), xform_fun()) -> parse_result().
p(Inp, StartIndex, Name, ParseFun, TransformFun) ->
  case get_memo(StartIndex, Name) of      % See if the current reduction is memoized
    {ok, Memo} -> %Memo;                     % If it is, return the stored result
      Memo;
    _ ->                                        % If not, attempt to parse
      Result = case ParseFun(Inp, StartIndex) of
        {fail,_} = Failure ->                       % If it fails, memoize the failure
          Failure;
        {Match, InpRem, NewIndex} ->               % If it passes, transform and memoize the result.
          Transformed = TransformFun(Match, StartIndex),
          {Transformed, InpRem, NewIndex}
      end,
      memoize(StartIndex, Name, Result),
      Result
  end.

-spec setup_memo() -> ets:tid().
setup_memo() ->
  put({parse_memo_table, ?MODULE}, ets:new(?MODULE, [set])).

-spec release_memo() -> true.
release_memo() ->
  ets:delete(memo_table_name()).

-spec memoize(index(), atom(), parse_result()) -> true.
memoize(Index, Name, Result) ->
  Memo = case ets:lookup(memo_table_name(), Index) of
              [] -> [];
              [{Index, Plist}] -> Plist
         end,
  ets:insert(memo_table_name(), {Index, [{Name, Result}|Memo]}).

-spec get_memo(index(), atom()) -> {ok, term()} | {error, not_found}.
get_memo(Index, Name) ->
  case ets:lookup(memo_table_name(), Index) of
    [] -> {error, not_found};
    [{Index, Plist}] ->
      case proplists:lookup(Name, Plist) of
        {Name, Result}  -> {ok, Result};
        _  -> {error, not_found}
      end
    end.

-spec memo_table_name() -> ets:tid().
memo_table_name() ->
    get({parse_memo_table, ?MODULE}).

-ifdef(p_eof).
-spec p_eof() -> parse_fun().
p_eof() ->
  fun(<<>>, Index) -> {eof, [], Index};
     (_, Index) -> {fail, {expected, eof, Index}} end.
-endif.

-ifdef(p_optional).
-spec p_optional(parse_fun()) -> parse_fun().
p_optional(P) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {fail,_} -> {[], Input, Index};
        {_, _, _} = Success -> Success
      end
  end.
-endif.

-ifdef(p_not).
-spec p_not(parse_fun()) -> parse_fun().
p_not(P) ->
  fun(Input, Index)->
      case P(Input,Index) of
        {fail,_} ->
          {[], Input, Index};
        {Result, _, _} -> {fail, {expected, {no_match, Result},Index}}
      end
  end.
-endif.

-ifdef(p_assert).
-spec p_assert(parse_fun()) -> parse_fun().
p_assert(P) ->
  fun(Input,Index) ->
      case P(Input,Index) of
        {fail,_} = Failure-> Failure;
        _ -> {[], Input, Index}
      end
  end.
-endif.

-ifdef(p_seq).
-spec p_seq([parse_fun()]) -> parse_fun().
p_seq(P) ->
  fun(Input, Index) ->
      p_all(P, Input, Index, [])
  end.

-spec p_all([parse_fun()], input(), index(), [term()]) -> parse_result().
p_all([], Inp, Index, Accum ) -> {lists:reverse( Accum ), Inp, Index};
p_all([P|Parsers], Inp, Index, Accum) ->
  case P(Inp, Index) of
    {fail, _} = Failure -> Failure;
    {Result, InpRem, NewIndex} -> p_all(Parsers, InpRem, NewIndex, [Result|Accum])
  end.
-endif.

-ifdef(p_choose).
-spec p_choose([parse_fun()]) -> parse_fun().
p_choose(Parsers) ->
  fun(Input, Index) ->
      p_attempt(Parsers, Input, Index, none)
  end.

-spec p_attempt([parse_fun()], input(), index(), none | parse_failure()) -> parse_result().
p_attempt([], _Input, _Index, Failure) -> Failure;
p_attempt([P|Parsers], Input, Index, FirstFailure)->
  case P(Input, Index) of
    {fail, _} = Failure ->
      case FirstFailure of
        none -> p_attempt(Parsers, Input, Index, Failure);
        _ -> p_attempt(Parsers, Input, Index, FirstFailure)
      end;
    Result -> Result
  end.
-endif.

-ifdef(p_zero_or_more).
-spec p_zero_or_more(parse_fun()) -> parse_fun().
p_zero_or_more(P) ->
  fun(Input, Index) ->
      p_scan(P, Input, Index, [])
  end.
-endif.

-ifdef(p_one_or_more).
-spec p_one_or_more(parse_fun()) -> parse_fun().
p_one_or_more(P) ->
  fun(Input, Index)->
      Result = p_scan(P, Input, Index, []),
      case Result of
        {[_|_], _, _} ->
          Result;
        _ ->
          {fail, {expected, Failure, _}} = P(Input,Index),
          {fail, {expected, {at_least_one, Failure}, Index}}
      end
  end.
-endif.

-ifdef(p_label).
-spec p_label(atom(), parse_fun()) -> parse_fun().
p_label(Tag, P) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {fail,_} = Failure ->
           Failure;
        {Result, InpRem, NewIndex} ->
          {{Tag, Result}, InpRem, NewIndex}
      end
  end.
-endif.

-ifdef(p_scan).
-spec p_scan(parse_fun(), input(), index(), [term()]) -> {[term()], input(), index()}.
p_scan(_, <<>>, Index, Accum) -> {lists:reverse(Accum), <<>>, Index};
p_scan(P, Inp, Index, Accum) ->
  case P(Inp, Index) of
    {fail,_} -> {lists:reverse(Accum), Inp, Index};
    {Result, InpRem, NewIndex} -> p_scan(P, InpRem, NewIndex, [Result | Accum])
  end.
-endif.

-ifdef(p_string).
-spec p_string(binary()) -> parse_fun().
p_string(S) ->
    Length = erlang:byte_size(S),
    fun(Input, Index) ->
      try
          <<S:Length/binary, Rest/binary>> = Input,
          {S, Rest, p_advance_index(S, Index)}
      catch
          error:{badmatch,_} -> {fail, {expected, {string, S}, Index}}
      end
    end.
-endif.

-ifdef(p_anything).
-spec p_anything() -> parse_fun().
p_anything() ->
  fun(<<>>, Index) -> {fail, {expected, any_character, Index}};
     (Input, Index) when is_binary(Input) ->
          <<C/utf8, Rest/binary>> = Input,
          {<<C/utf8>>, Rest, p_advance_index(<<C/utf8>>, Index)}
  end.
-endif.

-ifdef(p_charclass).
-spec p_charclass(string() | binary()) -> parse_fun().
p_charclass(Class) ->
    {ok, RE} = re:compile(Class, [unicode, dotall]),
    fun(Inp, Index) ->
            case re:run(Inp, RE, [anchored]) of
                {match, [{0, Length}|_]} ->
                    {Head, Tail} = erlang:split_binary(Inp, Length),
                    {Head, Tail, p_advance_index(Head, Index)};
                _ -> {fail, {expected, {character_class, binary_to_list(Class)}, Index}}
            end
    end.
-endif.

-ifdef(p_regexp).
-spec p_regexp(binary()) -> parse_fun().
p_regexp(Regexp) ->
    {ok, RE} = re:compile(Regexp, [unicode, dotall, anchored]),
    fun(Inp, Index) ->
        case re:run(Inp, RE) of
            {match, [{0, Length}|_]} ->
                {Head, Tail} = erlang:split_binary(Inp, Length),
                {Head, Tail, p_advance_index(Head, Index)};
            _ -> {fail, {expected, {regexp, binary_to_list(Regexp)}, Index}}
        end
    end.
-endif.

-ifdef(line).
-spec line(index() | term()) -> pos_integer() | undefined.
line({{line,L},_}) -> L;
line(_) -> undefined.
-endif.

-ifdef(column).
-spec column(index() | term()) -> pos_integer() | undefined.
column({_,{column,C}}) -> C;
column(_) -> undefined.
-endif.

-spec p_advance_index(input() | unicode:charlist() | pos_integer(), index()) -> index().
p_advance_index(MatchedInput, Index) when is_list(MatchedInput) orelse is_binary(MatchedInput)-> % strings
  lists:foldl(fun p_advance_index/2, Index, unicode:characters_to_list(MatchedInput));
p_advance_index(MatchedInput, Index) when is_integer(MatchedInput) -> % single characters
  {{line, Line}, {column, Col}} = Index,
  case MatchedInput of
    $\n -> {{line, Line+1}, {column, 1}};
    _ -> {{line, Line}, {column, Col+1}}
  end.
