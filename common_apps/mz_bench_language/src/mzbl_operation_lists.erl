-module(mzbl_operation_lists).

-export([get_value/2, get_value/3]).

-include("mzbl_types.hrl").

-spec get_value(term(), [tuple()]) -> term().
get_value(Key, L) -> get_value(Key, L, undefined).

-spec get_value(term(), [tuple()], term()) -> term().
get_value(_, [], Default) -> Default;
get_value(Key, [#operation{name = Key} = Op | _], _) -> Op#operation.args;
get_value(Key, [_ | T], Default) -> get_value(Key, T, Default).
