-module(mzbl_literals).

-export([convert/1]).

-include("mzbl_types.hrl").

-spec convert(term()) -> term().
convert(#constant{value = #operation{} = Op} = C) -> C#constant{value = convert(Op)};
convert(#constant{value = Val, units = kb}) -> #constant{value = Val*1024, units = b};
convert(#constant{value = Val, units = mb}) -> #constant{value = Val*1024*1024, units = b};
convert(#constant{value = Val, units = gb}) -> #constant{value = Val*1024*1024*1024, units = b};
convert(#constant{value = Val, units = tb}) -> #constant{value = Val*1024*1024*1024*1024, units = b};
convert(#constant{value = Val, units = sec}) -> #constant{value = Val*1000, units = ms};
convert(#constant{value = Val, units = min}) -> #constant{value = Val*60*1000, units = ms};
convert(#constant{value = Val, units = h}) -> #constant{value = Val*3600*1000, units = ms};
convert(#constant{value = Val, units = rpm}) -> #constant{value = Val/60, units = rps};
convert(#constant{value = Val, units = rph}) -> #constant{value = Val/3600, units = rps};
convert(#operation{args = A} = O) -> O#operation{args = convert(A)};
convert(L)  when is_list(L) -> lists:map(fun convert/1, L);
convert(T)  when is_tuple(T) -> erlang:list_to_tuple(convert(erlang:tuple_to_list(T)));
convert(A) -> A.
