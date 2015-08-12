-module(mzb_string).

-export([
    format/2,
    char_substitute/3,
    str_to_bstr/1
]).

-spec format(Format :: string(), Args :: [term()]) -> FlatString :: string().
format(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).

-spec char_substitute(string(), char(), char()) -> string().
char_substitute(String, OldChar, NewChar) ->
    lists:map(fun(Char) when Char =:= OldChar -> NewChar;
        (Char) -> Char end, String).

str_to_bstr([]) -> [];
str_to_bstr(T) when is_list(T) ->
    case io_lib:printable_list(T) of
        true -> list_to_binary(T);
        false -> [str_to_bstr(X) || X <- T]
    end;
str_to_bstr(T) when is_map(T) ->
    maps:from_list([{str_to_bstr(K), str_to_bstr(V)} || {K, V} <- maps:to_list(T)]);
str_to_bstr(T) -> T.

iso_8601_fmt(Seconds) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_universal_time({Seconds div 1000000, Seconds rem 1000000, 0}),
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
        [Year, Month, Day, Hour, Min, Sec]).
