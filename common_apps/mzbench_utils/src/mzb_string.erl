-module(mzb_string).

-export([
    format/2,
    char_substitute/3
]).

-spec format(Format :: string(), Args :: [term()]) -> FlatString :: string().
format(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).

-spec char_substitute(string(), char(), char()) -> string().
char_substitute(String, OldChar, NewChar) ->
    lists:map(fun(Char) when Char =:= OldChar -> NewChar;
        (Char) -> Char end, String).
