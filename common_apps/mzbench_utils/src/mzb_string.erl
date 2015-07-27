-module(mzb_string).

-export([
    char_substitute/3
]).

-spec char_substitute(string(), char(), char()) -> string().
char_substitute(String, OldChar, NewChar) ->
    lists:map(fun(Char) when Char =:= OldChar -> NewChar;
        (Char) -> Char end, String).
