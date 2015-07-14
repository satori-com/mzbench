-module(mzb_file).

-export(
   [
    expand_filename/1,
    wildcard/1,
    del_dir/1
   ]).

expand_filename("~/" ++ Filename) ->
    case init:get_argument(home) of
        {ok, [[HomeDir|_]|_]} ->
            filename:join(HomeDir, Filename);
        Error ->
            erlang:error({get_homedir_error, Error})
    end;
expand_filename(Filename) -> Filename.

wildcard(Wildcard) ->
    filelib:wildcard(expand_filename(Wildcard)).

del_dir(Dir) ->
    Files = [Dir|wildcard(filename:join(Dir, "**"))],
    RegularFiles = [F || F <- Files, filelib:is_regular(F)],
    Dirs = [F || F <- Files, filelib:is_dir(F)],
    SortedDirs = lists:usort(fun (S1, S2) -> length(S1) >= length(S2) end, Dirs),
    try
        _ = [{_, ok} = {F, file:delete(F)} || F <- RegularFiles],
        _ = [{_, ok} = {D, file:del_dir(D)} || D <- SortedDirs],
        ok
    catch
        error:{badmatch, Reason} ->
            {error, Reason}
    end.
