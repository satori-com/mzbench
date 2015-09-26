-module(mzb_git).

-export([
    get_git_short_sha1/3
]).

get_git_short_sha1(GitRepo, GitRef, Logger) ->
    case filelib:is_dir(GitRepo) of
        true ->
            _ = (catch mzb_subprocess:exec_format("bash -c \"cd ~s && git fetch --unshallow; true\"", [GitRepo], [stderr_to_stdout], Logger));
        false ->
            ok
    end,
    case {length(GitRef), lists:all(fun(Char) -> lists:member(Char, "01234567890abcdef") end, GitRef)} of
        {40, true} -> GitRef;
        _ ->
            Output = mzb_subprocess:exec_format("git ls-remote ~s ~s", [GitRepo, GitRef], [], Logger),
            case string:tokens(lists:flatten(Output), "\t") of
                [Commit | _] -> lists:sublist(Commit, 7);
                _ -> GitRef
            end
    end.

