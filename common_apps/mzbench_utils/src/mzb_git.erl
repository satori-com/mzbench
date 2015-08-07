-module(mzb_git).

-export([
    get_git_short_sha1/3
]).

get_git_short_sha1(GitRepo, GitRef, Logger) ->
    case string:tokens(lists:flatten(mzb_subprocess:exec_format("git ls-remote ~s ~s", [GitRepo, GitRef], [stderr_to_stdout], Logger)), "\t") of
        [Commit | _] -> lists:sublist(Commit, 7);
        _ -> GitRef
    end.

