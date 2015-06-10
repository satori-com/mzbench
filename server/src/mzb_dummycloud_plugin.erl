-module(mzb_dummycloud_plugin).

-export([
    create_cluster/3,
    destroy_cluster/1,

    lock_localhost/0,
    unlock_localhost/0
]).

create_cluster(Name, N, _Config) ->
    case lock_localhost() of
        ok -> {ok, {Name, N}, undefined, ["localhost"]};
        {error, locked} -> erlang:error(localhost_is_busy)
    end.

destroy_cluster(_) ->
    unlock_localhost(),
    ok.

% Tmp functions
% We need to block second allocation on localhost somehow
% to prevent localhost parallel benchmarks.
lock_localhost() -> lock_localhost(60).

lock_localhost(Attempts) when Attempts =< 0 -> {error, locked};
lock_localhost(Attempts) ->
    case gen_server:call(mzb_api_server, lock_localhost) of
        ok -> ok;
        {error, locked} ->
            timer:sleep(1000),
            lock_localhost(Attempts - 1)
    end.

unlock_localhost() ->
    gen_server:call(mzb_api_server, unlock_localhost).
