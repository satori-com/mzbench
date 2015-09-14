-module(mzb_dummycloud_plugin).

-export([
    create_cluster/3,
    destroy_cluster/1,

    lock_hosts/2,
    unlock_hosts/1
]).

create_cluster(_Name, N, _Config) ->
    Hosts = application:get_env(mzbench_api, dummycloud_hosts, ["localhost"]),

    case lock_hosts(N, Hosts) of
        {ok, L} -> {ok, L, undefined, L};
        {error, locked} -> erlang:error(hosts_are_busy)
    end.

destroy_cluster(Hosts) ->
    unlock_hosts(Hosts),
    ok.

% Tmp functions
% We need to block second allocation on localhost somehow
% to prevent localhost parallel benchmarks.
lock_hosts(N, Hosts) -> lock_hosts(N, Hosts, 60).

lock_hosts(_, Hosts, Attempts) when Attempts =< 0 -> {error, locked};
lock_hosts(N, Hosts, Attempts) ->
    case gen_server:call(mzb_api_server, {lock_hosts, N, Hosts}) of
        {ok, L} when is_list(L) -> {ok, L};
        {error, locked} ->
            timer:sleep(1000),
            lock_hosts(N, Hosts, Attempts - 1)
    end.

unlock_hosts(Hosts) ->
    gen_server:call(mzb_api_server, {unlock_hosts, Hosts}).
