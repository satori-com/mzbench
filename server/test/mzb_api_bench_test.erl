-module(mzb_api_bench_test).
-include_lib("eunit/include/eunit.hrl").

cloud_plugin_test() ->
    application:set_env(mz_bench_api, cloud_plugin, {module, dummy_plugin}),
    ok = meck:new(dummy_plugin, [non_strict]),
    try
        ok = meck:expect(dummy_plugin, create_cluster, fun ("purpose", 2, ConfigMap) ->
                                                               ?assertEqual(#{user => "user",
                                                                              description => "desc",
                                                                              exclusive_node_usage => false
                                                                             }, ConfigMap),
                                                               {ok, dummy_cluster, dummy_user, dummy_hosts}
                                                       end),
        ok = meck:expect(dummy_plugin, destroy_cluster, fun (dummy_cluster) -> ok end),
        ok = meck:expect(dummy_plugin, foo, fun (clusterId) -> ok end),
        {dummy_hosts, dummy_user, Deallocator} = mzb_api_bench:allocate_hosts("user", "purpose", 1, "desc", false, logger),
        ok = Deallocator(),
        ?assert(meck:called(dummy_plugin, create_cluster, '_')),
        ?assert(meck:called(dummy_plugin, destroy_cluster, '_')),
        ?assert(meck:validate(dummy_plugin))
    after
        meck:unload(dummy_plugin)
    end.


allocate_existing_hosts_test() ->
    ?assertError({different_users_for_hosts, ["a", "d"]}, mzb_api_bench:allocate_hosts(nothing, nothing, ["a@b,d@c"], nothing, nothing, logger)),
    ?assertMatch({["b", "c"], "a", _}, mzb_api_bench:allocate_hosts(nothing, nothing, ["a@b,c"], nothing, nothing, logger)),
    ?assertMatch({["b", "c"], "a", _}, mzb_api_bench:allocate_hosts(nothing, nothing, ["a@b,a@c"], nothing, nothing, logger)),
    ?assertMatch({["b", "c", "d"], "a", _}, mzb_api_bench:allocate_hosts(nothing, nothing, ["a@b,a@c,d"], nothing, nothing, logger)),
    ?assertMatch({["b", "c"], "root", _}, mzb_api_bench:allocate_hosts(nothing, nothing, ["b,c"], nothing, nothing, logger)).
