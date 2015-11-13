-module(mzb_api_bench_tests).
-include_lib("eunit/include/eunit.hrl").

cloud_plugin_test() ->
    application:set_env(mzbench_api, cloud_plugins, [{test_plugin, #{module => dummy_plugin}}]),
    ok = meck:new(dummy_plugin, [non_strict]),
    try
        ok = meck:expect(dummy_plugin, start, fun (test_plugin, #{}) -> instance end),
        ok = meck:expect(dummy_plugin, create_cluster, fun (instance, 2, ConfigMap) ->
                                                               ?assertMatch(#{user := "user",
                                                                              description := _,
                                                                              exclusive_node_usage := false
                                                                             }, ConfigMap),
                                                               {ok, dummy_cluster, dummy_user, dummy_hosts}
                                                       end),
        ok = meck:expect(dummy_plugin, destroy_cluster, fun (dummy_cluster) -> ok end),
        ok = meck:expect(dummy_plugin, foo, fun (clusterId) -> ok end),
        Config = #{cloud => undefined, initial_user => "user", purpose => "purpose", nodes_arg => 1, exclusive_node_usage => false},
        mzb_api_cloud:start_link(),
        {dummy_hosts, dummy_user, Deallocator} = (catch mzb_api_bench:allocate_hosts(Config, fun (_, _, _) -> ok end)),
        ok = Deallocator(),
        ?assert(meck:called(dummy_plugin, create_cluster, '_')),
        ?assert(meck:called(dummy_plugin, destroy_cluster, '_')),
        ?assert(meck:validate(dummy_plugin))
    after
        _ = (catch mzb_api_cloud:stop()),
        meck:unload(dummy_plugin)
    end.

allocate_existing_hosts_test() ->
    Config = #{initial_user => nothing, purpose => nothing, nodes_arg => [], exclusive_node_usage => false},
    ?assertError({different_users_for_hosts, ["a", "d"]}, mzb_api_bench:allocate_hosts(Config#{nodes_arg => ["a@b,d@c"]}, logger)),
    ?assertMatch({["b", "c"], "a", _}, mzb_api_bench:allocate_hosts(Config#{nodes_arg => ["a@b,c"]}, logger)),
    ?assertMatch({["b", "c"], "a", _}, mzb_api_bench:allocate_hosts(Config#{nodes_arg => ["a@b,a@c"]}, logger)),
    ?assertMatch({["b", "c", "d"], "a", _}, mzb_api_bench:allocate_hosts(Config#{nodes_arg => ["a@b,a@c,d"]}, logger)),
    ?assertMatch({["b", "c"], "root", _}, mzb_api_bench:allocate_hosts(Config#{nodes_arg => ["b,c"]}, logger)).
