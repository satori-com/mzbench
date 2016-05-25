-module(mzb_multicloud_plugin_tests).

-include_lib("eunit/include/eunit.hrl").

multicloud_plugin_test() ->
    application:set_env(mzbench_api, cloud_plugins, [
            {test_plugin1, #{module => dummy_plugin1}},
            {test_plugin2, #{module => dummy_plugin2}},
            {multi       , #{module => mzb_multicloud_plugin, clouds => [{test_plugin1, 3}, {test_plugin2, 7}]}}
            ]),
    ok = meck:new(dummy_plugin1, [non_strict]),
    ok = meck:new(dummy_plugin2, [non_strict]),
    try
        ok = meck:expect(dummy_plugin1, start,
                fun (test_plugin1, #{}) ->
                    instance1
                end),
        ok = meck:expect(dummy_plugin1, create_cluster,
                fun (instance1, 6, _ConfigMap) ->
                    {ok, dummy_cluster1, dummy_user1, [dummy_hosts1]}
                end),
        ok = meck:expect(dummy_plugin1, destroy_cluster,
                fun (dummy_cluster1) ->
                    ok
                end),

        ok = meck:expect(dummy_plugin2, start,
                fun (test_plugin2, #{}) ->
                    instance2
                end),
        ok = meck:expect(dummy_plugin2, create_cluster,
                fun (instance2, 14, _ConfigMap) ->
                    {ok, dummy_cluster2, dummy_user2, [dummy_hosts2]}
                end),
        ok = meck:expect(dummy_plugin2, destroy_cluster,
                fun (dummy_cluster2) ->
                    ok
                end),

        Config = #{},%#{cloud => undefined, initial_user => "user", purpose => "purpose", exclusive_node_usage => false},

        mzb_api_cloud:start_link(),
        {ok, Id, dummy_user1, [dummy_hosts1, dummy_hosts2]} = mzb_api_cloud:create_cluster( _BenchId = 1234, multi, 20, Config),
        ok = mzb_api_cloud:destroy_cluster(Id),
        ?assert(meck:called(dummy_plugin1, create_cluster, '_')),
        ?assert(meck:called(dummy_plugin1, destroy_cluster, '_')),
        ?assert(meck:validate(dummy_plugin1)),
        ?assert(meck:called(dummy_plugin2, create_cluster, '_')),
        ?assert(meck:called(dummy_plugin2, destroy_cluster, '_')),
        ?assert(meck:validate(dummy_plugin2))
    after
        _ = (catch mzb_api_cloud:stop()),
        meck:unload(dummy_plugin1),
        meck:unload(dummy_plugin2)
    end.

