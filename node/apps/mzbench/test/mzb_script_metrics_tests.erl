-module(mzb_script_metrics_tests).

-include_lib("eunit/include/eunit.hrl").

normalization_bc_test() ->
    ?assertEqual(
       [{group, "Default", [
            {graph, #{metrics => [ {"counter", counter, #{}} ]} }]
       }],
       mzb_script_metrics:normalize([{"counter", counter}])),

    ?assertEqual(
       [{group, "Default", [
            {graph, #{metrics => [
                        {"counter1", counter, #{}},
                        {"counter2", counter, #{}}]}
            }]
       }],
       mzb_script_metrics:normalize([[{"counter1", counter},
                                      {"counter2", counter}]])).


graphite_one_metric_test() ->
    ?assertEqual([
        #{ name => "Default",
           graphs => [#{metrics => [#{name => "counter.value"}] },
                      #{metrics => [#{name => "counter.rps.value"}],
                        units => "rps" }]}],
        build_graphite([{"counter", counter}])).

graphite_pass_options_test() ->
    ?assertEqual([
        #{ name => "Default",
           graphs => [#{metrics => [#{name => "counter.value",
                                      realtime => true}],
                        units   => "msg" },
                      #{metrics => [#{name => "counter.rps.value",
                                      realtime => true}],
                        units   => "msg/sec" }]}],
        build_graphite([{graph, #{ units => "msg",
                                   metrics => [{"counter", counter, #{realtime => true}}]}}])).

graphite_histogram_test() ->
    ?assertEqual([
        #{ name => "Default",
           graphs => [#{metrics => [ #{name => "bar.min.value"},
                                     #{name => "bar.max.value"},
                                     #{name => "bar.mean.value"},
                                     #{name => "bar.50.value"},
                                     #{name => "bar.75.value"},
                                     #{name => "bar.90.value"},
                                     #{name => "bar.95.value"},
                                     #{name => "bar.99.value"},
                                     #{name => "bar.999.value"}]}]}],
        build_graphite([{"bar", histogram}])).

graphite_counter_with_gauge_test() ->
    ?assertEqual([
        #{ name => "Default",
           graphs => [#{metrics => [ #{name => "gauge.value"},
                                     #{name => "counter.value"} ]},
                      #{metrics => [ #{name => "gauge.value"},
                                     #{name => "counter.rps.value"}]}]}],
        build_graphite([[{"counter", counter}, {"gauge", gauge}]])).

graphite_all_in_one_group_test() ->
    ?assertEqual([
        #{ name => "Default",
           graphs => [#{metrics => [ #{name => "counter.value"},
                                     #{name => "counter.rps.value"},
                                     #{name => "gauge.value"},
                                     #{name => "histogram.min.value"},
                                     #{name => "histogram.max.value"},
                                     #{name => "histogram.mean.value"},
                                     #{name => "histogram.50.value"},
                                     #{name => "histogram.75.value"},
                                     #{name => "histogram.90.value"},
                                     #{name => "histogram.95.value"},
                                     #{name => "histogram.99.value"},
                                     #{name => "histogram.999.value"}]}]}],
        build_graphite([[{"counter", counter}, {"gauge", gauge}, {"histogram", histogram}]])).

graphite_groups_test() ->
    ?assertEqual([
        #{ name => "Default",
           graphs => [#{metrics => [ #{name => "gauge.value"} ]}] },
        #{ name => "Group1",
           graphs => [#{metrics => [ #{name => "counter1.value"}]},
                      #{metrics => [ #{name => "counter1.rps.value"}],
                        units   => "rps" }]},
        #{ name => "Group2",
           graphs => [#{metrics => [ #{name => "counter2.value"}]},
                      #{metrics => [ #{name => "counter2.rps.value"}],
                        units   => "rps" }]}],
        build_graphite([{group, "Group1", [
                            {graph, #{metrics => [{"counter1", counter}]}}]},
                            {group, "Group2", [
                                    {graph, #{metrics => [{"counter2", counter}]}}]},
                                    {"gauge", gauge}])).

normalize_positive_test() ->
    UserMetrics = [
        {group, "Publishers", [
            {graph, #{units   => "Num. of publishers",
                      metrics => [{"publish", counter, #{realtime => true}}]}},
            {graph, #{title   => "Publisher latencies",
                      metrics => [{"publish.latency", histogram}]}}
        ]},

        {"counter", counter},

        [{"counter1", counter}, {"counter2", counter, #{}}],

        {graph, #{units   => "foo",
                  metrics => [{"bar", counter}]}},

        {group, "Subscribers", [
            {graph, #{metrics => [{"sub", counter}]}}
        ]}
    ],

    NormalizedMetrics = mzb_script_metrics:normalize(UserMetrics),
    ?assertEqual([
        {group, "Default", [
            {graph, #{metrics => [{"counter", counter, #{}}]}},
            {graph, #{metrics => [{"counter1", counter, #{}},
                                  {"counter2", counter, #{}}]}},
            {graph, #{units => "foo", metrics => [{"bar", counter, #{}}]}}
         ]},
        {group, "Publishers", [
            {graph, #{units   => "Num. of publishers",
                      metrics => [{"publish", counter, #{realtime => true}}]}},

            {graph, #{title   => "Publisher latencies",
                      metrics => [{"publish.latency", histogram, #{}}]}}
        ]},
        {group, "Subscribers", [
            {graph, #{metrics => [{"sub", counter, #{}}]}}
        ]}
    ], NormalizedMetrics),


    NormalizedMetrics2 = mzb_script_metrics:normalize([[{"foo", "counter"}]]),
    ?assertEqual([
        {group, "Default", [
            {graph, #{metrics => [{"foo", counter, #{}}]}}
        ]}], NormalizedMetrics2).

normalizetion_negative_test() ->
    ?assertError({unknown_group_format, {group, foo, bar, zoh}},
                 mzb_script_metrics:normalize([{group, foo, bar, zoh}])),
    ?assertError({unknown_graph_format, #{foo := bar}},
                 mzb_script_metrics:normalize([#{foo => bar}])).


build_graphite(Metrics) ->
    Normalized = mzb_script_metrics:normalize(Metrics),
    mzb_script_metrics:build_metric_groups_json(Normalized).

