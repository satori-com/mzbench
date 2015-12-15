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
           graphs => [#{metrics => [#{name => "counter"}] },
                      #{metrics => [#{name => "counter.rps"}],
                        units => "rps" }]}],
        build_graphite([{"counter", counter}])).

graphite_pass_options_test() ->
    ?assertEqual([
        #{ name => "Default",
           graphs => [#{metrics => [#{name => "counter",
                                      realtime => true}],
                        units   => "msg" },
                      #{metrics => [#{name => "counter.rps",
                                      realtime => true}],
                        units   => "msg/sec" }]}],
        build_graphite([{graph, #{ units => "msg",
                                   metrics => [{"counter", counter, #{realtime => true}}]}}])).

graphite_histogram_test() ->
    ?assertEqual([
        #{ name => "Default",
           graphs => [#{metrics => [ #{name => "bar.min"},
                                     #{name => "bar.max"},
                                     #{name => "bar.mean"},
                                     #{name => "bar.50"},
                                     #{name => "bar.75"},
                                     #{name => "bar.90"},
                                     #{name => "bar.95"},
                                     #{name => "bar.99"},
                                     #{name => "bar.999"}]}]}],
        build_graphite([{"bar", histogram}])).

graphite_counter_with_gauge_test() ->
    ?assertEqual([
        #{ name => "Default",
           graphs => [#{metrics => [ #{name => "gauge"},
                                     #{name => "counter"} ]},
                      #{metrics => [ #{name => "gauge"},
                                     #{name => "counter.rps"}]}]}],
        build_graphite([[{"counter", counter}, {"gauge", gauge}]])).

graphite_all_in_one_group_test() ->
    ?assertEqual([
        #{ name => "Default",
           graphs => [#{metrics => [ #{name => "counter"},
                                     #{name => "counter.rps"},
                                     #{name => "gauge"},
                                     #{name => "histogram.min"},
                                     #{name => "histogram.max"},
                                     #{name => "histogram.mean"},
                                     #{name => "histogram.50"},
                                     #{name => "histogram.75"},
                                     #{name => "histogram.90"},
                                     #{name => "histogram.95"},
                                     #{name => "histogram.99"},
                                     #{name => "histogram.999"}]}]}],
        build_graphite([[{"counter", counter}, {"gauge", gauge}, {"histogram", histogram}]])).

graphite_groups_test() ->
    ?assertEqual([
        #{ name => "Default",
           graphs => [#{metrics => [ #{name => "gauge"} ]}] },
        #{ name => "Group1",
           graphs => [#{metrics => [ #{name => "counter1"}]},
                      #{metrics => [ #{name => "counter1.rps"}],
                        units   => "rps" }]},
        #{ name => "Group2",
           graphs => [#{metrics => [ #{name => "counter2"}]},
                      #{metrics => [ #{name => "counter2.rps"}],
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

