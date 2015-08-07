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
           graphs => [#{metrics => [#{name => "foo.counter.value"}] },
                      #{metrics => [#{name => "foo.counter.rps.value"}],
                        units => "rps" }]}],
        build_graphite("foo", [{"counter", counter}])).

graphite_pass_options_test() ->
    ?assertEqual([
        #{ name => "Default",
           graphs => [#{metrics => [#{name => "foo.counter.value",
                                      realtime => true}],
                        units   => "msg" },
                      #{metrics => [#{name => "foo.counter.rps.value",
                                      realtime => true}],
                        units   => "msg/sec" }]}],
        build_graphite("foo", [{graph, #{ units => "msg",
                                          metrics => [{"counter", counter, #{realtime => true}}]}}])).

graphite_histogram_test() ->
    ?assertEqual([
        #{ name => "Default",
           graphs => [#{metrics => [ #{name => "foo.bar.min.value"},
                                     #{name => "foo.bar.max.value"},
                                     #{name => "foo.bar.mean.value"},
                                     #{name => "foo.bar.50.value"},
                                     #{name => "foo.bar.75.value"},
                                     #{name => "foo.bar.90.value"},
                                     #{name => "foo.bar.95.value"},
                                     #{name => "foo.bar.99.value"},
                                     #{name => "foo.bar.999.value"}]}]}],
        build_graphite("foo", [{"bar", histogram}])).

graphite_counter_with_gauge_test() ->
    ?assertEqual([
        #{ name => "Default",
           graphs => [#{metrics => [ #{name => "foo.gauge.value"},
                                     #{name => "foo.counter.value"} ]},
                      #{metrics => [ #{name => "foo.gauge.value"},
                                     #{name => "foo.counter.rps.value"}]}]}],
        build_graphite("foo", [[{"counter", counter}, {"gauge", gauge}]])).

graphite_all_in_one_group_test() ->
    ?assertEqual([
        #{ name => "Default",
           graphs => [#{metrics => [ #{name => "foo.counter.value"},
                                     #{name => "foo.counter.rps.value"},
                                     #{name => "foo.gauge.value"},
                                     #{name => "foo.histogram.min.value"},
                                     #{name => "foo.histogram.max.value"},
                                     #{name => "foo.histogram.mean.value"},
                                     #{name => "foo.histogram.50.value"},
                                     #{name => "foo.histogram.75.value"},
                                     #{name => "foo.histogram.90.value"},
                                     #{name => "foo.histogram.95.value"},
                                     #{name => "foo.histogram.99.value"},
                                     #{name => "foo.histogram.999.value"}]}]}],
        build_graphite("foo", [[{"counter", counter}, {"gauge", gauge}, {"histogram", histogram}]])).

graphite_groups_test() ->
    ?assertEqual([
        #{ name => "Default",
           graphs => [#{metrics => [ #{name => "foo.gauge.value"} ]}] },
        #{ name => "Group1",
           graphs => [#{metrics => [ #{name => "foo.counter1.value"}]},
                      #{metrics => [ #{name => "foo.counter1.rps.value"}],
                        units   => "rps" }]},
        #{ name => "Group2",
           graphs => [#{metrics => [ #{name => "foo.counter2.value"}]},
                      #{metrics => [ #{name => "foo.counter2.rps.value"}],
                        units   => "rps" }]}],
        build_graphite("foo", [{group, "Group1", [
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
    ], NormalizedMetrics).

normalizetion_negative_test() ->
    ?assertError({unknown_group_format, {group, foo, bar, zoh}},
                 mzb_script_metrics:normalize([{group, foo, bar, zoh}])),
    ?assertError({unknown_graph_format, #{foo := bar}},
                 mzb_script_metrics:normalize([#{foo => bar}])),
    ?assertError({unknown_metric_format, {"foo", "counter"}},
                 mzb_script_metrics:normalize([[{"foo", "counter"}]])).

build_graphite(Prefix, Metrics) ->
    Normalized = mzb_script_metrics:normalize(Metrics),
    mzb_script_metrics:build_graphite_groups(Prefix, Normalized).

