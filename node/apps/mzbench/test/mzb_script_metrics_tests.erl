-module(mzb_script_metrics_tests).

-include_lib("eunit/include/eunit.hrl").

normalization_bc_test() ->
    ?assertEqual(
       [{group, "Default", [
            {graph, #{metrics => [ {"counter", counter, #{visibility => true}} ]} }]
       }],
       mzb_script_metrics:normalize([{"counter", counter}])),

    ?assertEqual(
       [{group, "Default", [
            {graph, #{metrics => [
                        {"counter1", counter, #{visibility => true}},
                        {"counter2", counter, #{visibility => true}}]}
            }]
       }],
       mzb_script_metrics:normalize([[{"counter1", counter},
                                      {"counter2", counter}]])).

graphite_visibility_metric_test() ->
    ?assertMatch([
        #{ name:= "Default",
           graphs:= [#{metrics:= [#{name:= "counter", visibility:= true}] },
                      #{metrics:= [#{name:= "counter.rps", visibility:= true}],
                        units:= "rps" }]}],
        build_graphite([{"counter", counter}])),
    ?assertMatch([
        #{ name:= "Default",
           graphs:= [#{metrics:= [#{name:= "counter", rps_visibility:= false, visibility:= true}] },
                      #{metrics:= [#{name:= "counter.rps", visibility:= false}],
                        units:= "rps" }]}],
        build_graphite([{"counter", counter, #{rps_visibility => false}}])),
    ?assertMatch([
        #{ name:= "Default",
           graphs:= [#{metrics:= [#{name:= "counter", rps_visibility:= true, visibility:= false}] },
                      #{metrics:= [#{name:= "counter.rps", visibility:= true}],
                        units:= "rps" }]}],
        build_graphite([{"counter", counter, #{visibility => false, rps_visibility => true}}])).

graphite_one_metric_test() ->
    ?assertEqual([
        #{ name => "Default",
           graphs => [#{metrics => [#{name => "counter", visibility => true}], metatype => counters_and_gauges},
                      #{metrics => [#{name => "counter.rps", visibility => true}], metatype => counters_and_gauges,
                        units => "rps" }]}],
        build_graphite([{"counter", counter}])).

graphite_pass_options_test() ->
    ?assertMatch([
        #{ name:= "Default",
           graphs:= [#{metrics:= [#{name:= "counter",
                                    realtime:= true,
                                    visibility:= true}],
                        units:= "msg" },
                      #{metrics:= [#{name:= "counter.rps",
                                     realtime:= true,
                                     visibility:= true}],
                        units:= "msg/sec" }]}],
        build_graphite([{graph, #{ units => "msg",
                                   metrics => [{"counter", counter, #{realtime => true}}]}}])).

graphite_histogram_test() ->
    ?assertEqual([
        #{ name => "Default",
           graphs => [#{metrics => [ #{name => "bar.min", visibility => true},
                                     #{name => "bar.max", visibility => true},
                                     #{name => "bar.mean", visibility => true},
                                     #{name => "bar.50", visibility => true},
                                     #{name => "bar.75", visibility => true},
                                     #{name => "bar.90", visibility => true},
                                     #{name => "bar.95", visibility => true},
                                     #{name => "bar.99", visibility => true},
                                     #{name => "bar.999", visibility => true}], metatype => histograms}]}],
        build_graphite([{"bar", histogram}])).

graphite_counter_with_gauge_test() ->
    ?assertEqual([
        #{ name => "Default",
           graphs => [#{metrics => [ #{name => "gauge", visibility => true},
                                     #{name => "counter", visibility => true} ], metatype => counters_and_gauges},
                      #{metrics => [ #{name => "gauge", visibility => true},
                                     #{name => "counter.rps", visibility => true}], metatype => counters_and_gauges}]}],
        build_graphite([[{"counter", counter}, {"gauge", gauge}]])).

graphite_all_in_one_group_test() ->
    ?assertEqual([
        #{ name => "Default",
           graphs => [#{metrics => [ #{name => "counter", visibility => true},
                                     #{name => "counter.rps", visibility => true},
                                     #{name => "gauge", visibility => true},
                                     #{name => "histogram.min", visibility => true},
                                     #{name => "histogram.max", visibility => true},
                                     #{name => "histogram.mean", visibility => true},
                                     #{name => "histogram.50", visibility => true},
                                     #{name => "histogram.75", visibility => true},
                                     #{name => "histogram.90", visibility => true},
                                     #{name => "histogram.95", visibility => true},
                                     #{name => "histogram.99", visibility => true},
                                     #{name => "histogram.999", visibility => true}], metatype => mixed}]}],
        build_graphite([[{"counter", counter}, {"gauge", gauge}, {"histogram", histogram}]])).

graphite_groups_test() ->
    ?assertEqual([
        #{ name => "Default",
           graphs => [#{metrics => [ #{name => "gauge", visibility => true} ], metatype => counters_and_gauges }] },
        #{ name => "Group1",
           graphs => [#{metrics => [ #{name => "counter1", visibility => true}], metatype => counters_and_gauges},
                      #{metrics => [ #{name => "counter1.rps", visibility => true}], metatype => counters_and_gauges,
                        units   => "rps" }]},
        #{ name => "Group2",
           graphs => [#{metrics => [ #{name => "counter2", visibility => true}], metatype => counters_and_gauges},
                      #{metrics => [ #{name => "counter2.rps", visibility => true}], metatype => counters_and_gauges,
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
            {graph, #{metrics => [{"counter", counter, #{visibility => true}}]}},
            {graph, #{metrics => [{"counter1", counter, #{visibility => true}},
                                  {"counter2", counter, #{visibility => true}}]}},
            {graph, #{units => "foo", metrics => [{"bar", counter, #{visibility => true}}]}}
         ]},
        {group, "Publishers", [
            {graph, #{units   => "Num. of publishers",
                      metrics => [{"publish", counter, #{realtime => true,visibility => true}}]}},

            {graph, #{title   => "Publisher latencies",
                      metrics => [{"publish.latency", histogram, #{visibility => true}}]}}
        ]},
        {group, "Subscribers", [
            {graph, #{metrics => [{"sub", counter, #{visibility => true}}]}}
        ]}
    ], NormalizedMetrics),


    NormalizedMetrics2 = mzb_script_metrics:normalize([[{"foo", "counter"}]]),
    ?assertEqual([
        {group, "Default", [
            {graph, #{metrics => [{"foo", counter, #{visibility => true}}]}}
        ]}], NormalizedMetrics2).

normalizetion_negative_test() ->
    ?assertError({invalid_metric_name, atom_name},
                 mzb_script_metrics:normalize([{atom_name, counter}])),
    ?assertError({invalid_metric_type, unknown},
                 mzb_script_metrics:normalize([{"name", unknown}])),
    ?assertError({missing_resolver_function, "name"},
                 mzb_script_metrics:normalize([{"name", derived}])),
    ?assertError({invalid_resolver_function, 1},
                 mzb_script_metrics:normalize([{"name", derived, #{resolver => 1}}])),
    ?assertError({invalid_group_format, {group, foo, bar, zoh}},
                 mzb_script_metrics:normalize([{group, foo, bar, zoh}])),
    ?assertError({invalid_graph_format, #{foo := bar}},
                 mzb_script_metrics:normalize([#{foo => bar}])).

build_graphite(Metrics) ->
    Normalized = mzb_script_metrics:normalize(Metrics),
    mzb_script_metrics:build_metric_groups_json(Normalized).

