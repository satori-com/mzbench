-module(mzb_metrics_tests).
-include_lib("eunit/include/eunit.hrl").

build_metric_groups_test() ->

    ?assertEqual([[{"test", counter}], [{"test.rps", gauge}]], mzb_metrics:build_metric_groups({"test", counter})),

    ?assertEqual([[{"test", gauge}]], mzb_metrics:build_metric_groups({"test", gauge})),
    ?assertEqual([[{"test", gauge}]], mzb_metrics:build_metric_groups([{"test", gauge}])),

    ?assertEqual([[{"test.min",gauge},
                   {"test.max",gauge},
                   {"test.mean",gauge},
                   {"test.50",gauge},
                   {"test.75",gauge},
                   {"test.90",gauge},
                   {"test.95",gauge},
                   {"test.99",gauge},
                   {"test.999",gauge}]],
                  mzb_metrics:build_metric_groups({"test", histogram})),

    ?assertEqual([
                    [{"test3", gauge}, {"test1", counter}, {"test2", counter}],
                    [{"test3", gauge}, {"test1.rps", gauge}, {"test2.rps", gauge}]
                 ], mzb_metrics:build_metric_groups([{"test1", counter}, {"test2", counter}, {"test3", gauge}])),

    ?assertEqual([[{"test1", counter},
                   {"test1.rps", gauge},
                   {"test3", gauge},
                   {"test4.min",gauge},
                   {"test4.max",gauge},
                   {"test4.mean",gauge},
                   {"test4.50",gauge},
                   {"test4.75",gauge},
                   {"test4.90",gauge},
                   {"test4.95",gauge},
                   {"test4.99",gauge},
                   {"test4.999",gauge}]],
                 mzb_metrics:build_metric_groups([{"test1", counter}, {"test3", gauge}, {"test4", histogram}])),

    ?assertEqual([
                    [{"test3", gauge}, {"test1", counter}, {"test2", counter}],
                    [{"test3", gauge}, {"test1.rps", gauge}, {"test2.rps", gauge}]
                 ], mzb_metrics:build_metric_groups([{"test1", counter}, {"test2", counter}, {"test3", gauge}])),

    ?assertEqual([[{"test1", gauge}, {"test2", gauge}]],
                 mzb_metrics:build_metric_groups([{"test1", gauge}, {"test2", gauge}])),


    ?assertEqual([["prefix.test1.value", "prefix.test2.value"]],
                  mzb_metrics:build_graphite_groups("prefix", [[{"test1",gauge}, {"test2",gauge}]])).
