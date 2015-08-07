-module(mzb_metrics_tests).
-include_lib("eunit/include/eunit.hrl").

extract_exometer_test() ->
    Config = [
        {group, "Group1", [
            {graph, #{metrics => [{"counter1", counter}]}}]},
        {group, "Group2", [
            {graph, #{metrics => [{"counter2", counter}]}}]},
        {"gauge1", gauge},
        [{"histogram", histogram}, {"gauge2", gauge}]
    ],
    Normalized = mzb_script_metrics:normalize(Config),
    ExometerMetrics = mzb_metrics:extract_exometer_metrics(Normalized),
    ?assertEqual([{"gauge1",gauge},
                  {"histogram.min",gauge},
                  {"histogram.max",gauge},
                  {"histogram.mean",gauge},
                  {"histogram.50",gauge},
                  {"histogram.75",gauge},
                  {"histogram.90",gauge},
                  {"histogram.95",gauge},
                  {"histogram.99",gauge},
                  {"histogram.999",gauge},
                  {"gauge2",gauge},
                  {"counter1",counter},
                  {"counter1.rps",gauge},
                  {"counter2",counter},
                  {"counter2.rps",gauge}], ExometerMetrics).
