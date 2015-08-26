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
    ?assertMatch([{"gauge1", gauge ,_},
                  {"histogram.min", gauge, _},
                  {"histogram.max", gauge, _},
                  {"histogram.mean", gauge, _},
                  {"histogram.50", gauge, _},
                  {"histogram.75", gauge, _},
                  {"histogram.90", gauge, _},
                  {"histogram.95", gauge, _},
                  {"histogram.99", gauge, _},
                  {"histogram.999", gauge, _},
                  {"gauge2", gauge, _},
                  {"counter1", counter, _},
                  {"counter1.rps", gauge, _},
                  {"counter2", counter, _},
                  {"counter2.rps", gauge, _}], ExometerMetrics).
