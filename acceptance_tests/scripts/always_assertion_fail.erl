[
    {assert, always, {gt, "print.rps.value", 3.5}}, % print rate should be always greater than 3.5 (unsatisfiable)
    {pool, [{size, 1},
            {worker_type, dummy_worker}],
        [{loop, [{time, {1, min}},
                 {rate, {ramp, linear, {5, rps}, {0, rps}}}],
                [{print, "FOO"}]}]}
].
