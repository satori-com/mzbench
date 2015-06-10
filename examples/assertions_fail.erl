[
    {assert, {30, sec}, {gt, "print.value", 20}},
    % total number of print operations should be greater than 20 at least for 30 seconds
    {assert, always, {gt, "print.rps.value", 2}}, % print rate should be always greater than 2 (unsatisfiable)
    {pool, [{size, 1}, % one execution "thread"
            {worker_type, dummy_worker}],
        [{loop, [{time, {1, min}}, % total time is 1 minute
                    {rate, {1, rps}}], % constant rate is 1 operation per second
                [{print, "FOO"}]}]} % this operation prints "FOO" to console
].
