[
    % total number of print operations should be greater than 200 at least for 30 seconds
    {assert, {30, sec}, {gt, "print", 200}},
    {assert, always, {eq, "workers.pool1.failed", 0}}, % number of failed workers should always be 0
    {assert, always, {lt, 9, "print.rps"}}, % 9 should be always less than print rate
    {pool, [{size, 1}, % one execution "thread"
            {worker_type, dummy_worker}],
        [{loop, [{time, {1, min}}, % total time is 1 minute
                    {rate, {10, rps}}], % constant rate is 10 operations per second
                [{print, "FOO"}]}]} % this operation prints "FOO" to console
].
