[ % three different parallel examples
    {pool, [{size, 1},
         {worker_type, dummy_worker}],
        [
        % each iteration of the following loop would be executed in a spawned process,
        % without spawn option next iteration will start only after previous has finished
        % with spawn option new process will be spawned at given rate
        {loop, [{time, {15, sec}}, {spawn, true},
           {rate, {1, rps}}],
            [{print, "Foo"}]},

        % use two threads and try to gain 1 rps, this way is almost similar to bigger pool size
        % the difference is state sharing, in this case worker context would be the same
        {loop, [{time, {15, sec}}, {parallel, 2},
           {rate, {1, rps}}],
            [{print, "Foo"}]},

        % try to gain 1 rps with 10 simultaneous processes
        {loop, [{time, {15, sec}}, {parallel, 10},
           {rate, {ramp, linear, {1, rps}, {10, rps}}}],
            [{print, "Foo"}]}]}
].
