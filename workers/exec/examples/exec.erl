[ % shell command execution example, trying to execute "sleep" and "buy milk" commands in two threads
    {pool, [{size, 2},
           {worker_type, exec_worker}],
        [{loop, [{time, {20, sec}},
                 {rate, {1, rps}}],
            [{execute, "sleep 5"}]}]},
    {pool, [{size, 2}, % second pool starts simultaneously with the same rate and duration
            {worker_type, exec_worker}],
        [{loop, [{time, {20, sec}},
                 {rate, {1, rps}}],
            % unknown shell command (won't break execution)
            [{execute, "buy milk"}]}]}
].
