[
    {pre_hook, [{command, {exec, "echo pre_hook_1"}}]},

    {pre_hook, [{target, director},
                {command, {exec, "echo pre_hook_2"}}]},

    {pre_hook, [{target, director},
                {command, {worker_call, test_pre_hook, dummy_worker}}]},

    {pool, [{size, 3}, {worker_type, dummy_worker}], [
        {loop, [{time, {1, sec}},
                {rate, {ramp, linear, {10, rps}, {50, rps}}}],
            [{print, {var, "foo", "default"}}]}]},

    {post_hook, [{target, director},
                 {command, {exec, "echo post_hook_1"}}]}

].
