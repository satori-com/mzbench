[ % the simplest parallel
    {pool, [{size, 3}, % three execution "threads"
            {worker_type, dummy_worker}],
        [{parallel, [{print, "FOO"}, {print, "BAR"}, {print, "BAZ"}]}]}
].
