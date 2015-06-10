[ % in this example "E!" sould be printed after "C!"
    {pool, [{size, 1},
            {worker_type, dummy_worker}], [
        {set_signal, "C"},
        {wait, {1, sec}},
        {print, "C!"},
        {set_signal, "A"},
        {set_signal, "B"}]},
    {pool, [{size, 1},
            {worker_type, dummy_worker}], [
        {wait_signal, "C"},
        {wait_signal, "B"},
        {wait_signal, "A"},
        {print, "E!"}]}
].
