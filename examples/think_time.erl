%% Think time -- sugar for comb with 1 second active and X second non-active periods
[
    {pool, [{size, 3},
         {worker_type, dummy_worker}],
        [{loop, [{time, {120, sec}},
           {rate, {think_time, {1, sec}, {10, rps}}}],
            [{print, "Foo"}]}]}
].
