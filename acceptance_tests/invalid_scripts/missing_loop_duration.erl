%% Ramp from 30 to 150 rps using 3 workers
[
    {pool, [{size, 3},
         {worker_type, dummy_worker}],
        []},
    {pool, [{size, 3},
         {worker_type, dummy_worker}],
        [{loop, [{rate, {ramp, linear, {10, rps}, {50, rps}}}],
            [{print, "Foo"}]}]}
].