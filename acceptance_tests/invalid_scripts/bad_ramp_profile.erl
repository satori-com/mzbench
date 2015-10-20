[{pool,
    [{size, 1}, {worker_type, dummy_worker}],
    [{loop,
        [{time, {1, ms}},
         {rate, {ramp, dirichlet, {1, rps}, {2, rps}}}],
        [{print, ""}]
    }]
}].