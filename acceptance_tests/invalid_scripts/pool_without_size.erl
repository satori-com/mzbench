[{pool,
    [{worker_type, dummy_worker}],
    [{loop,
        [{time, {1, ms}},
         {rate, {ramp, linear, {1, rps}, {2, rps}}}],
        [{print, ""}]
    }]
}].