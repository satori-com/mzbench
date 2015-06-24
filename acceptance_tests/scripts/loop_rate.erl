[{pool, [{size, 1},
          {worker_type, dummy_worker}],

    [{loop, [{time, {200, ms}}, {rate, {1.1, rps}}],
         [{print, "FOO"}]},
     {loop, [{time, {0.2, sec}}, {rate, {1, rpm}}],
         [{print, "FOO"}]},
     {loop, [{time, {0.0033, min}}, {rate, {1.1, rph}}],
         [{print, "FOO"}]},
     {loop, [{time, {0.00005, h}},
             {rate, {ramp, linear, {1.1, rpm},
                                   {2.1, rps}}}],
         [{print, "FOO"}]}]
}].
