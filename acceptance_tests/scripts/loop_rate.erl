[{pool, [{size, 1},
          {worker_type, dummy_worker}],

    [{loop, [{time, {200, ms}}, {rate, {1.1, rps}}],
         [{print, "loop#1"}]},
     {loop, [{time, {0.2, sec}}, {rate, {1, rpm}}],
         [{print, "loop#2"}]},
     {loop, [{time, {0.0033, min}}, {rate, {1.1, rph}}],
         [{print, "loop#3"}]},
     {loop, [{time, {0.00005, h}},
             {rate, {ramp, linear, {1.1, rpm},
                                   {2.1, rps}}}],
         [{print, "loop#4"}]},
     {loop, [{time, {0.00005, h}},
             {rate, {ramp, linear, {5, rps},
                                   {5, rpm}}}],
         [{print, "loop#5"}]}]
}].
