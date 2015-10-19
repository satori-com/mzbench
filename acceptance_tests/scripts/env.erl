[{pool, [{size, {var, "pool_size"}},
        {worker_type, dummy_worker}],
    [{print, {var, "jozin"}}, 
     {print, {var, "missing", "fallback"}}, 
     {wait, {{numvar, "wait_ms"}, ms}},
     {wait, {{var, "wait_ms", 1}, ms}},
     {wait, {{var, "wait_ms_undefined", 5}, ms}},
     {set_signal, "A"}]
},
   {pool, [{size, 1},
          {worker_type, dummy_worker}], [
     {wait_signal, "A"},
     {loop,
         [{time, {{var, "loop_time", 1}, sec}},
          {rate, {{var, "loop_rate", 1}, rps}},
          {iterator, "it"}],
         [{print, {sprintf, "Iteration ~p", [{var, "it"}]}}]
     }
  ]}
].
