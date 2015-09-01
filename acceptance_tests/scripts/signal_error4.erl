[
   {pool, [{size, {var, "nodes_num"}},
        {worker_type, dummy_worker}],
    [
    {loop, [{time, {200, ms}}, {rate, {1.1, rps}}],
         [
         {loop, [{time, {200, ms}}, {rate, {1.1, rps}}],
         [{set_signal, 'A'}]}]}]}
].