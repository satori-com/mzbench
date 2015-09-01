[
   {pool, [{size, {var, "nodes_num"}},
        {worker_type, dummy_worker}],
    [
    {parallel, [
        {wait_signal, 'A'},
        {set_signal,  'A'}]}]},

   {pool, [{size, {var, "nodes_num"}},
        {worker_type, dummy_worker}],
    [
    {loop, [{time, {200, ms}}, {rate, {1.1, rps}}],
         [
         {loop, [{time, {200, ms}}, {rate, {1.1, rps}}],
            [{parallel, [
                {wait_signal, 'B'},
                {set_signal,  'B'}]}]}]}]}
].