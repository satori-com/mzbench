[
   {pool, [{size, {var, "nodes_num"}},
        {worker_type, dummy_worker}],
    [
    {wait_signal, 'A'},
    {set_signal,  'B'}
    ]},
   {pool, [{size, {var, "nodes_num"}},
        {worker_type, dummy_worker}],
    [
    {wait_signal, 'B'},
    {set_signal,  'C'}
    ]},
   {pool, [{size, {var, "nodes_num"}},
        {worker_type, dummy_worker}],
    [
    {wait_signal, 'C'},
    {set_signal,  'A'}
    ]}
].