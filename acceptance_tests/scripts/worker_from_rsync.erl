[ 
  {make_install, [
    {rsync, {var, "exec_worker_dir"}},
    {exclude, "deps"}]},
  {pool, [{size, 2}, {worker_type, exec_worker}],
    [{execute, "sleep 5"}]}
].