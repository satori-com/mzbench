[ 
  {make_install, [
    {git, {var, "mzbench_repo"}},
    {dir, "workers/exec"}]},
  {pool, [{size, 2}, {worker_type, exec_worker}],
    [{execute, "sleep 5"}]}
].
