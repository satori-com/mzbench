[ 
  {make_install, [
    {git, "https://github.com/machinezone/mzbench.git"},
    {dir, "workers/exec"}]},
  {pool, [{size, 2}, {worker_type, exec_worker}],
    [{execute, "sleep 5"}]}
].
