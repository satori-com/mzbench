[ 
  {make_install, [
    {git, "https://github.com/machinezone/mzbench.git"},
    {branch, {var, "worker_branch", "master"}},
    {dir, "workers/exec"}]},
  {pool, [{size, 2}, {worker_type, exec_worker}],
    [{execute, "sleep 5"}]}
].
