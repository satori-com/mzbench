[ 
  {make_install, [
    {git, {var, "mzbench_repo"}},
    {branch, {var, "worker_branch"}},
    {dir, "workers/lua"}]},
  {pool, [{size, 3}, {worker_type, lua, lua}],
    [{main}]}
].