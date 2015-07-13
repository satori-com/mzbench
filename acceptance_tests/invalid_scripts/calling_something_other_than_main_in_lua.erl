[ {make_install,
    [ {git, {var, "mzbench_repo", "git://github.com/machinezone/mzbench"}},
      {branch, "lua-metrics"},
      {dir, "workers/lua"}]},
  {pool,
    [{size, 3},
         {worker_type, lua, lua}],
    [{launch_the_missiles}]}
].