[ {make_install,
    [ {git, "file:///home/divanov/mz-bench"},
      {branch, "lua-metrics"},
      {dir, "workers/lua"}]},
  {pool,
    [{size, 3},
         {worker_type, lua, lua}],
    [{launch_the_missiles}]}
].