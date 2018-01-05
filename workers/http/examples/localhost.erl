%% Test a server running on a local machine on port 8080.
%% Send 10 requests per second for 1 minute from 5 nodes in parallel,
%% which totals up to 50 requests per second altogether.

[ {make_install, 
    [
        {git, "https://github.com/satori-com/mzbench.git"},
        {dir, "workers/http"}
    ]
  },
  {pool,
    [
        {size, 5},
        {worker_type, http_worker}
    ],
    [
        {connect, "localhost", 8080},
        {loop,
            [
                {time, {1, min}},
                {rate, {10, rps}}
            ],
            [
                {get, "/"}
            ]
        }
    ]
  }
].
