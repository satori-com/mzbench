%% Test a server running on a local machine on port 8080.
%% Send 10 requests per second for 1 minute from 5 nodes in parallel,
%% which totals up to 50 requests per second altogether.

[ {pool,
    [
        {size, 5},
        {worker_type, http_worker}
    ],
    [
        {set_host, "localhost"},
        {set_port, 8080},
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
}].
