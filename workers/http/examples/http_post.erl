[ % post request benchmarking example is similar to get one, the only difference is request type
    {assert, always, {gt, "http_ok.rps", 0.5}},
    {make_install, [{git, "https://github.com/machinezone/mzbench.git"}, % worker installation source
                    {dir, "workers/http"}]},  % subdir
    {pool, [{size, {numvar, "conn_count", 20}}, % number of "threads" with default
            {worker_start, {linear, {10, rps}}},
            {worker_type, http_worker}], [
        {set_host, {var, "host", "172.21.3.3"}}, % each of these variables could be set
        {set_port, {numvar, "port", 80}}, % from command line with env option
        {loop, [{time, {60, sec}},
                {rate, {ramp, linear, {1, rps}, {{numvar, "max_rps", 2000}, rps}}}], % maximum with default
            [{post, {var, "endpoint", "/update"}, "{\"coords\": [10, 20], \"color\": \"red\"}"}]}]}
].
