[ % this example is almost similar to simple_http the only difference is worker interface
    {assert, always, {gt, "http_ok.rps.value", 0.5}},
    {make_install, [{git, "https://github.com/machinezone/mzbench.git"},
                    {dir, "workers/http"}]},
    {pool, [{size, {numvar, "conn_count", 20}},
            {worker_type, http_worker}], [
        {set_host, {var, "host", "172.21.3.3"}}, % separate place for host and port definition
        {set_port, {numvar, "port", 80}},
        {loop, [{time, {60, sec}},
                {rate, {ramp, linear, {1, rps}, {{numvar, "max_rps", 2000}, rps}}}],
            [{get, {var, "endpoint", "/index.html"}}]}]}
].
