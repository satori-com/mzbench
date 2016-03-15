[ % this simplified http example is trying to request "target_url" at variable rate from 1 rps to 200
  % "ok" rate should be always greater than 0.5 per second, otherwise it fails
    {assert, always, {gt, "http_ok.rps", 0.5}},
    {make_install, [
        {git, "https://github.com/machinezone/mzbench.git"}, % worker location
        {dir, "workers/simple_http"}]}, % sub-folder in git repo
    {pool, [{size, {numvar, "worker_count", 20}}, % 20 parallel "threads"
            {worker_type, simple_http_worker}],
        [{loop, [{time, {120, sec}}, % execution time 120 seconds
                 {rate, {ramp, linear, {1, rps}, {{numvar, "max_rps", 200}, rps}}}],
            [{get, {var, "target_url", "http://172.21.3.3/index.html"}}]}]}
            % GET parameters are passed through url, for example {get, "http://172.21.3.3/?x=y"}
].
