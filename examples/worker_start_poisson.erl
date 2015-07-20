[ % "poisson" worker start -- something that couldn't be done with loop
    {pool, [{size, 60},
            % Syntax: {poisson, <L>}
            %  <L> - parameter λ, which is the expected number of "events" or "arrivals" that occur per second
            {worker_start, {poisson, {1, rps}}},
            {worker_type, dummy_worker}],
        [{print, {sprintf, "My number is: ~p", [{round_robin, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]}]}}]}
].
