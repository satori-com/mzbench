[ % "exponential" worker start -- something that couldn't be done with loop
    {pool, [{size, 60},
            % Syntax: {exp, <N>, <time>}
            %  in this case <N> workers are expected to start in a given period of time
            {worker_start, {exp, 2, {1, sec}}},
            {worker_type, dummy_worker}],
        [{print, {sprintf, "My number is: ~p", [{round_robin, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]}]}}]}].
