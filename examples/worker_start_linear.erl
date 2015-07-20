[ % linear worker start, almost similar to loop with "spawn" option, 
  % but in this case workers won't share one state
    {pool, [{size, 60},
            {worker_start, {linear, {1, rps}}},
            {worker_type, dummy_worker}],
        [{print, {sprintf, "My number is: ~p", [{round_robin, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]}]}}]}
].
