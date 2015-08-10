[
    {make_install, [
      {git, {var, "mzbench_repo"}},
      {branch, {var, "worker_branch"}},
      {dir, "worker_templates/python_empty"}]},
    {pool, [
        {size, 3},
        {worker_type, python_empty, python}
    ],
    [
        {my_print, "hello"}
    ]}
].
