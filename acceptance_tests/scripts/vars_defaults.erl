[
    {defaults, [
        {"var1", "var1_default_value"},
        {"var2", "var2_default_value"}
    ]},
    {pool, [{size, 1}, {worker_type, dummy_worker}], [
        {print, {sprintf, "the_var1_value_is_~s", [{var, "var1"}]}},
        {print, {sprintf, "the_var2_value_is_~s", [{var, "var2"}]}}
    ]}
].
