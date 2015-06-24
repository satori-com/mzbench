[{include_resource, strings, {var, "strings_filename"}},
 {pool, [{size, 1},
        {worker_type, dummy_worker}],
    [{print, {choose, {resource, strings}}}]
}].
