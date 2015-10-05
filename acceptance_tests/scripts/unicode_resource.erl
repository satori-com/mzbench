[{include_resource, strings, {var, "strings_filename"}},
 {include_resource, readme, "https://raw.githubusercontent.com/machinezone/mzbench/master/README.md", text},
 {pool, [{size, 1},
        {worker_type, dummy_worker}],
    [{print, {choose, {resource, strings}}},
     {print, {resource, readme}}]
}].
