[   %                  Name              File                Type
    {include_resource, erl_resource,    "resource_data.erl", erlang},
    {include_resource, text_resource,   "resource_data.txt", text},
    {include_resource, binary_resource, "resource_data.bin", binary},
    {include_resource, tsv_resource,    "resource_data.tsv", tsv},

    {pool, [{size, 3},
            {worker_type, dummy_worker}],
        [{print, {choose, {resource, erl_resource}}}]},

    {pool, [{size, 1},
            {worker_type, dummy_worker}],
        [{print, {resource, text_resource}}]},

    {pool, [{size, 1},
            {worker_type, dummy_worker}],
        [{print, {resource, binary_resource}}]},

    {pool, [{size, 3},
            {worker_type, dummy_worker}],
        [{print, {choose, {resource, tsv_resource}}}]}

].

% resource files could be used as function argument values
%
% sample output:
% 18:21:31.424 [info] <0.196.0> Appending "str1", Meta: [{function,print},{line,9},{pool_name,"pool1"},{worker_id,3}]
% ...
% 18:21:31.424 [info] <0.208.0> Appending <<0,1,2,3,4,5,6,7,8,9,10>>, Meta: [{function,print},{line,17},{pool_name,"pool3"},{worker_id,1}]
% ...
