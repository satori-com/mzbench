[

% iface: lo                     | iface: eth0
% hardware: mac                 | harware: p16-17.addsrv.net
%
% 1 connection                  | 1 connection     | 10 connections
% ---------------------------------------------------------------------------------------
% Msg size  | rps, x1000    Mbps  |  rps, x1000   Mbps  | rps, x1000    Mbps    CPU%
% ---------------------------------------------------------------------------------------
% 256b      |   87          170   |    32          62   |   300         585     1000
% 512b      |   88          343   |    27          105  |   230         898     800
% 1K        |   87          679   |    28.5        222  |   115         875     400
% 2K        |   88          1375  |    26.5        414  |   57.4        896     220
% 3K        |   61          1429  |    24          562  |   38.3        897     140
% 4K        |   45          1406  |    19          593  |   28.7        896     120
% 5K        |   35          1367  |    15          585  |   23          898     90
% 6K        |   30          1406  |    12.5        585  |   19.2        900     74
% 8K        |   22.5        1406  |    9.5         593  |   14.3        893     50
% 16K       |   11          1375  |    4           500  |   7.1         887     30

  {pool, [{size, {var, "workers", 10}},
          {worker_type, tcp_worker}], [
    {connect, {var, "host", {choose, {var, "bench_hosts", ["localhost"]}}}, {var, "port", 23423}},
    {send, {var, "first-message", <<"first message">>}},
    {loop, [{time, {{var, "duration", 100}, sec}},
            {rate, {{var, "message-rate", 50000}, rps}}
            ],
        [
            {send, {var, "message", <<"message">>}}
        ]},
    {disconnect}
  ]},
  {pool, [{size, {var, "nodes_num", 1}},
          {worker_type, tcp_worker}], [
   {start_receivers, {var, "port", 23423}, {var, "receivers-num", 100}, {var, "reconnect-timeout", infinity}, {{var, "duration", 100}, sec}}
  ]}
].
