-module(mzb_system_load_monitor_tests).
-include_lib("eunit/include/eunit.hrl").

netstat_linux1_parse_test() ->
    Output =
"Kernel Interface table
docker0: flags=4099<UP,BROADCAST,MULTICAST>  mtu 1500
        inet 172.18.42.1  netmask 255.255.0.0  broadcast 0.0.0.0
        ether 02:42:7b:d1:cc:f7  txqueuelen 0  (Ethernet)
        RX packets 0  bytes 0 (0.0 B)
        RX errors 0  dropped 0  overruns 0  frame 0
        TX packets 0  bytes 0 (0.0 B)
        TX errors 0  dropped 0 overruns 0  carrier 0  collisions 0

enp0s3: flags=4163<UP,BROADCAST,RUNNING,MULTICAST>  mtu 1500
        inet 10.0.2.15  netmask 255.255.255.0  broadcast 10.0.2.255
        inet6 fe80::a00:27ff:fef6:b007  prefixlen 64  scopeid 0x20<link>
        ether 08:00:27:f6:b0:07  txqueuelen 1000  (Ethernet)
        RX packets 627  bytes 66976 (65.4 KiB)
        RX errors 0  dropped 0  overruns 0  frame 0
        TX packets 477  bytes 60243 (58.8 KiB)
        TX errors 0  dropped 0 overruns 0  carrier 0  collisions 0

enp0s8: flags=4163<UP,BROADCAST,RUNNING,MULTICAST>  mtu 1500
        inet 172.17.2.9  netmask 255.255.255.0  broadcast 172.17.2.255
        inet6 fe80::a00:27ff:fe9a:a9ea  prefixlen 64  scopeid 0x20<link>
        ether 08:00:27:9a:a9:ea  txqueuelen 1000  (Ethernet)
        RX packets 0  bytes 0 (0.0 B)
        RX errors 0  dropped 0  overruns 0  frame 0
        TX packets 21  bytes 1566 (1.5 KiB)
        TX errors 0  dropped 0 overruns 0  carrier 0  collisions 0

lo: flags=73<UP,LOOPBACK,RUNNING>  mtu 65536
        inet 127.0.0.1  netmask 255.0.0.0
        inet6 ::1  prefixlen 128  scopeid 0x10<host>
        loop  txqueuelen 0  (Local Loopback)
        RX packets 88  bytes 8277 (8.0 KiB)
        RX errors 0  dropped 0  overruns 0  frame 0
        TX packets 88  bytes 8277 (8.0 KiB)
        TX errors 0  dropped 0 overruns 0  carrier 0  collisions 0

",
    ?assertEqual({75253,70086}, mzb_system_load_monitor:parse_linux_netstat_output(Output)).

netstat_linux2_parse_test() ->
    Output =
"Kernel Interface table
eth0      Link encap:Ethernet  HWaddr 02:42:AC:11:00:4F
          inet addr:172.17.0.79  Bcast:0.0.0.0  Mask:255.255.0.0
          inet6 addr: fe80::42:acff:fe11:4f/64 Scope:Link
          UP BROADCAST RUNNING  MTU:9001  Metric:1
          RX packets:217532415 errors:0 dropped:0 overruns:0 frame:0
          TX packets:259342117 errors:0 dropped:0 overruns:0 carrier:0
          collisions:0 txqueuelen:0
          RX bytes:16533820912 (15.3 GiB)  TX bytes:128116084567 (119.3 GiB)

lo        Link encap:Local Loopback
          inet addr:127.0.0.1  Mask:255.0.0.0
          inet6 addr: ::1/128 Scope:Host
          UP LOOPBACK RUNNING  MTU:65536  Metric:1
          RX packets:108100757 errors:0 dropped:0 overruns:0 frame:0
          TX packets:108100757 errors:0 dropped:0 overruns:0 carrier:0
          collisions:0 txqueuelen:0
          RX bytes:9170812056 (8.5 GiB)  TX bytes:9170812056 (8.5 GiB)

",
    ?assertEqual({25704632968,137286896623}, mzb_system_load_monitor:parse_linux_netstat_output(Output)).

netstat_darwin_parse_test() ->
    Output =
"Name  Mtu   Network       Address            Ipkts Ierrs     Ibytes    Opkts Oerrs     Obytes  Coll
lo0   16384 <Link#1>                          5289     0     238271     5289     0     238271     0
lo0   16384 ::1/128     ::1                   5289     -     238271     5289     -     238271     -
lo0   16384 127           127.0.0.1           5289     -     238271     5289     -     238271     -
lo0   16384 fe80::1%lo0 fe80:1::1             5289     -     238271     5289     -     238271     -
gif0* 1280  <Link#2>                             0     0          0        0     0          0     0
stf0* 1280  <Link#3>                             0     0          0        0     0          0     0
en0   1500  <Link#4>    78:31:c1:be:08:da   163292     0  158702977   131377     0   17967004     0
en0   1500  fe80::7a31: fe80:4::7a31:c1ff   163292     -  158702977   131377     -   17967004     -
en0   1500  192.168.1     192.168.1.177     163292     -  158702977   131377     -   17967004     -
en1   1500  <Link#5>    72:00:01:f0:7e:b0        0     0          0        0     0          0     0
en2   1500  <Link#6>    72:00:01:f0:7e:b1        0     0          0        0     0          0     0
p2p0  2304  <Link#7>    0a:31:c1:be:08:da        0     0          0        0     0          0     0
awdl0 1452  <Link#8>    56:35:b4:dd:6d:16        0     0          0       20     0       3151     0
awdl0 1452  fe80::5435: fe80:8::5435:b4ff        0     -          0       20     -       3151     -
bridg 1500  <Link#9>    7a:31:c1:eb:5a:00        0     0          0        1     0        342     0
tun0  1500  <Link#10>                         3893     0    1604324     5336     0    1195731     0
tun0  1500  192.168.2.6/3 192.168.2.6         3893     -    1604324     5336     -    1195731     -
",
    ?assertEqual({160545572,19404499}, mzb_system_load_monitor:parse_darwin_netstat_output(Output)).

