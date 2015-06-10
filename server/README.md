MZ-Bench API Server
===================

Building
------------

```
$ make
```

Start
-----

```
$ _rel/mz_bench_api/bin/mz_bench_api console
```

Start as a service
------------------

```
$ _rel/mz_bench_api/bin/mz_bench_api start
```

Server logs api endpoint
------------------------

http://<server-address>/slogs

i.e. http://localhost:8080/slogs

Server status api endpoint
--------------------------

http://<server-address>/sstatus

http://localhost:8080/sstatus

