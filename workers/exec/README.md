# Command executing worker for MZBench

Provides only `execute` function. This worker is useful when you have some external program which you want to run and benchmark. This worker starts a command, forwards output to MZBench logging subsystem, counts failures and latencies. If you need more advanced communication with external binary please refer to [TCPKali](/workers/tcpkali) worker.

## execute

	execute("ls -la")

[Full example](examples/exec.bdl)