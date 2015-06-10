
bench_loop.png: bench_loop.csv bench.plot
	./bench.plot

bench_loop.csv: bench_loop.beam counter_worker.beam ../node/apps/mz_bench/ebin/mzb_worker_runner.beam
	erl -pa ../node/apps/mz_bench/ebin\
		-pa ../node/deps/meck/ebin\
		-pa ../node/deps/folsom/ebin\
		-run bench_loop main '[]'\
		-run init stop -noshell | tee bench_loop.csv

../node/apps/mz_bench/ebin/mzb_worker_runner.beam: ../node/apps/mz_bench/src/mzb_worker_runner.erl
	make -C ../node compile

%.beam: %.erl
	erlc $<

.PHONY: clean
clean:
	- rm bench_loop.csv bench_loop.png *.beam
