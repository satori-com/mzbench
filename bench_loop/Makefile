
bench_loop.png: bench_loop.csv bench.plot
	./bench.plot

bench_loop.csv: bench_loop.beam counter_worker.beam ../node/apps/mzbench/ebin/mzb_worker_runner.beam
	erl -pa ../node/_build/default/deps/mzbench/ebin\
		-pa ../node/_build/default/deps/mzbench_language/ebin\
		-pa ../node/_build/default/deps/meck/ebin\
		-pa ../node/_build/default/deps/folsom/ebin\
		-run bench_loop main '[]'\
		-run init stop -noshell | tee bench_loop.csv

../node/apps/mzbench/ebin/mzb_worker_runner.beam: ../node/apps/mzbench/src/mzb_worker_runner.erl
	make -C ../node compile

%.beam: %.erl
	erlc $<

.PHONY: clean
clean:
	- rm bench_loop.csv bench_loop.png *.beam
