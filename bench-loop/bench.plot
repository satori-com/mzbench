#!/usr/bin/env gnuplot

set title 'mz-bench interpreter loop performance limit (loop body only increments a counter)'
set xlabel 'Expected RPS'
set ylabel 'Actual RPS'
set key bottom right

set terminal png size 1000, 600 enhanced font 'Verdana,10'
set output 'bench_loop.png'

set datafile separator ","
plot 'bench_loop.csv' u 1:2 w lines lw 2 t 'Constant rate',\
     'bench_loop.csv' u 1:3 w lines lw 2 t 'Linear ramp',\
     'bench_loop.csv' u 1:4 w lines lw 2 t 'Unbounded'

