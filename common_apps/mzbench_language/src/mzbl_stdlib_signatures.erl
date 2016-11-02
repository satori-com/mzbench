-module(mzbl_stdlib_signatures).

-export([is_std_function/2]).

-spec is_std_function(atom(), integer()) -> boolean().
is_std_function(t, _) -> true;
is_std_function(comb, _) -> true;
is_std_function(think_time, 2) -> true;
is_std_function(ramp, 3) -> true;
is_std_function(loop, 2) -> true;
is_std_function(parallel, 1) -> true;
is_std_function(ignore_failure, 1) -> true;
is_std_function(wait, 1) -> true;
is_std_function(choose, 1) -> true;
is_std_function(choose, 2) -> true;
is_std_function(error, 1) -> true;
is_std_function(term_to_binary, 1) -> true;
is_std_function(random_binary, 1) -> true;
is_std_function(random_list, 1) -> true;
is_std_function(random_number, 1) -> true;
is_std_function(random_number, 2) -> true;
is_std_function(random_string, 1) -> true;
is_std_function(sprintf, 2) -> true;
is_std_function(var, 1) -> true;
is_std_function(var, 2) -> true;
is_std_function(numvar, 1) -> true;
is_std_function(numvar, 2) -> true;
is_std_function(seq, 2) -> true;
is_std_function(resource, 1) -> true;
is_std_function(round_robin, 1) -> true;
is_std_function(set_signal, 1) -> true;
is_std_function(set_signal, 2) -> true;
is_std_function(wait_signal, 1) -> true;
is_std_function(wait_signal, 2) -> true;
is_std_function(wait_signal, 3) -> true;
is_std_function(dump, 1) -> true;
is_std_function(_, _) -> false.
