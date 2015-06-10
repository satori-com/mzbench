[ % error function generates execution error with specified message
	{pool, [{size, 1},
	        {worker_type, dummy_worker}], [
		{error, "waldo"}]}
].
% should output shomething like this among other log messages:
% 17:56:11.988 [error] <0.186.0> Worker <0.190.0> on 'mzb_...@...' has crashed: "waldo"
