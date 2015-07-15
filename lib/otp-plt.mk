
$(HOME)/.otp.plt:
	dialyzer --output_plt $@ --build_plt --apps erts stdlib kernel crypto syntax_tools os_mon inets
