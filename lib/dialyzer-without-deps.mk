
DIALYZABLE_EBINS ?= ebin

# lol
-include ../lib/otp-plt.mk
-include ../../lib/otp-plt.mk

dialyzer.log: $(HOME)/.otp.plt .make/compilation-up-to-date
	- dialyzer -nn \
		-o dialyzer.log \
		-Wno_undefined_callbacks \
		-Wunmatched_returns \
		-Werror_handling \
		-Wrace_conditions \
		--no_check_plt \
		--plt $< \
		$(DIALYZABLE_EBINS)
	-@perl -ne 'print if not /lager_not_running/' -i dialyzer.log
	-@perl -ne "print if not /The pattern {'error', UtilFailedReason} can never match the type float()/" -i dialyzer.log
	-@ cat dialyzer.log
	@python -c 'with open("dialyzer.log") as f: import sys; sys.exit(1 if f.read().strip() else 0)'