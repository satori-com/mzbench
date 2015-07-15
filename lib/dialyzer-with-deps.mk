
DIALYZABLE_EBINS ?= ebin

# lol
-include ../lib/otp-plt.mk
-include ../../lib/otp-plt.mk

deps.plt: .make/deps-compiled
	- dialyzer --output_plt $@ --build_plt $(BUILD_PLT_FLAGS) -pa deps/*/ebin

dialyzer.log: deps.plt $(HOME)/.otp.plt .make/compilation-up-to-date
	- dialyzer -nn \
		-o dialyzer.log \
		-Wno_undefined_callbacks \
		-Wunmatched_returns \
		-Werror_handling \
		-Wrace_conditions \
		--no_check_plt \
		--plts $< $(HOME)/.otp.plt -- \
		$(DIALYZABLE_EBINS)
	-@perl -ne 'print if not /lager_not_running/' -i dialyzer.log
	-@perl -ne "print if not /The pattern {'error', UtilFailedReason} can never match the type float()/" -i dialyzer.log
	-@ cat dialyzer.log
	@python -c 'with open("dialyzer.log") as f: import sys; sys.exit(1 if f.read().strip() else 0)'

.PHONY: clean-plt
clean-plt:
	-@rm *.plt