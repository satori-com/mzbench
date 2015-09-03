
DIALYZABLE_EBINS ?= ebin
DEPS_PLT ?= deps.plt


$(HOME)/.otp.plt:
	dialyzer --output_plt $@ --build_plt --apps erts stdlib kernel crypto os_mon inets

deps.plt: .make/compilation-up-to-date
	- dialyzer --output_plt $@ --build_plt $(BUILD_PLT_FLAGS) -pa deps/*/ebin

dialyzer.log: $(DEPS_PLT) $(HOME)/.otp.plt .make/compilation-up-to-date
	- dialyzer -nn \
		-o dialyzer.log \
		-Wno_undefined_callbacks \
		-Wunmatched_returns \
		-Werror_handling \
		-Wrace_conditions \
		--no_check_plt \
		--plts $(DEPS_PLT) $(HOME)/.otp.plt -- \
		"$(DIALYZABLE_EBINS)"
	-@perl -ne 'print if not /lager_not_running/' -i dialyzer.log
	-@perl -ne 'print if not /Unknown types/' -i dialyzer.log
	-@perl -ne 'print if not /erl_syntax:syntaxTree/' -i dialyzer.log
	-@perl -ne 'print if not /The call cowboy_req:reply\(200,Headers/' -i dialyzer.log
	-@perl -ne 'print if not /mzb_py/' -i dialyzer.log
	-@perl -ne "print if not /The pattern {'error', UtilFailedReason} can never match the type float()/" -i dialyzer.log
	-@ cat dialyzer.log
	@python -c 'with open("dialyzer.log") as f: import sys; sys.exit(1 if f.read().strip() else 0)'

.PHONY: clean-plt
clean-plt:
	-@rm *.plt
