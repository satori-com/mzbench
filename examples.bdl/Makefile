
EXAMPLES += $(wildcard *.bdl)

check: $(EXAMPLES)
	for example in $^; do ../bin/mzbench validate $$example || exit 1; done;