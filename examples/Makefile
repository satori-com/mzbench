
EXAMPLES += $(filter-out resource_data.erl, $(wildcard *.erl))

check: $(EXAMPLES)
	for example in $^; do ../bin/mzbench validate $$example || exit 1; done;