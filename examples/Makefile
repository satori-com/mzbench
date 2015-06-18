
EXAMPLES += $(wildcard *.erl)

check: $(EXAMPLES)
	for example in $^; do ../bin/mzbench validate $$example; done;