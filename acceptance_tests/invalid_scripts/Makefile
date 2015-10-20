
EXAMPLES := $(wildcard *.erl)
EXPECTED_ANSWERS := $(EXAMPLES:%.erl=%.expected_answer)
GOT_ANSWERS := $(EXAMPLES:%.erl=%.got_answer)
MATCHES := $(EXAMPLES:%.erl=%.match)

check: $(MATCHES)
	@rm -rf data* log* || true
	@rm -f *.got_answer || true
	@rm -f *.expected_answer || true
	@rm -f *.match || true
	@echo ok

%.match: %.got_answer %.expected_answer
	diff $^ && touch $@

%.expected_answer: %.expected_answer_template
	@sed -e "s#SCRIPT_NAME#$(abspath $*.erl)#" $< > $@

%.got_answer: %.erl
	-@../../bin/mzbench validate $< > $@ || true

clean:
	@rm -rf data.* log.* || true
	@rm *.got_answer || true
	@rm *.expected_answer || true
	@rm *.match || true