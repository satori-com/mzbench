
define DIALYZER_HELPER
import shutil
import re
import sys
report = sys.stdin.read()
warning_count = re.search(r'Warnings occured running dialyzer: (\d+)', report)
file_location = re.search(r'Warnings written to (.*\.dialyzer_warnings)', report)
ignore_list = [
	r"Expression produces a value of type 'ok' | {'error','lager_not_running'}, but this value is unmatched",

	# You're welcome to remove this and try to figure out what is dialyzer unhappy about
	r"The call cowboy_req:reply\(200,Headers"
	]
if warning_count:
	ignored_count = 0
	with open('dialyzer.log', 'w') as log:
		for line in report.split('\n'):
			for ignore_pattern in ignore_list:
				if re.search(ignore_pattern, line):
					ignored_count += 1
					break
			else:
				log.write(line + '\n')
	print 'Ignored {0} warnings'.format(ignored_count)
	if int(warning_count.group(1)) > ignored_count:
		print open('dialyzer.log').read()
		sys.exit(1)
	else:
		sys.exit(0)
else:
	with open('dialyzer.log', 'w') as f:
		f.write('')
	sys.exit(0)
endef
export DIALYZER_HELPER

dialyzer.log: .make/compilation-up-to-date
	@echo "dialyzing..."
	@$(REBAR) dialyzer | python -c "$$DIALYZER_HELPER"
