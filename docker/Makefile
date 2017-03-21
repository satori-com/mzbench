.PHONY: all
all: image run

.PHONY: image
image:
	docker build . -t mzbench

.PHONY: run
run:
	docker run -d -p 4800:80 --name mzbench_server mzbench
