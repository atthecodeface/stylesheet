all: install

.PHONY:test
test:
	jbuilder build @run_test

.PHONY:clean
clean:
	jbuilder clean

install:
	jbuilder build @install
	jbuilder install
