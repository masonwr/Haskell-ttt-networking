all:
	stack build
run:
	stack exec hask-echo-server-exe
test:
	stack test
