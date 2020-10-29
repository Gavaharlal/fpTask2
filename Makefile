all: run-test

run-app:
	stack build
	stack exec fpTask2-exe& stack exec fpTask2-front
	pkill fpTask2-exe

run-test:
	stack build
	stack exec fpTask2-exe& stack test
	pkill fpTask2-exe