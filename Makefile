.PHONY: test

test:
	gcc -std=c11 -Wall -pedantic -O2 cpu.c test.c -o test