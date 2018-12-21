.PHONY: test

test:
	gcc -std=c11 -O3 -Wall -pedantic cpu.c test.c -o test