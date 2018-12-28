.PHONY: test

test:
	gcc -std=c11 -Wall -pedantic -O2 cpu.c ppu.c test.c -o test