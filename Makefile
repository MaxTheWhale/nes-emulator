.PHONY: test

test:
	gcc -std=c11 -Wall -Wno-format -pedantic -O2 cpu.c ppu.c apu.c nes.c test.c -lSDL2 -o test