#include <stdio.h>
#include <stdlib.h>
#include "nes.h"
#include "cpu.h"

int main()
{
    nes *nes = nes_create();
    nes_loadROM(nes, "nestest.nes");
    cpu_reset(nes->cpu);
    for (int x = 0; x < 26300; x++)
    {
        //cpu_printState(cpu);
        //printf("\n");
        cpu_executeCycle(nes->cpu);
    }
    while (1)
    {
        cpu_printState(nes->cpu);
        getchar();
        cpu_executeCycle(nes->cpu);
    }
    return 0;
}
