#include <stdio.h>
#include <stdlib.h>
#include "cpu.h"

int main()
{
    cpu *cpu = cpu_create();
    cpu_loadROM(cpu, "nestest.nes");
    cpu_reset(cpu);
    for (int x = 0; x < 15810; x++)
    {
        //cpu_printState(cpu);
        //printf("\n");
        cpu_executeCycle(cpu);
    }
    while (1)
    {
        cpu_printState(cpu);
        getchar();
        cpu_executeCycle(cpu);
    }
    return 0;
}