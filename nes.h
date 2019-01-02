#include "cpu.h"
#include "ppu.h"

struct nes
{
    cpu *cpu;
    ppu *ppu;
};
typedef struct nes nes;

nes *nes_create();
void nes_loadROM(nes *n, char *rom);