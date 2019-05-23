#include "cpu.h"
#include "ppu.h"

struct nes
{
    cpu *cpu;
    ppu *ppu;
    uint8_t ram[0x800];
    uint32_t palette[64];
};
typedef struct nes nes;

nes *nes_create();
void nes_emulateFrame(nes *n, uint32_t *framebuffer);
void nes_loadROM(nes *n, char *rom);
void nes_loadPalette(nes *n, char *palette);