#include "cpu.h"
#include "ppu.h"

struct nes
{
    cpu *cpu;
    ppu *ppu;
    uint16_t *address;
    uint16_t *dot;
    uint16_t *scanline;
    uint32_t *framebuffer;
    bool reg_access;
    uint8_t ram[0x800];
    uint32_t palette[512];
};
typedef struct nes nes;

nes *nes_create();
bool nes_stepCycle(nes *n);
void nes_setFramebuffer(nes *n, uint32_t *framebuffer);
void nes_emulateFrame(nes *n);
void nes_loadROM(nes *n, char *rom);
void nes_loadPalette(nes *n, char *palette);