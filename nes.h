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
    uint8_t pad1_state;
    uint8_t pad2_state;
    uint8_t pad1_count;
    uint8_t pad2_count;
    uint8_t pad1_port;
    uint8_t pad2_port;
    uint8_t pad_latch;
    bool reg_access;
    bool *write;
    bool dma;
    uint8_t dma_page;
    uint8_t dma_byte;
    uint16_t dma_count;
    uint8_t ram[0x800];
    uint8_t vram[0x800];
    uint32_t palette[512];
};
typedef struct nes nes;

nes *nes_create();
bool nes_stepCycle(nes *n);
void nes_setFramebuffer(nes *n, uint32_t *framebuffer);
void nes_emulateFrame(nes *n);
void nes_loadROM(nes *n, char *rom);
void nes_updatePadState(nes *n, uint8_t state);
void nes_loadPalette(nes *n, char *palette);