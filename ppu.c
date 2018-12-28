#include "ppu.h"
#include <stdio.h>
#include <stdlib.h>

struct ppu
{
	uint16_t *address;
	uint8_t *write;
	uint8_t nmi;

    uint8_t PPUCTRL;
    uint8_t PPUMASK;
    uint8_t PPUSTATUS;
    uint8_t OAMADDR;
    uint8_t OAMDATA;
    uint8_t PPUSCROLL;
    uint8_t PPUADDR;
    uint8_t PPUDATA;
    uint8_t OAMDMA;

    uint8_t oam[0x100];
    uint8_t *memory[0x4000];
};

