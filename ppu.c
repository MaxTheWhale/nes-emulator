#include "ppu.h"
#include <stdio.h>
#include <stdlib.h>

struct ppu
{
	uint16_t *address;
	bool *write;
    bool *reg_access;
	bool nmi;
    bool odd_frame;

    uint8_t PPUCTRL;
    uint8_t PPUMASK;
    uint8_t PPUSTATUS;
    uint8_t OAMADDR;
    uint8_t OAMDATA;
    uint8_t PPUSCROLL;
    uint8_t PPUADDR;
    uint8_t PPUDATA;
    uint8_t OAMDMA;

    uint16_t dot;
    uint16_t scanline;

    uint8_t oam[0x100];
    uint8_t *memory[0x4000];
};

ppu* ppu_create()
{
    ppu *newPPU = malloc(sizeof(ppu));
    newPPU->PPUSTATUS = 0;
    return newPPU;
}

void ppu_mapMemory(ppu *p, uint16_t address, uint8_t *pointer)
{
    p->memory[address] = pointer;
}
void ppu_mapRW(ppu *p, bool *pointer)
{
    p->write = pointer;
}
void ppu_mapRegAccess(ppu *p, bool *pointer)
{
    p->reg_access = pointer;
}
void ppu_mapAddress(ppu *p, uint16_t *pointer)
{
    p->address = pointer;
}
bool* ppu_getNMI(ppu *p)
{
    return &(p->nmi);
}
uint8_t* ppu_getPPUCTRL(ppu *p)
{
    return &(p->PPUCTRL);
}
uint8_t* ppu_getPPUMASK(ppu *p)
{
    return &(p->PPUMASK);
}
uint8_t* ppu_getPPUSTATUS(ppu *p)
{
    return &(p->PPUSTATUS);
}
uint8_t* ppu_getOAMADDR(ppu *p)
{
    return &(p->OAMADDR);
}
uint8_t* ppu_getOAMDATA(ppu *p)
{
    return &(p->OAMDATA);
}
uint8_t* ppu_getPPUSCROLL(ppu *p)
{
    return &(p->PPUSCROLL);
}
uint8_t* ppu_getPPUADDR(ppu *p)
{
    return &(p->PPUADDR);
}
uint8_t* ppu_getPPUDATA(ppu *p)
{
    return &(p->PPUDATA);
}

void ppu_print(ppu *p)
{
    printf(" {%d, %d} ", p->dot, p->scanline);
}

uint16_t ppu_executeCycle(ppu *p)
{
    if (*p->reg_access)
    {
        int reg = *p->address & 0x7;
        if (reg == 2)
        {
            //p->PPUSTATUS &= 0x7f;
        }
    }
    p->dot++;
    if (p->dot > 340)
    {
        p->dot = 0;
        p->scanline++;
        if (p->scanline > 261)
        {
            p->scanline = 0;
        }
    }
    if (p->scanline == 241 && p->dot == 1)
    {
        printf("\nstart of vblank\n");
        p->PPUSTATUS |= 0x80;
    }
    if (p->scanline == 261 && p->dot == 1)
    {
        printf("\nend of vblank\n");
        p->PPUSTATUS &= 0x1f;
    }
    //ppu_print(p);
    return (p->dot + p->scanline) & 0x3f;
}

