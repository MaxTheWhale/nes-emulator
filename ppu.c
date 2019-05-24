#include "ppu.h"
#include <stdio.h>
#include <stdlib.h>

struct ppu
{
	uint16_t *cpu_address;
    uint16_t address_v;
    uint16_t address_temp;
    uint8_t fine_x;
    uint8_t *v_low;
    uint8_t *v_high;
    uint8_t *t_low;
    uint8_t *t_high;
    uint8_t dummy;
	bool *write;
    bool *reg_access;
	bool nmi;
    bool odd_frame;
    bool write_latch;

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
    newPPU->v_low = (uint8_t*)&newPPU->address_v;
    newPPU->v_high = newPPU->v_low + 1;
    newPPU->t_low = (uint8_t*)&newPPU->address_temp;
    newPPU->t_high = newPPU->t_low + 1;
    for (int i = 0; i < 0x4000; i++)
    {
        newPPU->memory[i] = &newPPU->dummy;
    }
    
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
    p->cpu_address = pointer;
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
        int reg = *p->cpu_address & 0x7;
        if (reg == 2)
        {
            //p->PPUSTATUS &= 0x7f;
            p->write_latch = false;
        }
        if (reg == 5 && p->write)
        {
            if (p->write_latch)
            {
                p->address_temp &= 0xc1f;
                p->address_temp |= (p->PPUSCROLL << 12);
                p->address_temp |= ((p->PPUSCROLL & 0xf8) << 2);
                p->write_latch = false;
            }
            else 
            {
                p->address_temp &= 0x7fe0;
                p->address_temp |= (p->PPUSCROLL >> 3);
                p->fine_x = p->PPUSCROLL & 0x7;
                p->write_latch = true;
            }
        }
        if (reg == 6 && p->write)
        {
            if (p->write_latch)
            {
                *p->t_low = p->PPUADDR;
                p->address_v = p->address_temp;
                p->write_latch = false;
            }
            else 
            {
                *p->t_high = p->PPUADDR & 0x3f;
                p->write_latch = true;
            }
        }
        if (reg == 7 && p->write)
        {
            *p->memory[p->address_v] = p->PPUDATA;
            p->address_v++;
            p->address_v &= 0x3fff;
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

