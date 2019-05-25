#include "ppu.h"
#include <stdio.h>
#include <stdlib.h>

struct ppu
{
	uint16_t *cpu_address;
    uint16_t address_v;
    uint16_t address_temp;
    uint8_t fine_x;
    uint8_t vram_inc;
    uint8_t *v_low;
    uint8_t *v_high;
    uint8_t *t_low;
    uint8_t *t_high;
    uint8_t dummy;
	bool *write;
    bool write_prev;
    bool *reg_access;
    bool reg_access_prev;
    bool vblank_clear;
    bool background_en;
    bool sprite_en;
    bool rendering_en;
    bool vblank;
    bool hblank;
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
    newPPU->vram_inc = 1;
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
    bool write = (*p->write && !p->write_prev);
    bool reg_access = (*p->reg_access && !p->reg_access_prev);
    if (!*p->reg_access && p->reg_access_prev)
    {
        p->vblank_clear = false;
        p->PPUSTATUS &= 0x7f;
    }
    if (reg_access)
    {
        int reg = *p->cpu_address & 0x7;
        if (reg == 0)
        {
            p->address_temp &= 0x73ff;
            p->address_temp |= ((p->PPUCTRL & 0x3) << 10);
            p->vram_inc = (p->PPUSTATUS & 0x4) ? 32 : 0;
        }
        if (reg == 1)
        {
            p->background_en = (p->PPUMASK & 0x8);
            p->sprite_en = (p->PPUMASK & 0x10);
            p->rendering_en = p->sprite_en || p->background_en;
        }
        if (reg == 2)
        {
            p->vblank_clear = true;
            p->write_latch = false;
        }
        if (reg == 5 && write)
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
        if (reg == 6 && write)
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
        if (reg == 7 && write)
        {
            *p->memory[p->address_v] = p->PPUDATA;
            p->address_v++;
            p->address_v &= 0x3fff;
        }
    }

    if (p->rendering_en && !p->vblank)
    {
        if (!p->hblank && !(p->dot % 8) && p->dot != 0)
        {
            if ((p->address_v & 0x001F) == 31) // if coarse X == 31
            {
                p->address_v &= ~0x001F;       // coarse X = 0
                p->address_v ^= 0x0400;        // switch horizontal nametable
            }
            else
            {
                p->address_v += 1;             // increment coarse X
            }
        }
        if (p->dot == 256)
        {
            if ((p->address_v & 0x7000) != 0x7000) // if fine Y < 7
            {     
                p->address_v += 0x1000;            // increment fine Y
            }
            else
            {
                p->address_v &= ~0x7000;           // fine Y = 0
                int y = (p->address_v & 0x03E0) >> 5; // let y = coarse Y
                if (y == 29)
                {
                    y = 0;                          // coarse Y = 0
                    p->address_v ^= 0x0800;         // switch vertical nametable
                }                 
                else if (y == 31)
                {
                    y = 0;                          // coarse Y = 0, nametable not switched
                }
                else
                {
                    y += 1;                         // increment coarse Y
                    p->address_v = (p->address_v & ~0x03E0) | (y << 5);  // put coarse Y back into v
                }
            }
        }
        if (p->dot == 257)
        {
            p->address_v &= 0x7be0;
            p->address_v |= (p->address_temp & 0x41f);
        }
        if (p->dot >= 280 && p->dot <= 304 && p->scanline == 261)
        {
            p->address_v &= 0x41f;
            p->address_v |= (p->address_temp & 0x7be0);
        }
    }

    p->dot++;
    if (p->dot == 257)
    {
        p->hblank = true;
    }
    if (p->dot == 321)
    {
        p->hblank = false;
    }
    if (p->dot > 340)
    {
        p->dot = 0;
        p->scanline++;
        if (p->scanline > 261)
        {
            p->scanline = 0;
        }
    }
    if (p->scanline == 240 && p->dot == 0)
    {
        p->vblank = true;
    }
    if (p->scanline == 241 && p->dot == 1)
    {
        printf("\nstart of vblank\n");
        p->PPUSTATUS |= 0x80;
    }
    if (p->scanline == 261 && p->dot == 1)
    {
        printf("\nend of vblank\n");
        p->vblank = false;
        p->PPUSTATUS &= 0x1f;
    }
    //ppu_print(p);
    p->reg_access_prev = *p->reg_access;
    p->write_prev = *p->write;
    return (p->dot + p->scanline) & 0x3f;
}

