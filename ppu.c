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
    uint8_t current_tile;
    uint8_t current_attr;
    uint8_t current_pattern_low;
    uint8_t current_pattern_high;
    uint16_t pattern_shift_low;
    uint16_t pattern_shift_high;
    uint16_t attr_shift_low;
    uint16_t attr_shift_high;
    uint16_t bg_base;
    uint16_t sprite_base;
    bool attr_latch_low;
    bool attr_latch_high;
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
    bool nmi_occurred;
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
    uint8_t palette[0x20];
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
    newPPU->dot = 340;
    newPPU->scanline = 261;
    for (int i = 0; i < 0x4000; i++)
    {
        newPPU->memory[i] = &newPPU->dummy;
    }
    for (int i = 0; i < 0x100; i++)
    {
        newPPU->memory[0x3f00 + i] = &newPPU->palette[i % 0x20];
    }
    
    return newPPU;
}

void ppu_mapMemory(ppu *p, uint16_t address, uint8_t *pointer)
{
    if (address < 0x3f00)
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
uint16_t* ppu_getDot(ppu *p)
{
    return &(p->dot);
}
uint16_t* ppu_getScanline(ppu *p)
{
    return &(p->scanline);
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

uint16_t getNametableAddr(uint16_t v)
{
    return 0x2000 + (v & 0xfff);
}

uint16_t getAttributeAddr(uint16_t v)
{
    return 0x23c0 + ((v & 0xc00) | (v & 0x1c) >> 2 | (v & 0x380) >> 4);
}

uint16_t getPatternAddr(uint8_t tile, uint16_t v, bool upper, bool right)
{
    uint16_t result = 0;
    if (right) result |= 0x1000;
    if (upper) result |= 0x8;
    result |= (tile << 4);
    result |= ((v & 0x7000) >> 12);
    //printf("tile: %x, result: %x", tile, result);
    return result;
}

uint16_t calcPixel(ppu *p)
{
    uint8_t pallete_index = 0;
    if (p->pattern_shift_low & 0x8000) pallete_index |= 1;
    if (p->pattern_shift_high & 0x8000) pallete_index |= 2;
    if (pallete_index != 0)
    {
        if (p->attr_shift_low & 0x80) pallete_index |= 4;
        if (p->attr_shift_high & 0x80) pallete_index |= 8;
    }
    return p->palette[pallete_index];
}

void ppu_print(ppu *p)
{
    printf(" {%d, %d} v=%x t=%x\n", p->dot, p->scanline, p->address_v & 0x7fff, p->address_temp & 0x7fff);
}

uint16_t ppu_executeCycle(ppu *p)
{
    p->dot++;
    if (p->dot > 340)
    {
        p->dot = 0;
        p->scanline++;
        if (p->scanline > 261)
        {
            if (p->odd_frame)
            {
                p->dot = 1;
                p->odd_frame = false;
            }
            else
                p->odd_frame = true;
            p->scanline = 0;
        }
    }

    if (p->dot == 257)
    {
        p->hblank = true;
    }
    if (p->dot == 321)
    {
        p->hblank = false;
    }
    if (p->scanline == 240 && p->dot == 0)
    {
        p->vblank = true;
    }
    if (p->scanline == 241 && p->dot == 1)
    {
        printf("\nstart of vblank\n");
        p->nmi_occurred = true;
        p->PPUSTATUS |= 0x80;
    }
    if (p->scanline == 261 && p->dot == 1)
    {
        printf("\nend of vblank\n");
        p->vblank = false;
        p->nmi_occurred = false;
        p->PPUSTATUS &= 0x1f;
    }

    bool write = (*p->write && !p->write_prev);
    bool reg_access = (*p->reg_access && !p->reg_access_prev);

    p->nmi = (p->nmi_occurred && (p->PPUCTRL & 0x80));

    if (!*p->reg_access && p->reg_access_prev)
    {
        p->vblank_clear = false;
        p->PPUSTATUS &= 0x7f;
    }
    if (reg_access)
    {
        int reg = *p->cpu_address & 0x7;
        if (reg == 0 && write)
        {
            p->address_temp &= 0x73ff;
            p->address_temp |= ((p->PPUCTRL & 0x3) << 10);
            p->vram_inc = (p->PPUCTRL & 0x4) ? 32 : 1;
            p->sprite_base = (p->PPUCTRL & 0x8) ? 0x1000 : 0;
            p->bg_base = (p->PPUCTRL & 0x10) ? 0x1000 : 0;
        }
        if (reg == 1 && write)
        {
            p->background_en = (p->PPUMASK & 0x8);
            p->sprite_en = (p->PPUMASK & 0x10);
            p->rendering_en = p->sprite_en || p->background_en;
        }
        if (reg == 2 && !write)
        {
            p->vblank_clear = true;
            p->nmi_occurred = false;
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
            p->address_v += p->vram_inc;
            p->address_v &= 0x3fff;
        }
    }

    if (p->rendering_en && !p->vblank)
    {
        if (!p->hblank && p->dot != 0)
        {
            p->pattern_shift_low <<= 1;
            p->pattern_shift_high <<= 1;
            p->attr_shift_low <<= 1;
            p->attr_shift_high <<= 1;
            if (p->attr_latch_low) p->attr_shift_low++;
            if (p->attr_latch_high) p->attr_shift_high++;
            if (p->dot % 8 == 0)
            {
                p->current_pattern_high = *p->memory[getPatternAddr(p->current_tile, p->address_v, true, p->bg_base)];
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
            if (p->dot % 8 == 1)
            {
                p->pattern_shift_low &= 0xff00;
                p->pattern_shift_high &= 0xff00;
                p->pattern_shift_low |= p->current_pattern_low;
                p->pattern_shift_high |= p->current_pattern_high;

                uint8_t attr = p->current_attr;
                if ((p->address_v - 1) & 0x02) attr >>= 2;
                if (p->address_v & 0x40) attr >>= 4;
                p->attr_latch_low = (attr & 1) ? true : false;
                p->attr_latch_high = (attr & 2) ? true : false;
            }
            if (p->dot % 8 == 2)
            {
                p->current_tile = *p->memory[getNametableAddr(p->address_v)];
            }
            if (p->dot % 8 == 4)
            {
                p->current_attr = *p->memory[getAttributeAddr(p->address_v)];
            }
            if (p->dot % 8 == 6)
            {
                p->current_pattern_low = *p->memory[getPatternAddr(p->current_tile, p->address_v, false, p->bg_base)];
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

    //ppu_print(p);
    p->reg_access_prev = *p->reg_access;
    p->write_prev = *p->write;

    return (p->dot < 256 && p->scanline < 240) ? calcPixel(p) : 0xffff;
}

