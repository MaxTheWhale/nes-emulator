#include "ppu.h"
#include <stdio.h>
#include <stdlib.h>

enum { PPUCTRL, PPUMASK, PPUSTATUS, OAMADDR, OAMDATA, PPUSCROLL, PPUADDR, PPUDATA };
enum { FINE_Y      = 0x7000, 
       NAMETABLE   = 0x0c00,
       NAMETABLE_Y = 0x0800,
       NAMETABLE_X = 0x0400,
       COARSE_Y    = 0x03e0,
       COARSE_X    = 0x001f,
       ATTR_Y      = 0x0380,
       ATTR_X      = 0x001c };

struct ppu
{
	uint16_t *cpu_address;
    uint16_t address_v;
    uint16_t address_temp;
    uint8_t fine_x;
    uint8_t vram_inc;
    uint8_t dummy;
    uint8_t current_tile;
    uint8_t current_attr;
    uint8_t current_pattern_low;
    uint8_t current_pattern_high;
    uint16_t pattern_shift_low;
    uint16_t pattern_shift_high;
    uint8_t sprite_shift_low[8];
    uint8_t sprite_shift_high[8];
    uint8_t sprite_counter[8];
    uint16_t attr_shift_low;
    uint16_t attr_shift_high;
    uint16_t bg_base;
    uint16_t sprite_base;
    bool attr_latch_low;
    bool attr_latch_high;
    bool sprite_latch[8];
	bool *write;
    bool write_prev;
    bool *reg_access;
    bool reg_access_prev;
    bool vblank_clear;
    bool background_en;
    bool sprite_en;
    bool rendering_en;
    bool vblank;
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

    uint16_t dot;
    uint16_t scanline;

    uint8_t oam[0x100];
    uint8_t secondary_oam[0x20];
    uint8_t palette[0x20];
    uint8_t *memory[0x4000];
};

ppu* ppu_create()
{
    ppu *newPPU = malloc(sizeof(ppu));
    newPPU->PPUSTATUS = 0;
    newPPU->vram_inc = 1;
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
    for (int i = 0; i < 8; i++)
    {
        newPPU->memory[0x3f00 + (i*4)] = newPPU->memory[0x3f00 + ((i*4) & ~0x10)];
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
    return 0x2000 + (v & (NAMETABLE | COARSE_Y | COARSE_X));
}

uint16_t getAttributeAddr(uint16_t v)
{
    return 0x23c0 + ((v & NAMETABLE) | (v & ATTR_X) >> 2 | (v & ATTR_Y) >> 4);
}

uint16_t getPatternAddr(ppu *p)
{
    uint16_t result = 0;
    result += p->bg_base;
    result |= (p->current_tile << 4);
    result |= ((p->address_v & FINE_Y) >> 12);
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
    if ((pallete_index & 0x13) == 0x10) pallete_index &= ~0x10;
    return p->palette[pallete_index];
}

void ppu_print(ppu *p)
{
    printf(" {%d, %d} v=%x t=%x\n", p->dot, p->scanline, p->address_v & 0x7fff, p->address_temp & 0x7fff);
}

void checkRegisters(ppu *p)
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
        if (write)
        {
            switch (reg)
            {
            case PPUCTRL:
                p->address_temp &= ~NAMETABLE;
                p->address_temp |= ((p->PPUCTRL & 0x3) << 10);
                p->vram_inc = (p->PPUCTRL & 0x4) ? 32 : 1;
                p->sprite_base = (p->PPUCTRL & 0x8) ? 0x1000 : 0;
                p->bg_base = (p->PPUCTRL & 0x10) ? 0x1000 : 0;
                break;

            case PPUMASK:
                p->background_en = (p->PPUMASK & 0x8);
                p->sprite_en = (p->PPUMASK & 0x10);
                p->rendering_en = p->sprite_en || p->background_en;
                break;

            case PPUSCROLL:
                if (p->write_latch)
                {
                    p->address_temp &= ~(FINE_Y | COARSE_Y);
                    p->address_temp |= (p->PPUSCROLL << 12);
                    p->address_temp |= ((p->PPUSCROLL & 0xf8) << 2);
                    p->write_latch = false;
                }
                else 
                {
                    p->address_temp &= ~COARSE_X;
                    p->address_temp |= (p->PPUSCROLL >> 3);
                    p->fine_x = p->PPUSCROLL & 0x7;
                    p->write_latch = true;
                }
                break;

            case PPUADDR:
                if (p->write_latch)
                {
                    p->address_temp &= 0xff00;
                    p->address_temp |= p->PPUADDR;
                    p->address_v = p->address_temp;
                    p->write_latch = false;
                }
                else 
                {
                    p->address_temp &= 0x00ff;
                    p->address_temp |= (p->PPUADDR & 0x3f) << 8;
                    p->write_latch = true;
                }
                break;

            case PPUDATA:
                *p->memory[p->address_v & 0x3fff] = p->PPUDATA;
                p->address_v += p->vram_inc;
                break;
            }
        }
        else
        {
            if (reg == PPUDATA)
            {
                p->vblank_clear = true;
                p->nmi_occurred = false;
                p->write_latch = false;
            }
        }
    }
}

void perform_background_fetches(ppu *p)
{
    switch (p->dot % 8)
    {
    case 2:
        p->current_tile = *p->memory[getNametableAddr(p->address_v)];
        break;
    case 4:
        p->current_attr = *p->memory[getAttributeAddr(p->address_v)];
        break;
    case 6:
        p->current_pattern_low = *p->memory[getPatternAddr(p)];
        break;
    case 0:
        p->current_pattern_high = *p->memory[getPatternAddr(p) + 8];
        break;
    }
}

void update_shift_registers(ppu *p)
{
    p->pattern_shift_low <<= 1;
    p->pattern_shift_high <<= 1;
    p->attr_shift_low <<= 1;
    p->attr_shift_high <<= 1;
    if (p->attr_latch_low) p->attr_shift_low++;
    if (p->attr_latch_high) p->attr_shift_high++;

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
}

void increment_horizontal_v(ppu *p)
{
    if ((p->address_v & COARSE_X) == 31)
    {
        p->address_v &= ~COARSE_X;
        p->address_v ^= NAMETABLE_X;
    }
    else
    {
        p->address_v += 1;
    }
}

void increment_vertical_v(ppu *p)
{
    if ((p->address_v & FINE_Y) < 0x7000)
    {     
        p->address_v += 0x1000;
    }
    else
    {
        p->address_v &= ~FINE_Y;
        int y = (p->address_v & COARSE_Y) >> 5;
        if (y == 29)
        {
            y = 0;
            p->address_v ^= NAMETABLE_Y;
        }                 
        else if (y == 31)
        {
            y = 0;
        }
        else
        {
            y += 1;
            p->address_v = (p->address_v & ~COARSE_Y) | (y << 5);
        }
    }
}

void incrementDot(ppu *p)
{
    if (++p->dot > 340)
    {
        p->dot = 0;
        if (++p->scanline > 261)
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
}

uint16_t ppu_executeCycle(ppu *p)
{
    incrementDot(p);

    if (p->scanline == 240 && p->dot == 0)
    {
        p->vblank = true;
    }
    if (p->scanline == 241 && p->dot == 1)
    {
        p->nmi_occurred = true;
        p->PPUSTATUS |= 0x80;
    }
    if (p->scanline == 261 && p->dot == 1)
    {
        p->vblank = false;
        p->nmi_occurred = false;
        p->PPUSTATUS &= 0x1f;
    }

    p->nmi = (p->nmi_occurred && (p->PPUCTRL & 0x80));

    checkRegisters(p);

    if (p->rendering_en && !p->vblank)
    {
        if ((p->dot >= 2 && p->dot <= 257) || (p->dot >= 322 && p->dot <= 337))
        {
            update_shift_registers(p);
        }
        if ((p->dot >= 1 && p->dot <= 256) || (p->dot >= 321 && p->dot <= 336))
        {
            perform_background_fetches(p);
            if (p->dot % 8 == 0)
            {
                increment_horizontal_v(p);
            }
        }
        if (p->dot == 256)
        {
            increment_vertical_v(p);
        }
        if (p->dot == 257)
        {
            p->address_v &= ~(NAMETABLE_X | COARSE_X);
            p->address_v |= (p->address_temp & (NAMETABLE_X | COARSE_X));
        }
        if (p->dot >= 280 && p->dot <= 304 && p->scanline == 261)
        {
            p->address_v &= ~(FINE_Y | NAMETABLE_Y | COARSE_Y);
            p->address_v |= (p->address_temp & (FINE_Y | NAMETABLE_Y | COARSE_Y));
        }
    }

    p->reg_access_prev = *p->reg_access;
    p->write_prev = *p->write;

    return (p->dot < 257 && p->dot > 0 && p->scanline < 240) ? calcPixel(p) : 0xffff;
}

