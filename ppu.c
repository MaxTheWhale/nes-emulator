#include "ppu.h"
#include <stdio.h>
#include <stdlib.h>

// Registers
enum { PPUCTRL, PPUMASK, PPUSTATUS, OAMADDR, OAMDATA, PPUSCROLL, PPUADDR, PPUDATA };

// PPUMASK bitmasks
enum {
    GREYSCALE   = 0x01,
    LEFT_BG     = 0x02,
    LEFT_SPRITE = 0x04,
    BG_EN       = 0x08,
    SPRITE_EN   = 0x10,
    RENDER_EN   = 0x18,
    R_EMPHASIS  = 0x20,
    G_EMPHASIS  = 0x40,
    B_EMPHASIS  = 0x80
};

// v/t register bitmasks
enum {
    FINE_Y      = 0x7000,
    NAMETABLE   = 0x0c00,
    NAMETABLE_Y = 0x0800,
    NAMETABLE_X = 0x0400,
    COARSE_Y    = 0x03e0,
    COARSE_X    = 0x001f,
    ATTR_Y      = 0x0380,
    ATTR_X      = 0x001c
};

struct ppu {
    // External signals
    uint16_t* cpu_address;
    uint16_t ppu_address;
    bool* write;
    bool nmi;

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
    uint8_t sprite_offset[8];
    uint8_t eval_n;
    uint8_t eval_m;
    uint8_t eval_sn;
    uint8_t sprites_on_line;
    uint8_t eval_temp;
    uint8_t eval_stage;
    uint16_t attr_shift_low;
    uint16_t attr_shift_high;
    uint16_t bg_base;
    uint16_t sprite_base;
    bool attr_latch_low;
    bool attr_latch_high;
    bool sprite_latch_low[8];
    bool sprite_latch_high[8];
    bool sprite_priority[8];
    bool write_prev;
    bool* reg_access;
    bool reg_access_prev;
    bool status_read;
    bool data_read;
    bool vblank;
    bool nmi_occurred;
    bool odd_frame;
    bool write_latch;
    bool sprite0_this_line;
    bool sprite0_next_line;

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
    uint8_t* memory[0x4000];
};

ppu* ppu_create() {
    ppu* newPPU = malloc(sizeof(ppu));
    newPPU->PPUSTATUS = 0;
    newPPU->vram_inc = 1;
    newPPU->dot = 340;
    newPPU->scanline = 261;
    for (int i = 0; i < 0x4000; i++) {
        newPPU->memory[i] = &newPPU->dummy;
    }
    for (int i = 0; i < 0x100; i++) {
        newPPU->memory[0x3f00 + i] = &newPPU->palette[i % 0x20];
    }
    for (int i = 0; i < 0x40; i++) {
        newPPU->memory[0x3f00 + (i * 4)] = newPPU->memory[0x3f00 + ((i * 4) & ~0x10)];
    }
    return newPPU;
}

void ppu_mapMemory(ppu* p, uint16_t address, uint8_t* start, uint16_t size, uint16_t mirrors) {
    for (int m = 0; m < mirrors; m++) {
        for (int n = 0; n < size; n++) {
            if (address + (m * size) + n < 0x3f00)
                p->memory[address + (m * size) + n] = start + n;
        }
    }
}
void ppu_mapRW(ppu* p, bool* pointer) {
    p->write = pointer;
}
void ppu_mapRegAccess(ppu* p, bool* pointer) {
    p->reg_access = pointer;
}
void ppu_mapAddress(ppu* p, uint16_t* pointer) {
    p->cpu_address = pointer;
}
bool* ppu_getNMI(ppu* p) {
    return &(p->nmi);
}
uint16_t* ppu_getDot(ppu* p) {
    return &(p->dot);
}
uint16_t* ppu_getScanline(ppu* p) {
    return &(p->scanline);
}
uint8_t* ppu_getPPUCTRL(ppu* p) {
    return &(p->PPUCTRL);
}
uint8_t* ppu_getPPUMASK(ppu* p) {
    return &(p->PPUMASK);
}
uint8_t* ppu_getPPUSTATUS(ppu* p) {
    return &(p->PPUSTATUS);
}
uint8_t* ppu_getOAMADDR(ppu* p) {
    return &(p->OAMADDR);
}
uint8_t* ppu_getOAMDATA(ppu* p) {
    return &(p->OAMDATA);
}
uint8_t* ppu_getPPUSCROLL(ppu* p) {
    return &(p->PPUSCROLL);
}
uint8_t* ppu_getPPUADDR(ppu* p) {
    return &(p->PPUADDR);
}
uint8_t* ppu_getPPUDATA(ppu* p) {
    return &(p->PPUDATA);
}

uint8_t readMemory(ppu* p, uint16_t addr) {
    p->ppu_address = addr & 0x3fff;
    return *p->memory[p->ppu_address];
}

void writeMemory(ppu* p, uint16_t addr, uint8_t data) {
    p->ppu_address = addr & 0x3fff;
    *p->memory[p->ppu_address] = data;
}

uint16_t getNametableAddr(uint16_t v) {
    return 0x2000 + (v & (NAMETABLE | COARSE_Y | COARSE_X));
}

uint16_t getAttributeAddr(uint16_t v) {
    return 0x23c0 + ((v & NAMETABLE) | (v & ATTR_X) >> 2 | (v & ATTR_Y) >> 4);
}

uint16_t getPatternAddr(ppu* p) {
    uint16_t result = 0;
    result += p->bg_base;
    result |= (p->current_tile << 4);
    result |= ((p->address_v & FINE_Y) >> 12);
    return result;
}

uint16_t getSpriteAddr(ppu* p) {
    uint16_t result = 0;
    result += p->sprite_base;
    result |= (p->secondary_oam[p->eval_sn * 4 + 1] << 4);
    uint8_t y_offset = (uint8_t)(p->scanline - p->secondary_oam[p->eval_sn * 4]);
    if (p->secondary_oam[p->eval_sn * 4 + 2] & 0x80)
        y_offset = 7 - y_offset;
    result |= y_offset;
    return result;
}

uint8_t flipByte(uint8_t data) {
    uint8_t new = 0;
    for (int i = 0; i < 8; i++) {
        if (data & (0x80 >> i))
            new |= (0x1 << i);
    }
    return new;
}

uint16_t calcPixel(ppu* p) {
    uint8_t bg_palette = 0;
    if ((p->PPUMASK & BG_EN) && !(!(p->PPUMASK & LEFT_BG) && p->dot <= 8)) {
        if (p->pattern_shift_low & (0x8000 >> p->fine_x))
            bg_palette |= 1;
        if (p->pattern_shift_high & (0x8000 >> p->fine_x))
            bg_palette |= 2;
        if (bg_palette > 0) {
            if (p->attr_shift_low & (0x80 >> p->fine_x))
                bg_palette |= 4;
            if (p->attr_shift_high & (0x80 >> p->fine_x))
                bg_palette |= 8;
        }
    }
    if ((p->PPUMASK & SPRITE_EN) && !(!(p->PPUMASK & LEFT_SPRITE) && p->dot <= 8) &&
        p->scanline > 0) {
        uint8_t sprite_palette = 0x10;
        for (int i = p->sprites_on_line - 1; i >= 0; i--) {
            if (p->sprite_counter[i] == 0) {
                uint8_t pattern = 0;
                if (p->sprite_shift_low[i] & 0x80)
                    pattern |= 1;
                if (p->sprite_shift_high[i] & 0x80)
                    pattern |= 2;
                if (pattern > 0) {
                    sprite_palette = 0x10;
                    sprite_palette |= pattern;
                    if (p->sprite_latch_low[i])
                        sprite_palette |= 4;
                    if (p->sprite_latch_high[i])
                        sprite_palette |= 8;
                    if (bg_palette > 0) {
                        if (i == 0 && p->sprite0_this_line && p->dot <= 255)
                            p->PPUSTATUS |= 0x40;
                        if (p->sprite_priority[i])
                            sprite_palette = 0x10;
                    }
                }
            }
        }
        if (sprite_palette > 0x10)
            bg_palette = sprite_palette;
    }
    return p->palette[bg_palette];
}

void ppu_print(ppu* p) {
    printf(" {%d, %d} v=%x t=%x\n", p->dot, p->scanline, p->address_v & 0x7fff,
           p->address_temp & 0x7fff);
}

void checkRegisters(ppu* p) {
    bool write = (*p->write && !p->write_prev);
    bool reg_access = (*p->reg_access && !p->reg_access_prev);
    if (!*p->reg_access && p->reg_access_prev) {
        if (p->status_read) {
            p->status_read = false;
            p->PPUSTATUS &= 0x7f;
        }
        if (p->data_read) {
            p->data_read = false;
            p->PPUDATA = readMemory(p, p->address_v);
        }
    }
    if (reg_access) {
        int reg = *p->cpu_address & 0x7;
        if (write) {
            switch (reg) {
                case PPUCTRL:
                    p->address_temp &= ~NAMETABLE;
                    p->address_temp |= ((p->PPUCTRL & 0x3) << 10);
                    p->vram_inc = (p->PPUCTRL & 0x4) ? 32 : 1;
                    p->sprite_base = (p->PPUCTRL & 0x8) ? 0x1000 : 0;
                    p->bg_base = (p->PPUCTRL & 0x10) ? 0x1000 : 0;
                    break;

                case OAMDATA:
                    p->oam[p->OAMADDR] = p->OAMDATA;
                    p->OAMADDR++;
                    break;

                case PPUSCROLL:
                    if (p->write_latch) {
                        p->address_temp &= ~(FINE_Y | COARSE_Y);
                        p->address_temp |= (p->PPUSCROLL << 12);
                        p->address_temp |= ((p->PPUSCROLL & 0xf8) << 2);
                        p->write_latch = false;
                    } else {
                        p->address_temp &= ~COARSE_X;
                        p->address_temp |= (p->PPUSCROLL >> 3);
                        p->fine_x = p->PPUSCROLL & 0x7;
                        p->write_latch = true;
                    }
                    break;

                case PPUADDR:
                    if (p->write_latch) {
                        p->address_temp &= 0xff00;
                        p->address_temp |= p->PPUADDR;
                        p->address_v = p->address_temp;
                        if (p->address_v >= 0x3f00)
                            p->PPUDATA = readMemory(p, p->address_v);
                        p->write_latch = false;
                    } else {
                        p->address_temp &= 0x00ff;
                        p->address_temp |= (p->PPUADDR & 0x3f) << 8;
                        p->write_latch = true;
                    }
                    break;

                case PPUDATA:
                    writeMemory(p, p->address_v, p->PPUDATA);
                    p->address_v += p->vram_inc;
                    break;
            }
        } else {
            switch (reg) {
                case PPUSTATUS:
                    p->status_read = true;
                    p->nmi_occurred = false;
                    p->write_latch = false;
                    break;

                case PPUDATA:
                    p->PPUDATA = readMemory(p, p->address_v);
                    p->address_v += p->vram_inc;
                    if (p->address_v >= 0x3f00)
                        p->PPUDATA = readMemory(p, p->address_v);
                    break;
            }
        }
    }
}

void perform_background_fetches(ppu* p) {
    switch (p->dot % 8) {
        case 2:
            p->current_tile = readMemory(p, getNametableAddr(p->address_v));
            break;
        case 4:
            p->current_attr = readMemory(p, getAttributeAddr(p->address_v));
            break;
        case 6:
            p->current_pattern_low = readMemory(p, getPatternAddr(p));
            break;
        case 0:
            p->current_pattern_high = readMemory(p, getPatternAddr(p) + 8);
            break;
    }
}

void perform_sprite_fetches(ppu* p) {
    switch (p->dot % 8) {
        case 2:
            p->current_tile = readMemory(p, getNametableAddr(p->address_v));
            break;
        case 4:
            p->current_attr = readMemory(p, getAttributeAddr(p->address_v));
            p->sprite_latch_low[p->eval_sn] =
                (p->secondary_oam[p->eval_sn * 4 + 2] & 1) ? true : false;
            p->sprite_latch_high[p->eval_sn] =
                (p->secondary_oam[p->eval_sn * 4 + 2] & 2) ? true : false;
            p->sprite_priority[p->eval_sn] =
                (p->secondary_oam[p->eval_sn * 4 + 2] & 0x20) ? true : false;
            p->sprite_counter[p->eval_sn] = p->secondary_oam[p->eval_sn * 4 + 3];
            p->sprite_offset[p->eval_sn] = 0;
            break;
        case 6:
            p->sprite_shift_low[p->eval_sn] =
                (p->eval_sn < p->sprites_on_line) ? readMemory(p, getSpriteAddr(p)) : 0;
            if (p->secondary_oam[p->eval_sn * 4 + 2] & 0x40)
                p->sprite_shift_low[p->eval_sn] = flipByte(p->sprite_shift_low[p->eval_sn]);
            break;
        case 0:
            p->sprite_shift_high[p->eval_sn] =
                (p->eval_sn < p->sprites_on_line) ? readMemory(p, getSpriteAddr(p) + 8) : 0;
            if (p->secondary_oam[p->eval_sn * 4 + 2] & 0x40)
                p->sprite_shift_high[p->eval_sn] = flipByte(p->sprite_shift_high[p->eval_sn]);
            p->eval_sn++;
            break;
    }
}

void update_shift_registers(ppu* p) {
    p->pattern_shift_low <<= 1;
    p->pattern_shift_high <<= 1;
    p->attr_shift_low <<= 1;
    p->attr_shift_high <<= 1;
    if (p->attr_latch_low)
        p->attr_shift_low++;
    if (p->attr_latch_high)
        p->attr_shift_high++;

    if (p->dot <= 257) {
        for (int i = 0; i < p->sprites_on_line; i++) {
            if (p->sprite_counter[i] > 0) {
                p->sprite_counter[i]--;
            } else {
                if (p->sprite_offset[i] < 8) {
                    p->sprite_offset[i]++;
                    p->sprite_shift_low[i] <<= 1;
                    p->sprite_shift_high[i] <<= 1;
                } else {
                    p->sprite_counter[i] = 255;
                }
            }
        }
    }

    if (p->dot % 8 == 1) {
        p->pattern_shift_low &= 0xff00;
        p->pattern_shift_high &= 0xff00;
        p->pattern_shift_low |= p->current_pattern_low;
        p->pattern_shift_high |= p->current_pattern_high;

        uint8_t attr = p->current_attr;
        if ((p->address_v - 1) & 0x02)
            attr >>= 2;
        if (p->address_v & 0x40)
            attr >>= 4;
        p->attr_latch_low = (attr & 1) ? true : false;
        p->attr_latch_high = (attr & 2) ? true : false;
    }
}

void increment_horizontal_v(ppu* p) {
    if ((p->address_v & COARSE_X) == 31) {
        p->address_v &= ~COARSE_X;
        p->address_v ^= NAMETABLE_X;
    } else {
        p->address_v += 1;
    }
}

void increment_vertical_v(ppu* p) {
    if ((p->address_v & FINE_Y) < 0x7000) {
        p->address_v += 0x1000;
    } else {
        p->address_v &= ~FINE_Y;
        int y = (p->address_v & COARSE_Y) >> 5;
        if (y == 29) {
            y = 0;
            p->address_v ^= NAMETABLE_Y;
        } else if (y == 31) {
            y = 0;
        } else {
            y += 1;
        }
        p->address_v = (p->address_v & ~COARSE_Y) | (y << 5);
    }
}

void eval_reset(ppu* p) {
    p->eval_m = 0;
    p->eval_n = 0;
    p->eval_sn = 0;
    p->eval_stage = 0;
    p->sprite0_next_line = false;
}

void eval_read(ppu* p) {
    if ((p->eval_n * 4 + p->eval_m + p->OAMADDR) < 256)
        p->eval_temp = p->oam[p->eval_n * 4 + p->eval_m + p->OAMADDR];
    else
        p->eval_stage = 4;
}

void eval_write(ppu* p) {
    if (p->eval_sn < 8)
        p->secondary_oam[p->eval_sn * 4 + p->eval_m] = p->eval_temp;
}

bool sprite_in_range(uint8_t spr_y, uint16_t screen_y) {
    if (spr_y + 7 >= screen_y && spr_y <= screen_y)
        return true;
    else
        return false;
}

void eval_odd_cycle(ppu* p) {
    switch (p->eval_stage) {
        case 1:
            p->eval_m++;
            break;
        case 2:
            p->eval_m = 0;
            p->eval_n++;
            if (p->eval_n == 0)
                p->eval_stage = 4;
            else if (p->eval_sn < 8)
                p->eval_stage = 0;
            else
                p->eval_stage = 3;
            break;
    }
    eval_read(p);
}

void eval_even_cycle(ppu* p) {
    eval_write(p);
    switch (p->eval_stage) {
        case 0:
            if (sprite_in_range(p->eval_temp, p->scanline))
                p->eval_stage = 1;
            else
                p->eval_stage = 2;
            break;
        case 1:
            if (p->eval_m == 3) {
                p->eval_stage = 2;
                p->eval_sn++;
                if (p->eval_n == 0)
                    p->sprite0_next_line = true;
            }
            break;
        case 3:
            if (sprite_in_range(p->eval_temp, p->scanline)) {
                p->PPUSTATUS &= ~0x40;
            } else {
                p->eval_m++;
                p->eval_n++;
            }
            break;
    }
}

void sprite_evaluation(ppu* p) {
    if (p->dot == 0) {
        eval_reset(p);
    }
    if (p->dot >= 1 && p->dot <= 64)
        p->secondary_oam[(p->dot - 1) / 2] = 0xff;
    if (p->dot >= 65 && p->dot <= 256) {
        if (p->dot % 2) {
            eval_odd_cycle(p);
        } else {
            eval_even_cycle(p);
        }
    }
}

void incrementDot(ppu* p) {
    if (++p->dot > 340) {
        p->dot = 0;
        if (++p->scanline > 261) {
            if (p->odd_frame) {
                p->dot = 1;
                p->odd_frame = false;
            } else {
                p->odd_frame = true;
            }
            p->scanline = 0;
        }
    }
}

uint16_t ppu_executeCycle(ppu* p) {
    incrementDot(p);

    if (p->scanline == 240 && p->dot == 0)
        p->vblank = true;
    if (p->scanline == 241 && p->dot == 1) {
        p->nmi_occurred = true;
        p->PPUSTATUS |= 0x80;
    }
    if (p->scanline == 261 && p->dot == 1) {
        p->vblank = false;
        p->nmi_occurred = false;
        p->PPUSTATUS &= 0x1f;
    }

    p->nmi = (p->nmi_occurred && (p->PPUCTRL & 0x80));

    checkRegisters(p);

    if ((p->PPUMASK & RENDER_EN) && !p->vblank) {
        if ((p->dot >= 2 && p->dot <= 257) || (p->dot >= 322 && p->dot <= 337))
            update_shift_registers(p);
        if ((p->dot >= 1 && p->dot <= 256) || (p->dot >= 321 && p->dot <= 336)) {
            perform_background_fetches(p);
            if (p->dot % 8 == 0)
                increment_horizontal_v(p);
        } else if (p->dot >= 257 && p->dot <= 320) {
            perform_sprite_fetches(p);
        }
        if (p->dot <= 256 && p->scanline < 240)
            sprite_evaluation(p);
        if (p->dot == 256) {
            p->sprites_on_line = p->eval_sn;
            p->eval_sn = 0;
            p->eval_n = 0;
            increment_vertical_v(p);
        }
        if (p->dot == 257) {
            p->address_v &= ~(NAMETABLE_X | COARSE_X);
            p->address_v |= (p->address_temp & (NAMETABLE_X | COARSE_X));
            p->sprite0_this_line = p->sprite0_next_line;
        }
        if (p->dot >= 280 && p->dot <= 304 && p->scanline == 261) {
            p->address_v &= ~(FINE_Y | NAMETABLE_Y | COARSE_Y);
            p->address_v |= (p->address_temp & (FINE_Y | NAMETABLE_Y | COARSE_Y));
        }
    }

    p->reg_access_prev = *p->reg_access;
    p->write_prev = *p->write;

    return (p->dot < 257 && p->dot > 0 && p->scanline < 240) ? calcPixel(p) : 0xffff;
}
