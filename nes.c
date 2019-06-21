#include "nes.h"
#include <stdio.h>
#include <stdlib.h>

FILE* fopenCheck(char* file, char* mode) {
    FILE* p = fopen(file, mode);
    if (p != NULL)
        return p;
    fprintf(stderr, "Can't open %s: ", file);
    fflush(stderr);
    perror("");
    exit(1);
}

void mapCPU_PPU(nes* n) {
    cpu_mapMemory(n->cpu, 0x0000, n->ram, 0x800, 4, READ_WRITE);
    cpu_mapNMI(n->cpu, ppu_getNMI(n->ppu));
    for (int i = 0; i < 0x2000; i += 8) {
        cpu_mapMemory(n->cpu, 0x2000 + i, ppu_getPPUCTRL(n->ppu), 1, 1, WRITE);
        cpu_mapMemory(n->cpu, 0x2001 + i, ppu_getPPUMASK(n->ppu), 1, 1, WRITE);
        cpu_mapMemory(n->cpu, 0x2002 + i, ppu_getPPUSTATUS(n->ppu), 1, 1, READ);
        cpu_mapMemory(n->cpu, 0x2003 + i, ppu_getOAMADDR(n->ppu), 1, 1, WRITE);
        cpu_mapMemory(n->cpu, 0x2004 + i, ppu_getOAMDATA(n->ppu), 1, 1, READ_WRITE);
        cpu_mapMemory(n->cpu, 0x2005 + i, ppu_getPPUSCROLL(n->ppu), 1, 1, WRITE);
        cpu_mapMemory(n->cpu, 0x2006 + i, ppu_getPPUADDR(n->ppu), 1, 1, WRITE);
        cpu_mapMemory(n->cpu, 0x2007 + i, ppu_getPPUDATA(n->ppu), 1, 1, READ_WRITE);
    }
    cpu_mapMemory(n->cpu, 0x4014, &n->dma_page, 1, 1, WRITE);
    cpu_mapMemory(n->cpu, 0x4016, &n->pad_latch, 1, 1, WRITE);
    cpu_mapMemory(n->cpu, 0x4016, &n->pad1_port, 1, 1, READ);
    cpu_mapMemory(n->cpu, 0x4017, &n->pad2_port, 1, 1, READ);
    n->write = cpu_getRW(n->cpu);
    ppu_mapRW(n->ppu, n->write);
    n->address = cpu_getAddress(n->cpu);
    n->dot = ppu_getDot(n->ppu);
    n->scanline = ppu_getScanline(n->ppu);
    ppu_mapAddress(n->ppu, n->address);
    ppu_mapRegAccess(n->ppu, &n->reg_access);
}

void nes_loadROM(nes* n, char* rom) {
    FILE* f = fopenCheck(rom, "rb");
    uint8_t header[16];

    fread(header, 16, 1, f);
    if (header[4] == 1) {
        uint8_t* prg = malloc(0x4000);
        fread(prg, 0x1000, 4, f);
        cpu_mapMemory(n->cpu, 0x8000, prg, 0x4000, 2, READ);
    } else if (header[4] == 2) {
        uint8_t* prg = malloc(0x8000);
        fread(prg, 0x1000, 8, f);
        cpu_mapMemory(n->cpu, 0x8000, prg, 0x8000, 1, READ);
    }
    if (header[5] == 1) {
        uint8_t* chr = malloc(0x2000);
        fread(chr, 0x1000, 2, f);
        ppu_mapMemory(n->ppu, 0x0000, chr, 0x2000, 1);
    }
    if (header[6] & 0x1) {
        ppu_mapMemory(n->ppu, 0x2000, n->vram, 0x800, 2);
    } else {
        ppu_mapMemory(n->ppu, 0x2000, n->vram, 0x400, 2);
        ppu_mapMemory(n->ppu, 0x2800, n->vram + 0x400, 0x400, 2);
    }
    fclose(f);
}

void nes_setFramebuffer(nes* n, uint32_t* framebuffer) {
    n->framebuffer = framebuffer;
}

void nes_updatePadState(nes* n, uint8_t state) {
    n->pad1_state = state;
}

void nes_loadPalette(nes* n, char* palette) {
    FILE* f = fopenCheck(palette, "rb");
    uint8_t data[64 * 3];

    fread(data, 64, 3, f);

    for (int i = 0; i < 64; i++) {
        uint32_t colour = 0;
        colour |= (data[i * 3] << 16);
        colour |= (data[i * 3 + 1] << 8);
        colour |= data[i * 3 + 2];
        n->palette[i] = colour;
    }
    for (int i = 1; i < 8; i++) {
        for (int j = 0; j < 64; j++) {
            n->palette[i * 64 + j] = n->palette[j];
        }
    }
    fclose(f);
}

bool nes_stepCycle(nes* n) {
    bool finished_frame = false;
    if (n->dma) {
        if (n->dma_count > 0) {
            if (n->dma_count % 2)
                n->dma_byte = cpu_readMemory(n->cpu, (n->dma_page << 8) | (n->dma_count / 2));
            else
                cpu_writeMemory(n->cpu, 0x2004, n->dma_byte);
            if (n->dma_count == 512)
                n->dma = false;
        }
        n->dma_count++;
    } else {
        cpu_executeCycle(n->cpu);
    }
    if ((*n->address & 0xe000) == 0x2000)
        n->reg_access = true;
    else
        n->reg_access = false;
    if (*n->address == 0x4016 && *n->write) {
        if (!(n->pad_latch & 1)) {
            n->pad1_port = 0x40 | (n->pad1_state & 1);
            n->pad1_count = 0;
        }
    }
    if (*n->address == 0x4016 && !*n->write) {
        n->pad1_count++;
        n->pad1_port = 0x40 | ((n->pad1_state >> n->pad1_count) & 1);
    }
    if (*n->address == 0x4014 && *n->write && !n->dma) {
        n->dma = true;
        n->dma_count = 0;
    }
    for (int i = 0; i < 3; i++) {
        uint16_t pixel = ppu_executeCycle(n->ppu);
        if (pixel < 0x200) {
            n->framebuffer[*n->scanline * 256 + (*n->dot - 1)] = n->palette[pixel];
            if (*n->scanline == 239 && *n->dot == 256)
                finished_frame = true;
        }
    }
    return finished_frame;
}

void nes_emulateFrame(nes* n) {
    while (!nes_stepCycle(n))
        ;
}

nes* nes_create() {
    nes* newNES = malloc(sizeof(nes));
    newNES->cpu = cpu_create();
    newNES->ppu = ppu_create();
    mapCPU_PPU(newNES);
    return newNES;
}
