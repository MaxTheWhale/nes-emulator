#include "nes.h"
#include <stdio.h>
#include <stdlib.h>

FILE *fopenCheck(char *file, char *mode)
{
    FILE *p = fopen(file, mode);
    if (p != NULL) return p;
    fprintf(stderr, "Can't open %s: ", file);
    fflush(stderr);
    perror("");
    exit(1);
}

void mapCPU_PPU(cpu *c, ppu *p)
{
    uint8_t *ram = malloc(sizeof(uint8_t) * 0x800);
    for (int i = 0; i < 4; i++)
    {
        for (int j = 0; j < 0x800; j++)
        {
            cpu_mapMemory(c, (i*0x800) + j, ram + j);
        }
    }
    cpu_mapNMI(c, ppu_getNMI(p));
    cpu_mapMemory(c, 0x2000, ppu_getPPUCTRL(p));
    cpu_mapMemory(c, 0x2001, ppu_getPPUMASK(p));
    cpu_mapMemory(c, 0x2002, ppu_getPPUSTATUS(p));
    cpu_mapMemory(c, 0x2003, ppu_getOAMADDR(p));
    cpu_mapMemory(c, 0x2004, ppu_getOAMDATA(p));
    cpu_mapMemory(c, 0x2005, ppu_getPPUSCROLL(p));
    cpu_mapMemory(c, 0x2006, ppu_getPPUADDR(p));
    cpu_mapMemory(c, 0x2007, ppu_getPPUDATA(p));
    ppu_mapRW(p, cpu_getRW(c));
    ppu_mapAddress(p, cpu_getAddress(c));
}

void nes_loadROM(nes *n, char *rom)
{
    FILE *f = fopenCheck(rom, "rb");
    uint8_t header[16];

	fread(header, 16, 1, f);
	if (header[4] == 1)
	{
		uint8_t *prg = malloc(0x4000);
		fread(prg, 0x1000, 4, f);
		for (int i = 0; i < 0x4000; i++)
		{
			cpu_mapMemory(n->cpu, 0x8000 + i, prg + i);
            cpu_mapMemory(n->cpu, 0xc000 + i, prg + i);
		}
	}
    if (header[5] == 1)
	{
		uint8_t *chr = malloc(0x2000);
		fread(chr, 0x1000, 2, f);
		for (int i = 0; i < 0x2000; i++)
		{
			ppu_mapMemory(n->ppu, 0x0000 + i, chr + i);
        }
	}
	fclose(f);
}

void nes_loadPalette(nes *n, char *palette)
{
    FILE *f = fopenCheck(palette, "rb");
    uint8_t data[64*3];

	fread(data, 64, 3, f);

    for (int i = 0; i < 64; i++)
    {
        uint32_t colour = 0;
        colour |= data[i*3];
        colour |= (data[i*3+1] << 8);
        colour |= (data[i*3+2] << 16);
        n->palette[i] = colour;
    }
    fclose(f);
}

void nes_emulateFrame(nes* n, uint32_t *framebuffer)
{
    for (int i = 0; i < (256*240); i++)
    {
        framebuffer[i] = n->palette[(i/8)%64];
    }
}

nes *nes_create()
{
    nes *newNES = malloc(sizeof(nes));
    newNES->cpu = cpu_create();
    newNES->ppu = ppu_create();
    mapCPU_PPU(newNES->cpu, newNES->ppu);
    return newNES;
}