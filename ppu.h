#ifndef __PPU_H
#define __PPU_H

#include <stdint.h>
#include <stdbool.h>

struct ppu;
typedef struct ppu ppu;

ppu *ppu_create();
void ppu_mapMemory(ppu *p, uint16_t address, uint8_t *start, uint16_t size, uint16_t mirrors);
void ppu_mapRW(ppu *p, bool *pointer);
void ppu_mapRegAccess(ppu *p, bool *pointer);
void ppu_mapAddress(ppu *p, uint16_t *pointer);
bool *ppu_getNMI(ppu *p);
uint16_t *ppu_getDot(ppu *p);
uint16_t *ppu_getScanline(ppu *p);
uint8_t *ppu_getPPUCTRL(ppu *p);
uint8_t *ppu_getPPUMASK(ppu *p);
uint8_t *ppu_getPPUSTATUS(ppu *p);
uint8_t *ppu_getOAMADDR(ppu *p);
uint8_t *ppu_getOAMDATA(ppu *p);
uint8_t *ppu_getPPUSCROLL(ppu *p);
uint8_t *ppu_getPPUADDR(ppu *p);
uint8_t *ppu_getPPUDATA(ppu *p);
void ppu_print(ppu *p);
uint16_t ppu_executeCycle(ppu *p);

#endif
