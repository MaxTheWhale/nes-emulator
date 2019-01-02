#include <stdint.h>

struct ppu;
typedef struct ppu ppu;

ppu* ppu_create();
void ppu_mapMemory(ppu *p, uint16_t address, uint8_t *pointer);
void ppu_mapRW(ppu *p, uint8_t *pointer);
void ppu_mapAddress(ppu *p, uint16_t *pointer);
uint8_t* ppu_getNMI(ppu *p);
uint8_t* ppu_getPPUCTRL(ppu *p);
uint8_t* ppu_getPPUMASK(ppu *p);
uint8_t* ppu_getPPUSTATUS(ppu *p);
uint8_t* ppu_getOAMADDR(ppu *p);
uint8_t* ppu_getOAMDATA(ppu *p);
uint8_t* ppu_getPPUSCROLL(ppu *p);
uint8_t* ppu_getPPUADDR(ppu *p);
uint8_t* ppu_getPPUDATA(ppu *p);