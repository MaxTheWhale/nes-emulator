#ifndef __APU_H
#define __APU_H

#include <stdbool.h>
#include <stdint.h>

struct apu;
typedef struct apu apu;

apu* apu_create();
bool* apu_getIRQ(apu* p);
void apu_mapRW(apu* a, bool* pointer);
void apu_mapAddress(apu* a, uint16_t* pointer);
void apu_executeCycle(apu* p);

#endif
