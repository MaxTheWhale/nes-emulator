#ifndef __CPU_H
#define __CPU_H

#include <stdint.h>
#include <stdbool.h>

struct cpu;
typedef struct cpu cpu;
typedef void (*opPtr)(cpu *);
enum access_t {READ = 0, WRITE = 1, READ_WRITE = 2};

cpu *cpu_create();
void cpu_reset(cpu *c);
void cpu_printState(cpu *c);
void cpu_executeCycle(cpu *c);

void cpu_mapMemory(cpu *c, uint16_t address, uint8_t *start, uint16_t size, uint16_t mirrors, enum access_t rw);
uint8_t cpu_readMemory(cpu *c, uint16_t address);
void cpu_writeMemory(cpu *c, uint16_t address, uint8_t value);
void cpu_mapNMI(cpu *c, bool *pointer);
void cpu_mapIRQ(cpu *c, bool *pointer);
bool *cpu_getRW(cpu *c);
uint16_t *cpu_getAddress(cpu *c);

#endif
