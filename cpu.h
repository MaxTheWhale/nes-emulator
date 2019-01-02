#include <stdint.h>

struct cpu;
typedef struct cpu cpu;
typedef void (*opPtr)(cpu*);

cpu *cpu_create();
void cpu_reset(cpu *c);
void cpu_printState(cpu *c);
void cpu_executeCycle(cpu *c);

void cpu_mapMemory(cpu *c, uint16_t address, uint8_t *pointer);
void cpu_mapNMI(cpu *c, uint8_t *pointer);
void cpu_mapIRQ(cpu *c, uint8_t *pointer);
uint8_t* cpu_getRW(cpu *c);
uint16_t* cpu_getAddress(cpu *c);