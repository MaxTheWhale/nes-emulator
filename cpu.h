struct cpu;
typedef struct cpu cpu;

cpu *cpu_create();
void cpu_reset(cpu *c);
void cpu_loadROM(cpu *c, char *rom);
void cpu_printState(cpu *c);
void cpu_executeCycle(cpu *c);