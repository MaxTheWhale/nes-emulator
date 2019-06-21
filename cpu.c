#include "cpu.h"
#include <stdio.h>
#include <stdlib.h>

const char instrs[256][4] = {
    "BRK", "ORA", "XXX", "SLO", "DOP", "ORA", "ASL", "SLO", "PHP", "ORA", "ASL", "AAC", "TOP",
    "ORA", "ASL", "SLO", "BPL", "ORA", "XXX", "SLO", "DOP", "ORA", "ASL", "SLO", "CLC", "ORA",
    "NOP", "SLO", "TOP", "ORA", "ASL", "SLO", "JSR", "AND", "XXX", "RLA", "BIT", "AND", "ROL",
    "RLA", "PLP", "AND", "ROL", "AAC", "BIT", "AND", "ROL", "RLA", "BMI", "AND", "XXX", "RLA",
    "DOP", "AND", "ROL", "RLA", "SEC", "AND", "NOP", "RLA", "TOP", "AND", "ROL", "RLA", "RTI",
    "EOR", "XXX", "SRE", "DOP", "EOR", "LSR", "SRE", "PHA", "EOR", "LSR", "ASR", "JMP", "EOR",
    "LSR", "SRE", "BVC", "EOR", "XXX", "SRE", "DOP", "EOR", "LSR", "SRE", "CLI", "EOR", "NOP",
    "SRE", "TOP", "EOR", "LSR", "SRE", "RTS", "ADC", "XXX", "RRA", "DOP", "ADC", "ROR", "RRA",
    "PLA", "ADC", "ROR", "ARR", "JMP", "ADC", "ROR", "RRA", "BVS", "ADC", "XXX", "RRA", "DOP",
    "ADC", "ROR", "RRA", "SEI", "ADC", "NOP", "RRA", "TOP", "ADC", "ROR", "RRA", "DOP", "STA",
    "DOP", "AAX", "STY", "STA", "STX", "AAX", "DEY", "DOP", "TXA", "???", "STY", "STA", "STX",
    "AAX", "BCC", "STA", "XXX", "???", "STY", "STA", "STX", "AAX", "TYA", "STA", "TXS", "???",
    "SYA", "STA", "SXA", "???", "LDY", "LDA", "LDX", "LAX", "LDY", "LDA", "LDX", "LAX", "TAY",
    "LDA", "TAX", "ATX", "LDY", "LDA", "LDX", "LAX", "BCS", "LDA", "XXX", "LAX", "LDY", "LDA",
    "LDX", "LAX", "CLV", "LDA", "TSX", "???", "LDY", "LDA", "LDX", "LAX", "CPY", "CMP", "DOP",
    "DCP", "CPY", "CMP", "DEC", "DCP", "INY", "CMP", "DEX", "AXS", "CPY", "CMP", "DEC", "DCP",
    "BNE", "CMP", "XXX", "DCP", "DOP", "CMP", "DEC", "DCP", "CLD", "CMP", "NOP", "DCP", "TOP",
    "CMP", "DEC", "DCP", "CPX", "SBC", "DOP", "ISC", "CPX", "SBC", "INC", "ISC", "INX", "SBC",
    "NOP", "SBC", "CPX", "SBC", "INC", "ISC", "BEQ", "SBC", "XXX", "ISC", "DOP", "SBC", "INC",
    "ISC", "SED", "SBC", "NOP", "ISC", "TOP", "SBC", "INC", "ISC"};

// Status bitmasks
enum {
    CARRY =       0x01,
    ZERO =        0x02,
    IRQ_DISABLE = 0x04,
    DECIMAL =     0x08,
    OVERFLOW =    0x40,
    NEGATIVE =    0x80
};

// Addressing modes
enum AddressMode_t {
    IMPLIED,
    ACCUMULATOR,
    RELATIVE,
    IMMEDIATE,
    ZEROPAGE,
    ZEROPAGE_X,
    ZEROPAGE_Y,
    ABSOLUTE,
    ABSOLUTE_X,
    ABSOLUTE_Y,
    INDIRECT_X,
    INDIRECT_Y
};
enum InstructionMode_t { STD_OP, RMW_OP, RMW_COMBO_OP };

struct cpu {
    // External signals
    uint16_t address;
    bool write;
    bool* nmi;
    bool* irq;

    uint8_t accumulator;
    uint8_t x;
    uint8_t y;
    uint8_t stackPointer;
    uint8_t temp;
    uint8_t dummy;
    uint8_t flags;
    uint16_t pc;
    uint8_t* pcL;
    uint8_t* pcH;
    uint16_t ad;
    uint8_t* adL;
    uint8_t* adH;
    uint8_t currentOp;
    uint8_t tick;
    bool nmi_prev;
    bool nmi_pending;
    bool irq_pending;
    bool nmi_executing;
    bool nmi_starting;

    opPtr ops[256][10];

    uint8_t* memory_read[0x10000];
    uint8_t* memory_write[0x10000];
};
typedef void (*opPtr)(cpu*);

uint8_t cpu_readMemory(cpu* c, uint16_t address) {
    c->address = address;
    c->write = false;
    return *c->memory_read[address];
}
void cpu_writeMemory(cpu* c, uint16_t address, uint8_t value) {
    c->address = address;
    c->write = true;
    *c->memory_write[address] = value;
}


void fetchOp(cpu* c) {
    c->currentOp = cpu_readMemory(c, c->pc);
    c->tick = 0;
    if (c->nmi_pending) {
        c->currentOp = 0;
        c->nmi_starting = true;
    } else {
        c->pc++;
    }
}
void stuck(cpu* c) {
    c->tick = 0;
}

void loadRegister(cpu* c, uint8_t* reg, uint8_t value) {
    if (value == 0)
        c->flags |= ZERO;
    else
        c->flags &= ~ZERO;
    if ((value & 0x80) == 0)
        c->flags &= ~NEGATIVE;
    else
        c->flags |= NEGATIVE;
    *reg = value;
}

void writeValue(cpu* c, uint16_t address, uint8_t value) {
    if (value == 0)
        c->flags |= ZERO;
    else
        c->flags &= ~ZERO;
    if ((value & 0x80) == 0)
        c->flags &= ~NEGATIVE;
    else
        c->flags |= NEGATIVE;
    cpu_writeMemory(c, address, value);
}

void fetchPCHbrk(cpu* c) {
    if (c->nmi_starting) {
        *c->pcH = cpu_readMemory(c, 0xfffb);
        c->nmi_pending = false;
        c->nmi_starting = false;
        c->nmi_executing = true;
    } else {
        *c->pcH = cpu_readMemory(c, 0xffff);
    }
}

void fetchPCLbrk(cpu* c) {
    if (c->nmi_starting)
        *c->pcL = cpu_readMemory(c, 0xfffa);
    else
        *c->pcL = cpu_readMemory(c, 0xfffe);
    c->flags |= IRQ_DISABLE;
}

void fetchADL(cpu* c) {
    *c->adH = 0;
    *c->adL = cpu_readMemory(c, c->pc);
    c->pc++;
}

void fetchADH(cpu* c) {
    *c->adH = cpu_readMemory(c, c->pc);
    c->pc++;
}

void readAddress(cpu* c) {
    c->temp = cpu_readMemory(c, c->ad);
}

void writeAddress(cpu* c) {
    cpu_writeMemory(c, c->ad, c->temp);
}
void fetchIndirectY(cpu* c) {
    (*c->adL)++;
    *c->adH = cpu_readMemory(c, c->ad);
    *c->adL = c->temp + c->y;
}
void fetchIndirectXLow(cpu* c) {
    *c->adL += c->x;
    c->temp = cpu_readMemory(c, c->ad);
}
void fetchIndirectXHigh(cpu* c) {
    (*c->adL)++;
    *c->adH = cpu_readMemory(c, c->ad);
    *c->adL = c->temp;
}

void readIndirect(cpu* c) {
    if (*c->adL < c->y) {
        c->temp = cpu_readMemory(c, c->ad);
        (*c->adH)++;
    } else {
        c->ops[c->currentOp][5](c);
    }
}
void readZpX(cpu* c) {
    c->temp = cpu_readMemory(c, c->ad);
    *c->adL += c->x;
}
void readZpY(cpu* c) {
    c->temp = cpu_readMemory(c, c->ad);
    *c->adL += c->y;
}
void readAbsX(cpu* c) {
    *c->adL += c->x;
    if (*c->adL < c->x) {
        c->temp = cpu_readMemory(c, c->ad);
        (*c->adH)++;
    } else {
        c->ops[c->currentOp][4](c);
    }
}
void readAbsY(cpu* c) {
    *c->adL += c->y;
    if (*c->adL < c->y) {
        c->temp = cpu_readMemory(c, c->ad);
        (*c->adH)++;
    } else {
        c->ops[c->currentOp][4](c);
    }
}
void writeAbsX(cpu* c) {
    *c->adL += c->x;
    c->temp = cpu_readMemory(c, c->ad);
    if (*c->adL < c->x)
        (*c->adH)++;
}
void writeAbsY(cpu* c) {
    *c->adL += c->y;
    c->temp = cpu_readMemory(c, c->ad);
    if (*c->adL < c->y)
        (*c->adH)++;
}

void writeIndirect(cpu* c) {
    c->temp = cpu_readMemory(c, c->ad);
    if (*c->adL < c->y)
        (*c->adH)++;
}

void pushBStatus(cpu* c) {
    if (c->nmi_starting)
        cpu_writeMemory(c, 0x100 + c->stackPointer, (c->flags & ~0x10) | 0x20);
    else
        cpu_writeMemory(c, 0x100 + c->stackPointer, c->flags | 0x30);
    c->stackPointer--;
}

void pushPCL(cpu* c) {
    cpu_writeMemory(c, 0x100 + c->stackPointer, *c->pcL);
    c->stackPointer--;
    if (c->currentOp == 0 && c->nmi_pending)
        c->nmi_starting = true;
}
void pushPCH(cpu* c) {
    cpu_writeMemory(c, 0x100 + c->stackPointer, *c->pcH);
    c->stackPointer--;
}
void pullPCL(cpu* c) {
    *c->pcL = cpu_readMemory(c, 0x100 + c->stackPointer);
    c->stackPointer++;
}
void pullPCH(cpu* c) {
    *c->pcH = cpu_readMemory(c, 0x100 + c->stackPointer);
}
void rtiPullPCH(cpu* c) {
    *c->pcH = cpu_readMemory(c, 0x100 + c->stackPointer);
    if (c->nmi_executing) {
        c->nmi_executing = false;
        c->nmi_pending = false;
    }
}
void pullFlags(cpu* c) {
    c->flags = cpu_readMemory(c, 0x100 + c->stackPointer);
    c->stackPointer++;
}
void incPC(cpu* c) {
    c->pc++;
}

void fetchValue(cpu* c) {
    c->temp = cpu_readMemory(c, c->pc);
    if (!c->nmi_starting)
        c->pc++;
}
void readValue(cpu* c) {
    c->temp = cpu_readMemory(c, c->pc);
}

void readStack(cpu* c) {
    c->temp = cpu_readMemory(c, 0x100 + c->stackPointer);
}

void immediateOp(cpu* c) {
    fetchValue(c);
    c->ops[c->currentOp][++c->tick](c);
}
void memoryOp(cpu* c) {
    c->temp = cpu_readMemory(c, c->ad);
    c->ops[c->currentOp][++c->tick](c);
}
void writeOp(cpu* c) {
    c->ops[c->currentOp][++c->tick](c);
    writeValue(c, c->ad, c->temp);
}
void accumulatorOp(cpu* c) {
    c->temp = c->accumulator;
    c->ops[c->currentOp][++c->tick](c);
    loadRegister(c, &c->accumulator, c->temp);
}
void ins(cpu* c) {
    c->stackPointer++;
}


void nop(cpu* c) {
    c->temp = cpu_readMemory(c, c->pc);
}
void tax(cpu* c) {
    loadRegister(c, &c->x, c->accumulator);
}
void tay(cpu* c) {
    loadRegister(c, &c->y, c->accumulator);
}
void tsx(cpu* c) {
    loadRegister(c, &c->x, c->stackPointer);
}
void txa(cpu* c) {
    loadRegister(c, &c->accumulator, c->x);
}
void txs(cpu* c) {
    c->stackPointer = c->x;
}
void tya(cpu* c) {
    loadRegister(c, &c->accumulator, c->y);
}
void sec(cpu* c) {
    c->flags |= CARRY;
}
void sei(cpu* c) {
    c->flags |= IRQ_DISABLE;
}
void sed(cpu* c) {
    c->flags |= DECIMAL;
}
void clc(cpu* c) {
    c->flags &= ~CARRY;
}
void cli(cpu* c) {
    c->flags &= ~IRQ_DISABLE;
}
void cld(cpu* c) {
    c->flags &= ~DECIMAL;
}
void clv(cpu* c) {
    c->flags &= ~OVERFLOW;
}
void lsr(cpu* c) {
    if (c->temp & 0x1)
        c->flags |= CARRY;
    else
        c->flags &= ~CARRY;
    c->temp >>= 1;
}
void asl(cpu* c) {
    if (c->temp & 0x80)
        c->flags |= CARRY;
    else
        c->flags &= ~CARRY;
    c->temp <<= 1;
}
void ror(cpu* c) {
    uint8_t val = c->temp >> 1;
    if (c->flags & CARRY)
        val |= 0x80;
    else
        val &= ~0x80;
    if (c->temp & 0x1)
        c->flags |= CARRY;
    else
        c->flags &= ~CARRY;
    c->temp = val;
}
void rol(cpu* c) {
    uint8_t val = c->temp << 1;
    if (c->flags & CARRY)
        val |= 0x1;
    else
        val &= ~0x1;
    if (c->temp & 0x80)
        c->flags |= CARRY;
    else
        c->flags &= ~CARRY;
    c->temp = val;
}
void inx(cpu* c) {
    loadRegister(c, &c->x, c->x + 1);
}
void iny(cpu* c) {
    loadRegister(c, &c->y, c->y + 1);
}
void dex(cpu* c) {
    loadRegister(c, &c->x, c->x - 1);
}
void dey(cpu* c) {
    loadRegister(c, &c->y, c->y - 1);
}
void lda(cpu* c) {
    loadRegister(c, &c->accumulator, c->temp);
}
void ldx(cpu* c) {
    loadRegister(c, &c->x, c->temp);
}
void ldy(cpu* c) {
    loadRegister(c, &c->y, c->temp);
}
void eor(cpu* c) {
    loadRegister(c, &c->accumulator, (c->temp ^ c->accumulator));
}
void and(cpu * c) {
    loadRegister(c, &c->accumulator, (c->temp & c->accumulator));
}
void ora(cpu* c) {
    loadRegister(c, &c->accumulator, (c->temp | c->accumulator));
}
void adc(cpu* c) {
    uint16_t sum = c->accumulator + c->temp + (c->flags & CARRY);
    uint8_t result = sum & 0xff;
    if (sum & 0x100)
        c->flags |= CARRY;
    else
        c->flags &= ~CARRY;
    if ((c->accumulator ^ result) & (c->temp ^ result) & 0x80)
        c->flags |= OVERFLOW;
    else
        c->flags &= ~OVERFLOW;
    loadRegister(c, &c->accumulator, result);
}
void sbc(cpu* c) {
    c->temp = ~c->temp;
    adc(c);
}
void cmp(cpu* c) {
    if (c->temp <= c->accumulator)
        c->flags |= CARRY;
    else
        c->flags &= ~CARRY;
    if (c->temp == c->accumulator)
        c->flags |= ZERO;
    else
        c->flags &= ~ZERO;
    if ((c->accumulator - c->temp) & 0x80)
        c->flags |= NEGATIVE;
    else
        c->flags &= ~NEGATIVE;
}
void cpx(cpu* c) {
    if (c->temp <= c->x)
        c->flags |= CARRY;
    else
        c->flags &= ~CARRY;
    if (c->temp == c->x)
        c->flags |= ZERO;
    else
        c->flags &= ~ZERO;
    if ((c->x - c->temp) & 0x80)
        c->flags |= NEGATIVE;
    else
        c->flags &= ~NEGATIVE;
}
void cpy(cpu* c) {
    if (c->temp <= c->y)
        c->flags |= CARRY;
    else
        c->flags &= ~CARRY;
    if (c->temp == c->y)
        c->flags |= ZERO;
    else
        c->flags &= ~ZERO;
    if ((c->y - c->temp) & 0x80)
        c->flags |= NEGATIVE;
    else
        c->flags &= ~NEGATIVE;
}
void bit(cpu* c) {
    uint8_t val = cpu_readMemory(c, c->ad);
    if (val & c->accumulator)
        c->flags &= ~ZERO;
    else
        c->flags |= ZERO;
    if (val & 0x80)
        c->flags |= NEGATIVE;
    else
        c->flags &= ~NEGATIVE;
    if (val & 0x40)
        c->flags |= OVERFLOW;
    else
        c->flags &= ~OVERFLOW;
}
void sta(cpu* c) {
    cpu_writeMemory(c, c->ad, c->accumulator);
}
void stx(cpu* c) {
    cpu_writeMemory(c, c->ad, c->x);
}
void sty(cpu* c) {
    cpu_writeMemory(c, c->ad, c->y);
}
void dec(cpu* c) {
    c->temp--;
}
void inc(cpu* c) {
    c->temp++;
}
void beq(cpu* c) {
    if (c->flags & ZERO)
        *c->pcL += c->temp;
    else
        fetchOp(c);
}
void bne(cpu* c) {
    if (!(c->flags & ZERO))
        *c->pcL += c->temp;
    else
        fetchOp(c);
}
void bmi(cpu* c) {
    if (c->flags & NEGATIVE)
        *c->pcL += c->temp;
    else
        fetchOp(c);
}
void bpl(cpu* c) {
    if (!(c->flags & NEGATIVE))
        *c->pcL += c->temp;
    else
        fetchOp(c);
}
void bcs(cpu* c) {
    if (c->flags & CARRY)
        *c->pcL += c->temp;
    else
        fetchOp(c);
}
void bcc(cpu* c) {
    if (!(c->flags & CARRY))
        *c->pcL += c->temp;
    else
        fetchOp(c);
}
void bvs(cpu* c) {
    if (c->flags & OVERFLOW)
        *c->pcL += c->temp;
    else
        fetchOp(c);
}
void bvc(cpu* c) {
    if (!(c->flags & OVERFLOW))
        *c->pcL += c->temp;
    else
        fetchOp(c);
}
void branch(cpu* c) {
    if (c->temp < 128) {
        if (*c->pcL < c->temp) {
            (*c->pcH)++;
            c->tick++;
        } else {
            fetchOp(c);
        }
    } else {
        uint8_t prev = *c->pcL - c->temp;
        if (*c->pcL > prev) {
            (*c->pcH)--;
            c->tick++;
        } else {
            fetchOp(c);
        }
    }
}
void jmp(cpu* c) {
    *c->pcH = cpu_readMemory(c, c->pc);
    *c->pcL = *c->adL;
}
void jmpIndirect(cpu* c) {
    (*c->adL)++;
    *c->pcH = cpu_readMemory(c, c->ad);
    *c->pcL = c->temp;
}
void pla(cpu* c) {
    loadRegister(c, &c->accumulator, cpu_readMemory(c, 0x100 + c->stackPointer));
}
void plp(cpu* c) {
    c->flags = cpu_readMemory(c, 0x100 + c->stackPointer);
}
void pha(cpu* c) {
    cpu_writeMemory(c, 0x100 + c->stackPointer, c->accumulator);
    c->stackPointer--;
}
void php(cpu* c) {
    cpu_writeMemory(c, 0x100 + c->stackPointer, c->flags | 0x30);
    c->stackPointer--;
}

void lax(cpu* c) {
    loadRegister(c, &c->accumulator, c->temp);
    loadRegister(c, &c->x, c->temp);
}

void anc(cpu* c) {
    and(c);
    if (c->accumulator & 0x80)
        c->flags |= CARRY;
    else
        c->flags &= ~CARRY;
}
void alr(cpu* c) {
    and(c);
    c->temp = c->accumulator;
    lsr(c);
    loadRegister(c, &c->accumulator, c->temp);
}
void arr(cpu* c) {
    and(c);
    c->temp = c->accumulator;
    ror(c);
    loadRegister(c, &c->accumulator, c->temp);
    if (c->accumulator & 0x40) {
        c->flags |= CARRY;
        if (c->accumulator & 0x20)
            c->flags &= ~OVERFLOW;
        else
            c->flags |= OVERFLOW;
    } else {
        c->flags &= ~CARRY;
        if (c->accumulator & 0x20)
            c->flags |= OVERFLOW;
        else
            c->flags &= ~OVERFLOW;
    }
}
void atx(cpu* c) {
    loadRegister(c, &c->accumulator, c->temp);
    loadRegister(c, &c->x, c->accumulator);
}
void axs(cpu* c) {
    if (c->temp <= (c->accumulator & c->x))
        c->flags |= CARRY;
    else
        c->flags &= ~CARRY;
    loadRegister(c, &c->x, (c->accumulator & c->x) - c->temp);
}

void sya(cpu* c) {
    uint8_t result = c->y & (*c->adH + 1);
    if (cpu_readMemory(c, c->pc - 1) != *c->adH)
        *c->adH = result;
    cpu_writeMemory(c, c->ad, result);
}
void sxa(cpu* c) {
    uint8_t result = c->x & (*c->adH + 1);
    if (cpu_readMemory(c, c->pc - 1) != *c->adH)
        *c->adH = result;
    cpu_writeMemory(c, c->ad, result);
}

void aax(cpu* c) {
    cpu_writeMemory(c, c->ad, c->accumulator & c->x);
}

void dcp(cpu* c) {
    dec(c);
    writeValue(c, c->ad, c->temp);
    readAddress(c);
    cmp(c);
}
void isc(cpu* c) {
    inc(c);
    writeValue(c, c->ad, c->temp);
    readAddress(c);
    sbc(c);
}
void rla(cpu* c) {
    rol(c);
    writeValue(c, c->ad, c->temp);
    readAddress(c);
    and(c);
}
void rra(cpu* c) {
    ror(c);
    writeValue(c, c->ad, c->temp);
    readAddress(c);
    adc(c);
}
void slo(cpu* c) {
    asl(c);
    writeValue(c, c->ad, c->temp);
    readAddress(c);
    ora(c);
}
void sre(cpu* c) {
    lsr(c);
    writeValue(c, c->ad, c->temp);
    readAddress(c);
    eor(c);
}

void cpu_mapMemory(cpu* c,
                   uint16_t address,
                   uint8_t* start,
                   uint16_t size,
                   uint16_t mirrors,
                   enum access_t rw) {
    for (int m = 0; m < mirrors; m++) {
        for (int n = 0; n < size; n++) {
            if (rw == READ || rw == READ_WRITE)
                c->memory_read[address + (m * size) + n] = start + n;
            if (rw == WRITE || rw == READ_WRITE)
                c->memory_write[address + (m * size) + n] = start + n;
        }
    }
}
void cpu_mapNMI(cpu* c, bool* pointer) {
    c->nmi = pointer;
}
void cpu_mapIRQ(cpu* c, bool* pointer) {
    c->irq = pointer;
}
bool* cpu_getRW(cpu* c) {
    return &(c->write);
}
uint16_t* cpu_getAddress(cpu* c) {
    return &(c->address);
}

void map_operation(cpu* c,
                   uint8_t opcode,
                   enum AddressMode_t addr_mode,
                   enum InstructionMode_t instr_mode,
                   opPtr op) {
    c->ops[opcode][0] = fetchOp;
    int op_offset = 0;

    switch (addr_mode) {
        case IMPLIED:
            op_offset = 1;
            break;
        case ACCUMULATOR:
            c->ops[opcode][1] = accumulatorOp;
            op_offset = 2;
            break;
        case IMMEDIATE:
            c->ops[opcode][1] = immediateOp;
            op_offset = 2;
            break;
        case ABSOLUTE:
            c->ops[opcode][1] = fetchADL;
            c->ops[opcode][2] = fetchADH;
            c->ops[opcode][3] = memoryOp;
            op_offset = 4;
            break;
        case ZEROPAGE:
            c->ops[opcode][1] = fetchADL;
            c->ops[opcode][2] = memoryOp;
            op_offset = 3;
            break;
        case ZEROPAGE_X:
            c->ops[opcode][1] = fetchADL;
            c->ops[opcode][2] = readZpX;
            c->ops[opcode][3] = memoryOp;
            op_offset = 4;
            break;
        case ZEROPAGE_Y:
            c->ops[opcode][1] = fetchADL;
            c->ops[opcode][2] = readZpY;
            c->ops[opcode][3] = memoryOp;
            op_offset = 4;
            break;
        case ABSOLUTE_X:
            c->ops[opcode][1] = fetchADL;
            c->ops[opcode][2] = fetchADH;
            c->ops[opcode][3] = readAbsX;
            c->ops[opcode][4] = memoryOp;
            op_offset = 5;
            break;
        case ABSOLUTE_Y:
            c->ops[opcode][1] = fetchADL;
            c->ops[opcode][2] = fetchADH;
            c->ops[opcode][3] = readAbsY;
            c->ops[opcode][4] = memoryOp;
            op_offset = 5;
            break;
        case INDIRECT_X:
            c->ops[opcode][1] = fetchADL;
            c->ops[opcode][2] = readAddress;
            c->ops[opcode][3] = fetchIndirectXLow;
            c->ops[opcode][4] = fetchIndirectXHigh;
            c->ops[opcode][5] = memoryOp;
            op_offset = 6;
            break;
        case INDIRECT_Y:
            c->ops[opcode][1] = fetchADL;
            c->ops[opcode][2] = readAddress;
            c->ops[opcode][3] = fetchIndirectY;
            c->ops[opcode][4] = readIndirect;
            c->ops[opcode][5] = memoryOp;
            op_offset = 6;
            break;
        case RELATIVE:
            c->ops[opcode][1] = fetchValue;
            op_offset = 2;
            c->ops[opcode][3] = branch;
            break;
    }

    switch (instr_mode) {
        case RMW_OP:
            c->ops[opcode][op_offset - 1] = readAddress;
            c->ops[opcode][op_offset] = writeAddress;
            c->ops[opcode][op_offset + 1] = writeOp;
            c->ops[opcode][op_offset + 2] = op;
            break;
        case RMW_COMBO_OP:
            c->ops[opcode][op_offset - 1] = readAddress;
            c->ops[opcode][op_offset] = writeAddress;
            c->ops[opcode][op_offset + 1] = op;
            break;
        default:
            c->ops[opcode][op_offset] = op;
            break;
    }
}

cpu* cpu_create() {
    cpu* newCPU = malloc(sizeof(cpu));

    newCPU->pcL = (uint8_t*)&newCPU->pc;
    newCPU->pcH = newCPU->pcL + 1;
    newCPU->adL = (uint8_t*)&newCPU->ad;
    newCPU->adH = newCPU->adL + 1;
    newCPU->tick = 0xff;
    newCPU->stackPointer = 0xfd;
    newCPU->nmi_executing = false;
    newCPU->nmi_pending = false;
    newCPU->nmi_prev = false;
    newCPU->irq_pending = false;
    newCPU->nmi_starting = false;
    newCPU->nmi = (bool*)&newCPU->dummy;
    newCPU->irq = (bool*)&newCPU->dummy;
    for (int i = 0; i < 0x10000; i++) {
        newCPU->memory_read[i] = &(newCPU->dummy);
        newCPU->memory_write[i] = &(newCPU->dummy);
    }
    for (int i = 0; i < 0x100; i++) {
        for (int j = 0; j < 10; j++) {
            newCPU->ops[i][j] = fetchOp;
        }
    }

    // BRK
    newCPU->ops[0x00][1] = fetchValue;
    newCPU->ops[0x00][2] = pushPCH;
    newCPU->ops[0x00][3] = pushPCL;
    newCPU->ops[0x00][4] = pushBStatus;
    newCPU->ops[0x00][5] = fetchPCLbrk;
    newCPU->ops[0x00][6] = fetchPCHbrk;

    // JSR
    newCPU->ops[0x20][1] = fetchADL;
    newCPU->ops[0x20][2] = readStack;
    newCPU->ops[0x20][3] = pushPCH;
    newCPU->ops[0x20][4] = pushPCL;
    newCPU->ops[0x20][5] = jmp;

    // JMP indirect
    newCPU->ops[0x6c][1] = fetchADL;
    newCPU->ops[0x6c][2] = fetchADH;
    newCPU->ops[0x6c][3] = readAddress;
    newCPU->ops[0x6c][4] = jmpIndirect;

    // PLP
    newCPU->ops[0x28][1] = readValue;
    newCPU->ops[0x28][2] = ins;
    newCPU->ops[0x28][3] = plp;

    // PLA
    newCPU->ops[0x68][1] = readValue;
    newCPU->ops[0x68][2] = ins;
    newCPU->ops[0x68][3] = pla;

    // PHA
    newCPU->ops[0x48][1] = readValue;
    newCPU->ops[0x48][2] = pha;

    // PHP
    newCPU->ops[0x08][1] = readValue;
    newCPU->ops[0x08][2] = php;

    // JMP
    newCPU->ops[0x4c][1] = fetchADL;
    newCPU->ops[0x4c][2] = jmp;

    // RTI
    newCPU->ops[0x40][1] = fetchValue;
    newCPU->ops[0x40][2] = ins;
    newCPU->ops[0x40][3] = pullFlags;
    newCPU->ops[0x40][4] = pullPCL;
    newCPU->ops[0x40][5] = rtiPullPCH;

    // RTS
    newCPU->ops[0x60][1] = fetchValue;
    newCPU->ops[0x60][2] = ins;
    newCPU->ops[0x60][3] = pullPCL;
    newCPU->ops[0x60][4] = pullPCH;
    newCPU->ops[0x60][5] = incPC;

    // STANDARD OPS
    map_operation(newCPU, 0x18, IMPLIED, STD_OP, clc);
    map_operation(newCPU, 0xd8, IMPLIED, STD_OP, cld);
    map_operation(newCPU, 0x58, IMPLIED, STD_OP, cli);
    map_operation(newCPU, 0xb8, IMPLIED, STD_OP, clv);
    map_operation(newCPU, 0x38, IMPLIED, STD_OP, sec);
    map_operation(newCPU, 0xf8, IMPLIED, STD_OP, sed);
    map_operation(newCPU, 0x78, IMPLIED, STD_OP, sei);
    map_operation(newCPU, 0xaa, IMPLIED, STD_OP, tax);
    map_operation(newCPU, 0xa8, IMPLIED, STD_OP, tay);
    map_operation(newCPU, 0xba, IMPLIED, STD_OP, tsx);
    map_operation(newCPU, 0x8a, IMPLIED, STD_OP, txa);
    map_operation(newCPU, 0x9a, IMPLIED, STD_OP, txs);
    map_operation(newCPU, 0x98, IMPLIED, STD_OP, tya);
    map_operation(newCPU, 0xea, IMPLIED, STD_OP, nop);
    map_operation(newCPU, 0xe8, IMPLIED, STD_OP, inx);
    map_operation(newCPU, 0xc8, IMPLIED, STD_OP, iny);
    map_operation(newCPU, 0xca, IMPLIED, STD_OP, dex);
    map_operation(newCPU, 0x88, IMPLIED, STD_OP, dey);
    map_operation(newCPU, 0x0a, ACCUMULATOR, STD_OP, asl);
    map_operation(newCPU, 0x4a, ACCUMULATOR, STD_OP, lsr);
    map_operation(newCPU, 0x2a, ACCUMULATOR, STD_OP, rol);
    map_operation(newCPU, 0x6a, ACCUMULATOR, STD_OP, ror);
    map_operation(newCPU, 0x10, RELATIVE, STD_OP, bpl);
    map_operation(newCPU, 0x30, RELATIVE, STD_OP, bmi);
    map_operation(newCPU, 0x50, RELATIVE, STD_OP, bvc);
    map_operation(newCPU, 0x70, RELATIVE, STD_OP, bvs);
    map_operation(newCPU, 0x90, RELATIVE, STD_OP, bcc);
    map_operation(newCPU, 0xb0, RELATIVE, STD_OP, bcs);
    map_operation(newCPU, 0xd0, RELATIVE, STD_OP, bne);
    map_operation(newCPU, 0xf0, RELATIVE, STD_OP, beq);
    map_operation(newCPU, 0x09, IMMEDIATE, STD_OP, ora);
    map_operation(newCPU, 0x29, IMMEDIATE, STD_OP, and);
    map_operation(newCPU, 0x49, IMMEDIATE, STD_OP, eor);
    map_operation(newCPU, 0x69, IMMEDIATE, STD_OP, adc);
    map_operation(newCPU, 0xa9, IMMEDIATE, STD_OP, lda);
    map_operation(newCPU, 0xc9, IMMEDIATE, STD_OP, cmp);
    map_operation(newCPU, 0xe9, IMMEDIATE, STD_OP, sbc);
    map_operation(newCPU, 0xc0, IMMEDIATE, STD_OP, cpy);
    map_operation(newCPU, 0xe0, IMMEDIATE, STD_OP, cpx);
    map_operation(newCPU, 0xa0, IMMEDIATE, STD_OP, ldy);
    map_operation(newCPU, 0xa2, IMMEDIATE, STD_OP, ldx);
    map_operation(newCPU, 0x05, ZEROPAGE, STD_OP, ora);
    map_operation(newCPU, 0x25, ZEROPAGE, STD_OP, and);
    map_operation(newCPU, 0x45, ZEROPAGE, STD_OP, eor);
    map_operation(newCPU, 0x65, ZEROPAGE, STD_OP, adc);
    map_operation(newCPU, 0xa5, ZEROPAGE, STD_OP, lda);
    map_operation(newCPU, 0xc5, ZEROPAGE, STD_OP, cmp);
    map_operation(newCPU, 0xe5, ZEROPAGE, STD_OP, sbc);
    map_operation(newCPU, 0xc4, ZEROPAGE, STD_OP, cpy);
    map_operation(newCPU, 0xe4, ZEROPAGE, STD_OP, cpx);
    map_operation(newCPU, 0xa4, ZEROPAGE, STD_OP, ldy);
    map_operation(newCPU, 0xa6, ZEROPAGE, STD_OP, ldx);
    map_operation(newCPU, 0x24, ZEROPAGE, STD_OP, bit);
    map_operation(newCPU, 0x85, ZEROPAGE, STD_OP, sta);
    map_operation(newCPU, 0x86, ZEROPAGE, STD_OP, stx);
    map_operation(newCPU, 0x84, ZEROPAGE, STD_OP, sty);
    map_operation(newCPU, 0x06, ZEROPAGE, RMW_OP, asl);
    map_operation(newCPU, 0x46, ZEROPAGE, RMW_OP, lsr);
    map_operation(newCPU, 0x26, ZEROPAGE, RMW_OP, rol);
    map_operation(newCPU, 0x66, ZEROPAGE, RMW_OP, ror);
    map_operation(newCPU, 0xe6, ZEROPAGE, RMW_OP, inc);
    map_operation(newCPU, 0xc6, ZEROPAGE, RMW_OP, dec);
    map_operation(newCPU, 0x15, ZEROPAGE_X, STD_OP, ora);
    map_operation(newCPU, 0x35, ZEROPAGE_X, STD_OP, and);
    map_operation(newCPU, 0x55, ZEROPAGE_X, STD_OP, eor);
    map_operation(newCPU, 0x75, ZEROPAGE_X, STD_OP, adc);
    map_operation(newCPU, 0xb5, ZEROPAGE_X, STD_OP, lda);
    map_operation(newCPU, 0xd5, ZEROPAGE_X, STD_OP, cmp);
    map_operation(newCPU, 0xf5, ZEROPAGE_X, STD_OP, sbc);
    map_operation(newCPU, 0xb6, ZEROPAGE_Y, STD_OP, ldx);
    map_operation(newCPU, 0xb4, ZEROPAGE_X, STD_OP, ldy);
    map_operation(newCPU, 0x95, ZEROPAGE_X, STD_OP, sta);
    map_operation(newCPU, 0x96, ZEROPAGE_Y, STD_OP, stx);
    map_operation(newCPU, 0x94, ZEROPAGE_X, STD_OP, sty);
    map_operation(newCPU, 0x16, ZEROPAGE_X, RMW_OP, asl);
    map_operation(newCPU, 0x56, ZEROPAGE_X, RMW_OP, lsr);
    map_operation(newCPU, 0x36, ZEROPAGE_X, RMW_OP, rol);
    map_operation(newCPU, 0x76, ZEROPAGE_X, RMW_OP, ror);
    map_operation(newCPU, 0xf6, ZEROPAGE_X, RMW_OP, inc);
    map_operation(newCPU, 0xd6, ZEROPAGE_X, RMW_OP, dec);
    map_operation(newCPU, 0x0d, ABSOLUTE, STD_OP, ora);
    map_operation(newCPU, 0x2d, ABSOLUTE, STD_OP, and);
    map_operation(newCPU, 0x4d, ABSOLUTE, STD_OP, eor);
    map_operation(newCPU, 0x6d, ABSOLUTE, STD_OP, adc);
    map_operation(newCPU, 0xad, ABSOLUTE, STD_OP, lda);
    map_operation(newCPU, 0xcd, ABSOLUTE, STD_OP, cmp);
    map_operation(newCPU, 0xed, ABSOLUTE, STD_OP, sbc);
    map_operation(newCPU, 0xcc, ABSOLUTE, STD_OP, cpy);
    map_operation(newCPU, 0xec, ABSOLUTE, STD_OP, cpx);
    map_operation(newCPU, 0xac, ABSOLUTE, STD_OP, ldy);
    map_operation(newCPU, 0xae, ABSOLUTE, STD_OP, ldx);
    map_operation(newCPU, 0x2c, ABSOLUTE, STD_OP, bit);
    map_operation(newCPU, 0x8d, ABSOLUTE, STD_OP, sta);
    map_operation(newCPU, 0x8e, ABSOLUTE, STD_OP, stx);
    map_operation(newCPU, 0x8c, ABSOLUTE, STD_OP, sty);
    map_operation(newCPU, 0x0e, ABSOLUTE, RMW_OP, asl);
    map_operation(newCPU, 0x4e, ABSOLUTE, RMW_OP, lsr);
    map_operation(newCPU, 0x2e, ABSOLUTE, RMW_OP, rol);
    map_operation(newCPU, 0x6e, ABSOLUTE, RMW_OP, ror);
    map_operation(newCPU, 0xee, ABSOLUTE, RMW_OP, inc);
    map_operation(newCPU, 0xce, ABSOLUTE, RMW_OP, dec);
    map_operation(newCPU, 0x1d, ABSOLUTE_X, STD_OP, ora);
    map_operation(newCPU, 0x3d, ABSOLUTE_X, STD_OP, and);
    map_operation(newCPU, 0x5d, ABSOLUTE_X, STD_OP, eor);
    map_operation(newCPU, 0x7d, ABSOLUTE_X, STD_OP, adc);
    map_operation(newCPU, 0xbd, ABSOLUTE_X, STD_OP, lda);
    map_operation(newCPU, 0xdd, ABSOLUTE_X, STD_OP, cmp);
    map_operation(newCPU, 0xfd, ABSOLUTE_X, STD_OP, sbc);
    map_operation(newCPU, 0xbc, ABSOLUTE_X, STD_OP, ldy);
    map_operation(newCPU, 0x9d, ABSOLUTE_X, STD_OP, sta);
    map_operation(newCPU, 0x1e, ABSOLUTE_X, RMW_OP, asl);
    map_operation(newCPU, 0x5e, ABSOLUTE_X, RMW_OP, lsr);
    map_operation(newCPU, 0x3e, ABSOLUTE_X, RMW_OP, rol);
    map_operation(newCPU, 0x7e, ABSOLUTE_X, RMW_OP, ror);
    map_operation(newCPU, 0xfe, ABSOLUTE_X, RMW_OP, inc);
    map_operation(newCPU, 0xde, ABSOLUTE_X, RMW_OP, dec);
    map_operation(newCPU, 0x19, ABSOLUTE_Y, STD_OP, ora);
    map_operation(newCPU, 0x39, ABSOLUTE_Y, STD_OP, and);
    map_operation(newCPU, 0x59, ABSOLUTE_Y, STD_OP, eor);
    map_operation(newCPU, 0x79, ABSOLUTE_Y, STD_OP, adc);
    map_operation(newCPU, 0xb9, ABSOLUTE_Y, STD_OP, lda);
    map_operation(newCPU, 0xd9, ABSOLUTE_Y, STD_OP, cmp);
    map_operation(newCPU, 0xf9, ABSOLUTE_Y, STD_OP, sbc);
    map_operation(newCPU, 0xbe, ABSOLUTE_Y, STD_OP, ldx);
    map_operation(newCPU, 0x99, ABSOLUTE_Y, STD_OP, sta);
    map_operation(newCPU, 0x01, INDIRECT_X, STD_OP, ora);
    map_operation(newCPU, 0x21, INDIRECT_X, STD_OP, and);
    map_operation(newCPU, 0x41, INDIRECT_X, STD_OP, eor);
    map_operation(newCPU, 0x61, INDIRECT_X, STD_OP, adc);
    map_operation(newCPU, 0xa1, INDIRECT_X, STD_OP, lda);
    map_operation(newCPU, 0xc1, INDIRECT_X, STD_OP, cmp);
    map_operation(newCPU, 0xe1, INDIRECT_X, STD_OP, sbc);
    map_operation(newCPU, 0x81, INDIRECT_X, STD_OP, sta);
    map_operation(newCPU, 0x11, INDIRECT_Y, STD_OP, ora);
    map_operation(newCPU, 0x31, INDIRECT_Y, STD_OP, and);
    map_operation(newCPU, 0x51, INDIRECT_Y, STD_OP, eor);
    map_operation(newCPU, 0x71, INDIRECT_Y, STD_OP, adc);
    map_operation(newCPU, 0xb1, INDIRECT_Y, STD_OP, lda);
    map_operation(newCPU, 0xd1, INDIRECT_Y, STD_OP, cmp);
    map_operation(newCPU, 0xf1, INDIRECT_Y, STD_OP, sbc);
    map_operation(newCPU, 0x91, INDIRECT_Y, STD_OP, sta);

    // ILLEGAL OPS
    map_operation(newCPU, 0x1a, IMPLIED, STD_OP, nop);
    map_operation(newCPU, 0x3a, IMPLIED, STD_OP, nop);
    map_operation(newCPU, 0x5a, IMPLIED, STD_OP, nop);
    map_operation(newCPU, 0x7a, IMPLIED, STD_OP, nop);
    map_operation(newCPU, 0xda, IMPLIED, STD_OP, nop);
    map_operation(newCPU, 0xfa, IMPLIED, STD_OP, nop);
    map_operation(newCPU, 0x80, IMMEDIATE, STD_OP, nop);
    map_operation(newCPU, 0x82, IMMEDIATE, STD_OP, nop);
    map_operation(newCPU, 0x89, IMMEDIATE, STD_OP, nop);
    map_operation(newCPU, 0xc2, IMMEDIATE, STD_OP, nop);
    map_operation(newCPU, 0xe2, IMMEDIATE, STD_OP, nop);
    map_operation(newCPU, 0xeb, IMMEDIATE, STD_OP, sbc);
    map_operation(newCPU, 0x0b, IMMEDIATE, STD_OP, anc);
    map_operation(newCPU, 0x2b, IMMEDIATE, STD_OP, anc);
    map_operation(newCPU, 0x4b, IMMEDIATE, STD_OP, alr);
    map_operation(newCPU, 0x6b, IMMEDIATE, STD_OP, arr);
    map_operation(newCPU, 0xab, IMMEDIATE, STD_OP, atx);
    map_operation(newCPU, 0xcb, IMMEDIATE, STD_OP, axs);
    map_operation(newCPU, 0x04, ZEROPAGE, STD_OP, nop);
    map_operation(newCPU, 0x44, ZEROPAGE, STD_OP, nop);
    map_operation(newCPU, 0x64, ZEROPAGE, STD_OP, nop);
    map_operation(newCPU, 0x87, ZEROPAGE, STD_OP, aax);
    map_operation(newCPU, 0xa7, ZEROPAGE, STD_OP, lax);
    map_operation(newCPU, 0x07, ZEROPAGE, RMW_COMBO_OP, slo);
    map_operation(newCPU, 0x27, ZEROPAGE, RMW_COMBO_OP, rla);
    map_operation(newCPU, 0x47, ZEROPAGE, RMW_COMBO_OP, sre);
    map_operation(newCPU, 0x67, ZEROPAGE, RMW_COMBO_OP, rra);
    map_operation(newCPU, 0xc7, ZEROPAGE, RMW_COMBO_OP, dcp);
    map_operation(newCPU, 0xe7, ZEROPAGE, RMW_COMBO_OP, isc);
    map_operation(newCPU, 0x14, ZEROPAGE_X, STD_OP, nop);
    map_operation(newCPU, 0x34, ZEROPAGE_X, STD_OP, nop);
    map_operation(newCPU, 0x54, ZEROPAGE_X, STD_OP, nop);
    map_operation(newCPU, 0x74, ZEROPAGE_X, STD_OP, nop);
    map_operation(newCPU, 0xd4, ZEROPAGE_X, STD_OP, nop);
    map_operation(newCPU, 0xf4, ZEROPAGE_X, STD_OP, nop);
    map_operation(newCPU, 0x97, ZEROPAGE_Y, STD_OP, aax);
    map_operation(newCPU, 0xb7, ZEROPAGE_Y, STD_OP, lax);
    map_operation(newCPU, 0x17, ZEROPAGE_X, RMW_COMBO_OP, slo);
    map_operation(newCPU, 0x37, ZEROPAGE_X, RMW_COMBO_OP, rla);
    map_operation(newCPU, 0x57, ZEROPAGE_X, RMW_COMBO_OP, sre);
    map_operation(newCPU, 0x77, ZEROPAGE_X, RMW_COMBO_OP, rra);
    map_operation(newCPU, 0xd7, ZEROPAGE_X, RMW_COMBO_OP, dcp);
    map_operation(newCPU, 0xf7, ZEROPAGE_X, RMW_COMBO_OP, isc);
    map_operation(newCPU, 0x0c, ABSOLUTE, STD_OP, nop);
    map_operation(newCPU, 0x8f, ABSOLUTE, STD_OP, aax);
    map_operation(newCPU, 0xaf, ABSOLUTE, STD_OP, lax);
    map_operation(newCPU, 0x0f, ABSOLUTE, RMW_COMBO_OP, slo);
    map_operation(newCPU, 0x2f, ABSOLUTE, RMW_COMBO_OP, rla);
    map_operation(newCPU, 0x4f, ABSOLUTE, RMW_COMBO_OP, sre);
    map_operation(newCPU, 0x6f, ABSOLUTE, RMW_COMBO_OP, rra);
    map_operation(newCPU, 0xcf, ABSOLUTE, RMW_COMBO_OP, dcp);
    map_operation(newCPU, 0xef, ABSOLUTE, RMW_COMBO_OP, isc);
    map_operation(newCPU, 0x1c, ABSOLUTE_X, STD_OP, nop);
    map_operation(newCPU, 0x3c, ABSOLUTE_X, STD_OP, nop);
    map_operation(newCPU, 0x5c, ABSOLUTE_X, STD_OP, nop);
    map_operation(newCPU, 0x7c, ABSOLUTE_X, STD_OP, nop);
    map_operation(newCPU, 0xdc, ABSOLUTE_X, STD_OP, nop);
    map_operation(newCPU, 0xfc, ABSOLUTE_X, STD_OP, nop);
    map_operation(newCPU, 0x9c, ABSOLUTE_X, STD_OP, sya);
    map_operation(newCPU, 0x1f, ABSOLUTE_X, RMW_COMBO_OP, slo);
    map_operation(newCPU, 0x3f, ABSOLUTE_X, RMW_COMBO_OP, rla);
    map_operation(newCPU, 0x5f, ABSOLUTE_X, RMW_COMBO_OP, sre);
    map_operation(newCPU, 0x7f, ABSOLUTE_X, RMW_COMBO_OP, rra);
    map_operation(newCPU, 0xdf, ABSOLUTE_X, RMW_COMBO_OP, dcp);
    map_operation(newCPU, 0xff, ABSOLUTE_X, RMW_COMBO_OP, isc);
    map_operation(newCPU, 0x9e, ABSOLUTE_Y, STD_OP, sxa);
    map_operation(newCPU, 0xbf, ABSOLUTE_Y, STD_OP, lax);
    map_operation(newCPU, 0x1b, ABSOLUTE_Y, RMW_COMBO_OP, slo);
    map_operation(newCPU, 0x3b, ABSOLUTE_Y, RMW_COMBO_OP, rla);
    map_operation(newCPU, 0x5b, ABSOLUTE_Y, RMW_COMBO_OP, sre);
    map_operation(newCPU, 0x7b, ABSOLUTE_Y, RMW_COMBO_OP, rra);
    map_operation(newCPU, 0xdb, ABSOLUTE_Y, RMW_COMBO_OP, dcp);
    map_operation(newCPU, 0xfb, ABSOLUTE_Y, RMW_COMBO_OP, isc);
    map_operation(newCPU, 0x83, INDIRECT_X, STD_OP, aax);
    map_operation(newCPU, 0xa3, INDIRECT_X, STD_OP, lax);
    map_operation(newCPU, 0x03, INDIRECT_X, RMW_COMBO_OP, slo);
    map_operation(newCPU, 0x23, INDIRECT_X, RMW_COMBO_OP, rla);
    map_operation(newCPU, 0x43, INDIRECT_X, RMW_COMBO_OP, sre);
    map_operation(newCPU, 0x63, INDIRECT_X, RMW_COMBO_OP, rra);
    map_operation(newCPU, 0xc3, INDIRECT_X, RMW_COMBO_OP, dcp);
    map_operation(newCPU, 0xe3, INDIRECT_X, RMW_COMBO_OP, isc);
    map_operation(newCPU, 0xb3, INDIRECT_Y, STD_OP, lax);
    map_operation(newCPU, 0x13, INDIRECT_Y, RMW_COMBO_OP, slo);
    map_operation(newCPU, 0x33, INDIRECT_Y, RMW_COMBO_OP, rla);
    map_operation(newCPU, 0x53, INDIRECT_Y, RMW_COMBO_OP, sre);
    map_operation(newCPU, 0x73, INDIRECT_Y, RMW_COMBO_OP, rra);
    map_operation(newCPU, 0xd3, INDIRECT_Y, RMW_COMBO_OP, dcp);
    map_operation(newCPU, 0xf3, INDIRECT_Y, RMW_COMBO_OP, isc);

    return newCPU;
}

void cpu_reset(cpu* c) {
    c->flags |= IRQ_DISABLE;
    *c->pcL = cpu_readMemory(c, 0xfffc);
    *c->pcH = cpu_readMemory(c, 0xfffd);
}

void cpu_printState(cpu* c) {
    printf("OP:%s (%02hhx)", instrs[c->currentOp], c->currentOp);
    printf(" PC:%04hx", c->pc);
    printf(" A:%02hhx", c->accumulator);
    printf(" X:%02hhx", c->x);
    printf(" Y:%02hhx", c->y);
    if (c->flags & NEGATIVE)
        printf(" N");
    else
        printf(" n");
    if (c->flags & OVERFLOW)
        printf("V");
    else
        printf("v");
    if (c->flags & IRQ_DISABLE)
        printf("I");
    else
        printf("i");
    if (c->flags & ZERO)
        printf("Z");
    else
        printf("z");
    if (c->flags & CARRY)
        printf("C ");
    else
        printf("c ");
    printf(" Stack: ");
    for (int i = 0xff; i > c->stackPointer; i--) {
        printf("%02hhx, ", *c->memory_read[0x100 + i]);
    }
}

void cpu_executeCycle(cpu* c) {
    c->tick++;
    c->write = false;
    c->ops[c->currentOp][c->tick](c);
    if (!c->nmi_pending && !c->nmi_executing && !c->nmi_starting)
        c->nmi_pending = (*c->nmi && !c->nmi_prev);
    c->irq_pending = *c->irq;
    c->nmi_prev = *c->nmi;
}
