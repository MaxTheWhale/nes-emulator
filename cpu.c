#include "cpu.h"
#include <stdio.h>
#include <stdlib.h>

const char instrs[256][4] = { "BRK", "ORA", "XXX", "SLO", "DOP", "ORA", "ASL", "SLO",
							  "PHP", "ORA", "ASL", "AAC", "TOP", "ORA", "ASL", "SLO", 
							  "BPL", "ORA", "XXX", "SLO", "DOP", "ORA", "ASL", "SLO", 
							  "CLC", "ORA", "NOP", "SLO", "TOP", "ORA", "ASL", "SLO", 
							  "JSR", "AND", "XXX", "RLA", "BIT", "AND", "ROL", "RLA", 
							  "PLP", "AND", "ROL", "AAC", "BIT", "AND", "ROL", "RLA", 
							  "BMI", "AND", "XXX", "RLA", "DOP", "AND", "ROL", "RLA", 
							  "SEC", "AND", "NOP", "RLA", "TOP", "AND", "ROL", "RLA",
							  "RTI", "EOR", "XXX", "SRE", "DOP", "EOR", "LSR", "SRE",
							  "PHA", "EOR", "LSR", "ASR", "JMP", "EOR", "LSR", "SRE", 
							  "BVC", "EOR", "XXX", "SRE", "DOP", "EOR", "LSR", "SRE", 
							  "CLI", "EOR", "NOP", "SRE", "TOP", "EOR", "LSR", "SRE", 
							  "RTS", "ADC", "XXX", "RRA", "DOP", "ADC", "ROR", "RRA", 
							  "PLA", "ADC", "ROR", "ARR", "JMP", "ADC", "ROR", "RRA", 
							  "BVS", "ADC", "XXX", "RRA", "DOP", "ADC", "ROR", "RRA", 
							  "SEI", "ADC", "NOP", "RRA", "TOP", "ADC", "ROR", "RRA", 
							  "DOP", "STA", "DOP", "AAX", "STY", "STA", "STX", "AAX", 
							  "DEY", "DOP", "TXA", "???", "STY", "STA", "STX", "AAX", 
							  "BCC", "STA", "XXX", "???", "STY", "STA", "STX", "AAX",
							  "TYA", "STA", "TXS", "???", "SYA", "STA", "SXA", "???",
							  "LDY", "LDA", "LDX", "LAX", "LDY", "LDA", "LDX", "LAX",
							  "TAY", "LDA", "TAX", "ATX", "LDY", "LDA", "LDX", "LAX",
							  "BCS", "LDA", "XXX", "LAX", "LDY", "LDA", "LDX", "LAX",
							  "CLV", "LDA", "TSX", "???", "LDY", "LDA", "LDX", "LAX",
							  "CPY", "CMP", "DOP", "DCP", "CPY", "CMP", "DEC", "DCP",
							  "INY", "CMP", "DEX", "AXS", "CPY", "CMP", "DEC", "DCP",
							  "BNE", "CMP", "XXX", "DCP", "DOP", "CMP", "DEC", "DCP",
							  "CLD", "CMP", "NOP", "DCP", "TOP", "CMP", "DEC", "DCP",
							  "CPX", "SBC", "DOP", "ISC", "CPX", "SBC", "INC", "ISC",
							  "INX", "SBC", "NOP", "SBC", "CPX", "SBC", "INC", "ISC",
							  "BEQ", "SBC", "XXX", "ISC", "DOP", "SBC", "INC", "ISC",
							  "SED", "SBC", "NOP", "ISC", "TOP", "SBC", "INC", "ISC" };

inline void setCarry(uint8_t *p)       { *p |= 1; }
inline void setZero(uint8_t *p)        { *p |= 2; }
inline void setInterrupt(uint8_t *p)   { *p |= 4; }
inline void setDecimal(uint8_t *p)     { *p |= 8; }
inline void setOverflow(uint8_t *p)    { *p |= 64; }
inline void setNegative(uint8_t *p)    { *p |= 128; }

inline void clearCarry(uint8_t *p)     { *p &= 254; }
inline void clearZero(uint8_t *p)      { *p &= 253; }
inline void clearInterrupt(uint8_t *p) { *p &= 251; }
inline void clearDecimal(uint8_t *p)   { *p &= 247; }
inline void clearOverflow(uint8_t *p)  { *p &= 191; }
inline void clearNegative(uint8_t *p)  { *p &= 127; }

inline uint8_t checkCarry(uint8_t p)     { return p & 1; }
inline uint8_t checkZero(uint8_t p)      { return p & 2; }
inline uint8_t checkInterrupt(uint8_t p) { return p & 4; }
inline uint8_t checkDecimal(uint8_t p)   { return p & 8; }
inline uint8_t checkOverflow(uint8_t p)  { return p & 64; }
inline uint8_t checkNegative(uint8_t p)  { return p & 128; }

struct cpu
{
    uint8_t accumulator;
    uint8_t x;
    uint8_t y;
    uint8_t stackPointer;
	uint8_t temp;
	uint8_t dummy;
    uint8_t flags;
    uint16_t pc;
	uint16_t address;
	bool write;
    uint8_t *pcL;
    uint8_t *pcH;
	uint16_t ad;
    uint8_t *adL;
    uint8_t *adH;
	uint8_t currentOp;
	uint8_t tick;
	bool *nmi;
	bool *irq;
	bool nmi_prev;
	bool nmi_pending;
	bool irq_pending;
	bool nmi_executing;
	bool nmi_starting;

	opPtr ops[256][8];

    uint8_t *memory_read[0x10000];
	uint8_t *memory_write[0x10000];
};
typedef void (*opPtr)(cpu*);

uint8_t readMemory(cpu *c, uint16_t address)
{
	c->address = address;
	return *c->memory_read[address];
}
void writeMemory(cpu *c, uint16_t address, uint8_t value)
{
	c->address = address;
	c->write = true;
	*c->memory_write[address] = value;
}

void fetchOp(cpu *c)
{
    c->currentOp = readMemory(c, c->pc);
	if (c->nmi_pending)
	{
		c->currentOp = 0;
		c->nmi_starting = true;
	}
	else
		c->pc++;
}

void loadRegister(cpu *c, uint8_t *reg, uint8_t value)
{
	if (value == 0) setZero(&c->flags); else clearZero(&c->flags);
	if ((value & 0x80) == 0) clearNegative(&c->flags); else setNegative(&c->flags);
	*reg = value;
}

void writeValue(cpu *c, uint16_t address, uint8_t value)
{
	if (value == 0) setZero(&c->flags); else clearZero(&c->flags);
	if ((value & 0x80) == 0) clearNegative(&c->flags); else setNegative(&c->flags);
	writeMemory(c, address, value);
}

void fetchPCHbrk(cpu *c)
{
	if (c->nmi_starting)
	{
		*c->pcH = readMemory(c, 0xfffb);
		c->nmi_pending = false;
		c->nmi_starting = false;
		c->nmi_executing = true;
	}
	else
		*c->pcH = readMemory(c, 0xffff);
	c->tick = 0xff;
}

void fetchPCLbrk(cpu *c)
{
	if (c->nmi_starting)
		*c->pcL = readMemory(c, 0xfffa);
	else
		*c->pcL = readMemory(c, 0xfffe);
	setInterrupt(&c->flags);
}

void fetchADL(cpu *c)
{
	*c->adH = 0;
	*c->adL = readMemory(c, c->pc);
	c->pc++;
}

void fetchADH(cpu *c)
{
	*c->adH = readMemory(c, c->pc);
	c->pc++;
}

void readAddress(cpu *c)
{
	c->temp = readMemory(c, c->ad);
}

void writeAddress(cpu *c)
{
	writeMemory(c, c->ad, c->temp);
}
void fetchIndirectY(cpu *c)
{
	(*c->adL)++;
	*c->adH = readMemory(c, c->ad);
	*c->adL = c->temp + c->y;
}
void fetchIndirectXLow(cpu *c)
{
	*c->adL += c->x;
	c->temp = readMemory(c, c->ad);
}
void fetchIndirectXHigh(cpu *c)
{
	(*c->adL)++;
	*c->adH = readMemory(c, c->ad);
	*c->adL = c->temp;
}

void readIndirect(cpu *c)
{
	if (*c->adL < c->y)
	{
		c->temp = readMemory(c, c->ad);
		(*c->adH)++;
	}
	else
	{
		c->ops[c->currentOp][5](c);
	}
}
void readZpX(cpu *c)
{
	c->temp = readMemory(c, c->ad);
	*c->adL += c->x;
}
void readZpY(cpu *c)
{
	c->temp = readMemory(c, c->ad);
	*c->adL += c->y;
}
void readAbsX(cpu *c)
{
	*c->adL += c->x;
	if (*c->adL < c->x)
	{
		c->temp = readMemory(c, c->ad);
		(*c->adH)++;
	}
	else
	{
		c->ops[c->currentOp][4](c);
	}
}
void readAbsY(cpu *c)
{
	*c->adL += c->y;
	if (*c->adL < c->y)
	{
		c->temp = readMemory(c, c->ad);
		(*c->adH)++;
	}
	else
	{
		c->ops[c->currentOp][4](c);
	}
}
void writeAbsX(cpu *c)
{
	*c->adL += c->x;
	c->temp = readMemory(c, c->ad);
	if (*c->adL < c->x)
	{
		(*c->adH)++;
	}
}
void writeAbsY(cpu *c)
{
	*c->adL += c->y;
	c->temp = readMemory(c, c->ad);
	if (*c->adL < c->y)
	{
		(*c->adH)++;
	}
}

void writeIndirect(cpu *c)
{
	c->temp = readMemory(c, c->ad);
	if (*c->adL < c->y) (*c->adH)++;
}

void pushBStatus(cpu *c)
{
	if (c->nmi_starting)
		writeMemory(c, 0x100 + c->stackPointer, (c->flags & ~0x10) | 0x20);
	else
		writeMemory(c, 0x100 + c->stackPointer, c->flags | 0x30);
	c->stackPointer--;
}

void pushPCL(cpu *c)
{
	writeMemory(c, 0x100 + c->stackPointer, *c->pcL);
	c->stackPointer--;
	if (c->currentOp == 0 && c->nmi_pending)
		c->nmi_starting = true;
}
void pushPCH(cpu *c)
{
	writeMemory(c, 0x100 + c->stackPointer, *c->pcH);
	c->stackPointer--;
}
void pullPCL(cpu *c)
{
	*c->pcL = readMemory(c, 0x100 + c->stackPointer);
	c->stackPointer++;
}
void pullPCH(cpu *c)
{
	*c->pcH = readMemory(c, 0x100 + c->stackPointer);
}
void rtiPullPCH(cpu *c)
{
	*c->pcH = readMemory(c, 0x100 + c->stackPointer);
	if (c->nmi_executing)
	{
		c->nmi_executing = false;
		c->nmi_pending = false;
	}
	c->tick = 0xff;
}
void pullFlags(cpu *c)
{
	c->flags = readMemory(c, 0x100 + c->stackPointer);
	c->stackPointer++;
}
void incPC(cpu *c)
{
	c->pc++;
	c->tick = 0xff;
}

void fetchValue(cpu *c)
{
	c->temp = readMemory(c, c->pc);
	if (!c->nmi_starting) c->pc++;
}
void readValue(cpu *c)
{
	c->temp = readMemory(c, c->pc);
}

void readStack(cpu *c)
{
	c->temp = readMemory(c, 0x100 + c->stackPointer);
}

void jmp(cpu *c)
{
	*c->pcH = readMemory(c, c->pc);
	*c->pcL = *c->adL;
	c->tick = 0xff;
}
void jmpIndirect(cpu *c)
{
	(*c->adL)++;
	*c->pcH = readMemory(c, c->ad);
	*c->pcL = c->temp;
	c->tick = 0xff;
}
void pla(cpu *c)
{
	loadRegister(c, &c->accumulator, readMemory(c, 0x100 + c->stackPointer));
	c->tick = 0xff;
}
void plp(cpu *c)
{
	c->flags = readMemory(c, 0x100 + c->stackPointer);
	c->tick = 0xff;
}
void pha(cpu *c)
{
	writeMemory(c, 0x100 + c->stackPointer, c->accumulator);
	c->stackPointer--;
	c->tick = 0xff;
}
void php(cpu *c)
{
	writeMemory(c, 0x100 + c->stackPointer, c->flags | 0x30);
	c->stackPointer--;
	c->tick = 0xff;
}
void ins(cpu *c)
{
	c->stackPointer++;
}
void nop(cpu *c)
{
	c->temp = readMemory(c, c->pc);
	c->tick = 0xff;
}

void tax(cpu *c)
{
	loadRegister(c, &c->x, c->accumulator);
	c->tick = 0xff;
}

void tay(cpu *c)
{
	loadRegister(c, &c->y, c->accumulator);
	c->tick = 0xff;
}

void tsx(cpu *c)
{
	loadRegister(c, &c->x, c->stackPointer);
	c->tick = 0xff;
}

void txa(cpu *c)
{
	loadRegister(c, &c->accumulator, c->x);
	c->tick = 0xff;
}

void txs(cpu *c)
{
	c->stackPointer = c->x;
	c->tick = 0xff;
}

void tya(cpu *c)
{
	loadRegister(c, &c->accumulator, c->y);
	c->tick = 0xff;
}

void sec(cpu *c)
{
    setCarry(&c->flags);
	c->tick = 0xff;
}
void sei(cpu *c)
{
    setInterrupt(&c->flags);
	c->tick = 0xff;
}
void sed(cpu *c)
{
    setDecimal(&c->flags);
	c->tick = 0xff;
}
void clc(cpu *c)
{
    clearCarry(&c->flags);
	c->tick = 0xff;
}
void cli(cpu *c)
{
    clearInterrupt(&c->flags);
	c->tick = 0xff;
}
void cld(cpu *c)
{
    clearDecimal(&c->flags);
	c->tick = 0xff;
}
void clv(cpu *c)
{
    clearOverflow(&c->flags);
	c->tick = 0xff;
}
void inx(cpu *c)
{
	loadRegister(c, &c->x, c->x + 1);
	c->tick = 0xff;
}
void iny(cpu *c)
{
	loadRegister(c, &c->y, c->y + 1);
	c->tick = 0xff;
}
void dex(cpu *c)
{
	loadRegister(c, &c->x, c->x - 1);
	c->tick = 0xff;
}
void dey(cpu *c)
{
	loadRegister(c, &c->y, c->y - 1);
	c->tick = 0xff;
}
void lda(cpu *c)
{
	loadRegister(c, &c->accumulator, c->temp);
	c->tick = 0xff;
}
void ldaImmediate(cpu *c)
{
	fetchValue(c);
	lda(c);
}
void ldaMemory(cpu *c)
{
	c->temp = readMemory(c, c->ad);
	lda(c);
}
void ldx(cpu *c)
{
	loadRegister(c, &c->x, c->temp);
	c->tick = 0xff;
}
void ldxImmediate(cpu *c)
{
	fetchValue(c);
	ldx(c);
}
void ldxMemory(cpu *c)
{
	c->temp = readMemory(c, c->ad);
	ldx(c);
}
void ldy(cpu *c)
{
	loadRegister(c, &c->y, c->temp);
	c->tick = 0xff;
}

void ldyImmediate(cpu *c)
{
	fetchValue(c);
	ldy(c);
}
void ldyMemory(cpu *c)
{
	c->temp = readMemory(c, c->ad);
	ldy(c);
}
void lax(cpu *c)
{
	c->temp = readMemory(c, c->ad);
	loadRegister(c, &c->accumulator, c->temp);
	loadRegister(c, &c->x, c->temp);
	c->tick = 0xff;
}
void eor(cpu *c)
{
	loadRegister(c, &c->accumulator, (c->temp ^ c->accumulator));
	c->tick = 0xff;
}
void eorImmediate(cpu *c)
{
	fetchValue(c);
	eor(c);
}
void eorMemory(cpu *c)
{
	c->temp = readMemory(c, c->ad);
	eor(c);
}
void and(cpu *c)
{
	loadRegister(c, &c->accumulator, (c->temp & c->accumulator));
	c->tick = 0xff;
}
void andImmediate(cpu *c)
{
	fetchValue(c);
	and(c);
}
void andMemory(cpu *c)
{
	c->temp = readMemory(c, c->ad);
	and(c);
}
void ora(cpu *c)
{
	loadRegister(c, &c->accumulator, (c->temp | c->accumulator));
	c->tick = 0xff;
}
void oraImmediate(cpu *c)
{
	fetchValue(c);
	ora(c);
}
void oraMemory(cpu *c)
{
	c->temp = readMemory(c, c->ad);
	ora(c);
}
void adc(cpu *c)
{
	uint16_t sum = c->accumulator + c->temp + checkCarry(c->flags);
	uint8_t result = sum & 0xff;
	if (sum & 0x100)
		setCarry(&c->flags);
	else
		clearCarry(&c->flags);
	if ((c->accumulator ^ result) & (c->temp ^ result) & 0x80)
		setOverflow(&c->flags);
	else
		clearOverflow(&c->flags);
	loadRegister(c, &c->accumulator, result);
	c->tick = 0xff;
}
void adcImmediate(cpu *c)
{
	fetchValue(c);
	adc(c);
}
void adcMemory(cpu *c)
{
	c->temp = readMemory(c, c->ad);
	adc(c);
}
void sbcImmediate(cpu *c)
{
	fetchValue(c);
	c->temp = ~c->temp;
	adc(c);
}
void sbcMemory(cpu *c)
{
	c->temp = readMemory(c, c->ad);
	c->temp = ~c->temp;
	adc(c);
}
void cmp(cpu *c)
{
	if (c->temp <= c->accumulator) setCarry(&c->flags);
	else clearCarry(&c->flags);
	if (c->temp == c->accumulator) setZero(&c->flags);
	else clearZero(&c->flags);
	if ((c->accumulator - c->temp) & 0x80) setNegative(&c->flags);
	else clearNegative(&c->flags);
	c->tick = 0xff;
}
void cpx(cpu *c)
{
	if (c->temp <= c->x) setCarry(&c->flags);
	else clearCarry(&c->flags);
	if (c->temp == c->x) setZero(&c->flags);
	else clearZero(&c->flags);
	if ((c->x - c->temp) & 0x80) setNegative(&c->flags);
	else clearNegative(&c->flags);
	c->tick = 0xff;
}
void cpy(cpu *c)
{
	if (c->temp <= c->y) setCarry(&c->flags);
	else clearCarry(&c->flags);
	if (c->temp == c->y) setZero(&c->flags);
	else clearZero(&c->flags);
	if ((c->y - c->temp) & 0x80) setNegative(&c->flags);
	else clearNegative(&c->flags);
	c->tick = 0xff;
}
void cmpImmediate(cpu *c)
{
	fetchValue(c);
	cmp(c);
}
void cmpMemory(cpu *c)
{
	c->temp = readMemory(c, c->ad);
	cmp(c);
}
void cpxImmediate(cpu *c)
{
	fetchValue(c);
	cpx(c);
}
void cpxMemory(cpu *c)
{
	c->temp = readMemory(c, c->ad);
	cpx(c);
}
void cpyImmediate(cpu *c)
{
	fetchValue(c);
	cpy(c);
}
void cpyMemory(cpu *c)
{
	c->temp = readMemory(c, c->ad);
	cpy(c);
}
void bit(cpu *c)
{
	uint8_t val = readMemory(c, c->ad);
	if (val & c->accumulator) clearZero(&c->flags);
	else setZero(&c->flags);
	if (val & 0x80) setNegative(&c->flags);
	else clearNegative(&c->flags);
	if (val & 0x40) setOverflow(&c->flags);
	else clearOverflow(&c->flags);
	c->tick = 0xff;
}
void sta(cpu *c)
{
	writeMemory(c, c->ad, c->accumulator);
	c->tick = 0xff;
}
void stx(cpu *c)
{
	writeMemory(c, c->ad, c->x);
	c->tick = 0xff;
}
void aax(cpu *c)
{
	writeMemory(c, c->ad, c->accumulator & c->x);
	c->tick = 0xff;
}
void sty(cpu *c)
{
	writeMemory(c, c->ad, c->y);
	c->tick = 0xff;
}
void dec(cpu *c)
{
	writeValue(c, c->ad, c->temp - 1);
	c->tick = 0xff;
}
void inc(cpu *c)
{
	writeValue(c, c->ad, c->temp + 1);
	c->tick = 0xff;
}
void lsrAccumulator(cpu *c)
{
	if (c->accumulator & 0x1) setCarry(&c->flags);
	else clearCarry(&c->flags);
	loadRegister(c, &c->accumulator, c->accumulator >> 1);
	c->tick = 0xff;
}
void aslAccumulator(cpu *c)
{
	if (c->accumulator & 0x80) setCarry(&c->flags);
	else clearCarry(&c->flags);
	loadRegister(c, &c->accumulator, c->accumulator << 1);
	c->tick = 0xff;
}
void rorAccumulator(cpu *c)
{
	uint8_t val = c->accumulator >> 1;
	if (checkCarry(c->flags)) val |= 0x80;
	else val &= ~0x80;
	if (c->accumulator & 0x1) setCarry(&c->flags);
	else clearCarry(&c->flags);
	loadRegister(c, &c->accumulator, val);
	c->tick = 0xff;
}
void rolAccumulator(cpu *c)
{
	uint8_t val = c->accumulator << 1;
	if (checkCarry(c->flags)) val |= 0x1;
	else val &= ~0x1;
	if (c->accumulator & 0x80) setCarry(&c->flags);
	else clearCarry(&c->flags);
	loadRegister(c, &c->accumulator, val);
	c->tick = 0xff;
}
void lsrMemory(cpu *c)
{
	if (c->temp & 0x1) setCarry(&c->flags);
	else clearCarry(&c->flags);
	writeValue(c, c->ad, c->temp >> 1);
	c->tick = 0xff;
}
void aslMemory(cpu *c)
{
	if (c->temp & 0x80) setCarry(&c->flags);
	else clearCarry(&c->flags);
	writeValue(c, c->ad, c->temp << 1);
	c->tick = 0xff;
}
void rorMemory(cpu *c)
{
	uint8_t val = c->temp >> 1;
	if (checkCarry(c->flags)) val |= 0x80;
	else val &= ~0x80;
	if (c->temp & 0x1) setCarry(&c->flags);
	else clearCarry(&c->flags);
	writeValue(c, c->ad, val);
	c->tick = 0xff;
}
void rolMemory(cpu *c)
{
	uint8_t val = c->temp << 1;
	if (checkCarry(c->flags)) val |= 0x1;
	else val &= ~0x1;
	if (c->temp & 0x80) setCarry(&c->flags);
	else clearCarry(&c->flags);
	writeValue(c, c->ad, val);
	c->tick = 0xff;
}
void dcp(cpu *c)
{
	dec(c);
	cmpMemory(c);
}
void isc(cpu *c)
{
	inc(c);
	sbcMemory(c);
}
void rla(cpu *c)
{
	rolMemory(c);
	andMemory(c);
}
void rra(cpu *c)
{
	rorMemory(c);
	adcMemory(c);
}
void slo(cpu *c)
{
	aslMemory(c);
	oraMemory(c);
}
void sre(cpu *c)
{
	lsrMemory(c);
	eorMemory(c);
}
void beq(cpu *c)
{
	if (checkZero(c->flags))
	{
		*c->pcL += c->temp;
	}
	else {
		c->tick = 0;
		fetchOp(c);
	}
}
void bne(cpu *c)
{
	if (!checkZero(c->flags))
	{
		*c->pcL += c->temp;
	}
	else {
		c->tick = 0;
		fetchOp(c);
	}
}
void bmi(cpu *c)
{
	if (checkNegative(c->flags))
	{
		*c->pcL += c->temp;
	}
	else {
		c->tick = 0;
		fetchOp(c);
	}
}
void bpl(cpu *c)
{
	if (!checkNegative(c->flags))
	{
		*c->pcL += c->temp;
	}
	else {
		c->tick = 0;
		fetchOp(c);
	}
}
void bcs(cpu *c)
{
	if (checkCarry(c->flags))
	{
		*c->pcL += c->temp;
	}
	else {
		c->tick = 0;
		fetchOp(c);
	}
}
void bcc(cpu *c)
{
	if (!checkCarry(c->flags))
	{
		*c->pcL += c->temp;
	}
	else {
		c->tick = 0;
		fetchOp(c);
	}
}
void bvs(cpu *c)
{
	if (checkOverflow(c->flags))
	{
		*c->pcL += c->temp;
	}
	else {
		c->tick = 0;
		fetchOp(c);
	}
}
void bvc(cpu *c)
{
	if (!checkOverflow(c->flags))
	{
		*c->pcL += c->temp;
	}
	else {
		c->tick = 0;
		fetchOp(c);
	}
}
void branch(cpu *c)
{
	if (c->temp < 127)
	{
		if (*c->pcL < c->temp)
		{
			(*c->pcH)++;
			c->tick = 0xff;
		}
		else
		{
			c->tick = 0;
			fetchOp(c);
		}
	}
	else
	{
		uint8_t prev = *c->pcL - c->temp;
		if (*c->pcL > prev)
		{
			(*c->pcH)--;
			c->tick = 0xff;
		}
		else
		{
			c->tick = 0;
			fetchOp(c);
		}
	}
}

void stuck(cpu *c)
{
	c->tick = 0;
}

void cpu_mapMemory(cpu *c, uint16_t address, uint8_t *pointer, bool write)
{
	if (write)
	{
		c->memory_write[address] = pointer;
	}
	else
	{
		c->memory_read[address] = pointer;
	}
}
void cpu_mapNMI(cpu *c, bool *pointer)
{
	c->nmi = pointer;
}
void cpu_mapIRQ(cpu *c, bool *pointer)
{
	c->irq = pointer;
}
bool* cpu_getRW(cpu *c)
{
	return &(c->write);
}
uint16_t* cpu_getAddress(cpu *c)
{
	return &(c->address);
}

cpu *cpu_create()
{
    cpu *newCPU = malloc(sizeof(cpu));
    
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
	for (int i = 0; i < 0x10000; i++)
    {
        newCPU->memory_read[i] = &(newCPU->dummy);
		newCPU->memory_write[i] = &(newCPU->dummy);
    }
	for (int i = 0; i < 0x100; i++)
    {
        newCPU->ops[i][0] = fetchOp;
		newCPU->ops[i][1] = stuck;
    }

	for (int i = 0; i < 0x10; i++)
	{
		newCPU->ops[i*0x10 + 0x1][1] = fetchADL;
		newCPU->ops[i*0x10 + 0x3][1] = fetchADL;
		newCPU->ops[i*0x10 + 0x4][1] = fetchADL;
		newCPU->ops[i*0x10 + 0x5][1] = fetchADL;
		newCPU->ops[i*0x10 + 0x6][1] = fetchADL;
		newCPU->ops[i*0x10 + 0x7][1] = fetchADL;
		newCPU->ops[i*0x10 + 0xc][1] = fetchADL;
		newCPU->ops[i*0x10 + 0xd][1] = fetchADL;
		newCPU->ops[i*0x10 + 0xe][1] = fetchADL;
		newCPU->ops[i*0x10 + 0xf][1] = fetchADL;

		newCPU->ops[i*0x10 + 0xc][2] = fetchADH;
		newCPU->ops[i*0x10 + 0xd][2] = fetchADH;
		newCPU->ops[i*0x10 + 0xe][2] = fetchADH;
		newCPU->ops[i*0x10 + 0xf][2] = fetchADH;

		newCPU->ops[i*0x10 + 0x1][2] = readAddress;
		newCPU->ops[i*0x10 + 0x3][2] = readAddress;
		
		if (i % 2)
		{
			newCPU->ops[i*0x10 + 0x9][1] = fetchADL;
			newCPU->ops[i*0x10 + 0x9][2] = fetchADH;
			newCPU->ops[i*0x10 + 0x4][2] = readZpX;
			newCPU->ops[i*0x10 + 0x5][2] = readZpX;
			newCPU->ops[i*0x10 + 0x1][3] = fetchIndirectY;
			newCPU->ops[i*0x10 + 0x3][3] = fetchIndirectY;
			newCPU->ops[i*0x10][3] = branch;

			if ((i & 0xc) != 0x8)
			{
				newCPU->ops[i*0x10 + 0xb][1] = fetchADL;
				newCPU->ops[i*0x10 + 0xb][2] = fetchADH;
				newCPU->ops[i*0x10 + 0xe][4] = readAddress;
				newCPU->ops[i*0x10 + 0x6][3] = readAddress;
				newCPU->ops[i*0x10 + 0xf][4] = readAddress;
				newCPU->ops[i*0x10 + 0xb][4] = readAddress;
				newCPU->ops[i*0x10 + 0x7][3] = readAddress;
				newCPU->ops[i*0x10 + 0x3][5] = readAddress;
				newCPU->ops[i*0x10 + 0x6][4] = writeAddress;
				newCPU->ops[i*0x10 + 0x7][4] = writeAddress;
				newCPU->ops[i*0x10 + 0xb][5] = writeAddress;
				newCPU->ops[i*0x10 + 0xe][5] = writeAddress;
				newCPU->ops[i*0x10 + 0xf][5] = writeAddress;
				newCPU->ops[i*0x10 + 0x3][6] = writeAddress;
				newCPU->ops[i*0x10 + 0x6][2] = readZpX;
				newCPU->ops[i*0x10 + 0x7][2] = readZpX;
				newCPU->ops[i*0x10 + 0xe][3] = writeAbsX;
				newCPU->ops[i*0x10 + 0xf][3] = writeAbsX;
				newCPU->ops[i*0x10 + 0xb][3] = writeAbsY;
				newCPU->ops[i*0x10 + 0x4][3] = nop;
				newCPU->ops[i*0x10 + 0xc][4] = nop;
			}

			if (i != 0x9)
			{
				newCPU->ops[i*0x10 + 0x1][4] = readIndirect;
				newCPU->ops[i*0x10 + 0x3][4] = readIndirect;
				newCPU->ops[i*0x10 + 0xc][3] = readAbsX;
				newCPU->ops[i*0x10 + 0xd][3] = readAbsX;
				newCPU->ops[i*0x10 + 0x9][3] = readAbsY;
			}
		}
		else
		{
			if ((i & 0xc) != 0x8)
			{
				newCPU->ops[i*0x10 + 0xe][3] = readAddress;
				newCPU->ops[i*0x10 + 0x6][2] = readAddress;
				newCPU->ops[i*0x10 + 0xf][3] = readAddress;
				newCPU->ops[i*0x10 + 0x7][2] = readAddress;
				newCPU->ops[i*0x10 + 0x6][3] = writeAddress;
				newCPU->ops[i*0x10 + 0x7][3] = writeAddress;
				newCPU->ops[i*0x10 + 0xe][4] = writeAddress;
				newCPU->ops[i*0x10 + 0xf][4] = writeAddress;
				newCPU->ops[i*0x10 + 0x3][5] = readAddress;
				newCPU->ops[i*0x10 + 0x3][6] = writeAddress;
			}
			newCPU->ops[i*0x10 + 0x1][3] = fetchIndirectXLow;
			newCPU->ops[i*0x10 + 0x3][3] = fetchIndirectXLow;
			newCPU->ops[i*0x10 + 0x1][4] = fetchIndirectXHigh;
			newCPU->ops[i*0x10 + 0x3][4] = fetchIndirectXHigh;
		}
	}

	newCPU->ops[0x6c][3] = readAddress;

	newCPU->ops[0x20][1] = fetchADL;

	newCPU->ops[0x00][1] = fetchValue;
	newCPU->ops[0x10][1] = fetchValue;
	newCPU->ops[0x30][1] = fetchValue;
	newCPU->ops[0x40][1] = fetchValue;
	newCPU->ops[0x50][1] = fetchValue;
	newCPU->ops[0x60][1] = fetchValue;
	newCPU->ops[0x70][1] = fetchValue;
	newCPU->ops[0x90][1] = fetchValue;
	newCPU->ops[0xb0][1] = fetchValue;
	newCPU->ops[0xd0][1] = fetchValue;
	newCPU->ops[0xf0][1] = fetchValue;

	newCPU->ops[0x96][2] = readZpY;
	newCPU->ops[0xb6][2] = readZpY;
	newCPU->ops[0x97][2] = readZpY;
	newCPU->ops[0xb7][2] = readZpY;

	newCPU->ops[0x91][4] = writeIndirect;

	newCPU->ops[0x9d][3] = writeAbsX;

	newCPU->ops[0xbe][3] = readAbsY;
	newCPU->ops[0xbf][3] = readAbsY;

	newCPU->ops[0x99][3] = writeAbsY;

	newCPU->ops[0xa1][5] = ldaMemory;
	newCPU->ops[0xb1][5] = ldaMemory;
	newCPU->ops[0xa5][2] = ldaMemory;
	newCPU->ops[0xb5][3] = ldaMemory;
	newCPU->ops[0xb9][4] = ldaMemory;
	newCPU->ops[0xad][3] = ldaMemory;
	newCPU->ops[0xbd][4] = ldaMemory;

	newCPU->ops[0xa6][2] = ldxMemory;
	newCPU->ops[0xb6][3] = ldxMemory;
	newCPU->ops[0xae][3] = ldxMemory;
	newCPU->ops[0xbe][4] = ldxMemory;

	newCPU->ops[0xa3][5] = lax;
	newCPU->ops[0xb3][5] = lax;
	newCPU->ops[0xa7][2] = lax;
	newCPU->ops[0xb7][3] = lax;
	newCPU->ops[0xaf][3] = lax;
	newCPU->ops[0xbf][4] = lax;

	newCPU->ops[0xa4][2] = ldyMemory;
	newCPU->ops[0xb4][3] = ldyMemory;
	newCPU->ops[0xac][3] = ldyMemory;
	newCPU->ops[0xbc][4] = ldyMemory;

	newCPU->ops[0x01][5] = oraMemory;
	newCPU->ops[0x11][5] = oraMemory;
	newCPU->ops[0x05][2] = oraMemory;
	newCPU->ops[0x15][3] = oraMemory;
	newCPU->ops[0x19][4] = oraMemory;
	newCPU->ops[0x0d][3] = oraMemory;
	newCPU->ops[0x1d][4] = oraMemory;

	newCPU->ops[0x21][5] = andMemory;
	newCPU->ops[0x31][5] = andMemory;
	newCPU->ops[0x25][2] = andMemory;
	newCPU->ops[0x35][3] = andMemory;
	newCPU->ops[0x39][4] = andMemory;
	newCPU->ops[0x2d][3] = andMemory;
	newCPU->ops[0x3d][4] = andMemory;

	newCPU->ops[0x41][5] = eorMemory;
	newCPU->ops[0x51][5] = eorMemory;
	newCPU->ops[0x45][2] = eorMemory;
	newCPU->ops[0x55][3] = eorMemory;
	newCPU->ops[0x59][4] = eorMemory;
	newCPU->ops[0x4d][3] = eorMemory;
	newCPU->ops[0x5d][4] = eorMemory;

	newCPU->ops[0x61][5] = adcMemory;
	newCPU->ops[0x71][5] = adcMemory;
	newCPU->ops[0x65][2] = adcMemory;
	newCPU->ops[0x75][3] = adcMemory;
	newCPU->ops[0x79][4] = adcMemory;
	newCPU->ops[0x6d][3] = adcMemory;
	newCPU->ops[0x7d][4] = adcMemory;

	newCPU->ops[0xc1][5] = cmpMemory;
	newCPU->ops[0xd1][5] = cmpMemory;
	newCPU->ops[0xc5][2] = cmpMemory;
	newCPU->ops[0xd5][3] = cmpMemory;
	newCPU->ops[0xd9][4] = cmpMemory;
	newCPU->ops[0xcd][3] = cmpMemory;
	newCPU->ops[0xdd][4] = cmpMemory;

	newCPU->ops[0xe1][5] = sbcMemory;
	newCPU->ops[0xf1][5] = sbcMemory;
	newCPU->ops[0xe5][2] = sbcMemory;
	newCPU->ops[0xf5][3] = sbcMemory;
	newCPU->ops[0xf9][4] = sbcMemory;
	newCPU->ops[0xed][3] = sbcMemory;
	newCPU->ops[0xfd][4] = sbcMemory;

	newCPU->ops[0xe4][2] = cpxMemory;
	newCPU->ops[0xec][3] = cpxMemory;

	newCPU->ops[0xc4][2] = cpyMemory;	
	newCPU->ops[0xcc][3] = cpyMemory;

	newCPU->ops[0x06][4] = aslMemory;
	newCPU->ops[0x0e][5] = aslMemory;
	newCPU->ops[0x16][5] = aslMemory;
	newCPU->ops[0x1e][6] = aslMemory;

	newCPU->ops[0x26][4] = rolMemory;
	newCPU->ops[0x2e][5] = rolMemory;
	newCPU->ops[0x36][5] = rolMemory;
	newCPU->ops[0x3e][6] = rolMemory;

	newCPU->ops[0x46][4] = lsrMemory;
	newCPU->ops[0x4e][5] = lsrMemory;
	newCPU->ops[0x56][5] = lsrMemory;
	newCPU->ops[0x5e][6] = lsrMemory;

	newCPU->ops[0x66][4] = rorMemory;
	newCPU->ops[0x6e][5] = rorMemory;
	newCPU->ops[0x76][5] = rorMemory;
	newCPU->ops[0x7e][6] = rorMemory;

	newCPU->ops[0x03][7] = slo;
	newCPU->ops[0x13][7] = slo;
	newCPU->ops[0x07][4] = slo;
	newCPU->ops[0x17][5] = slo;
	newCPU->ops[0x1b][6] = slo;
	newCPU->ops[0x0f][5] = slo;
	newCPU->ops[0x1f][6] = slo;

	newCPU->ops[0x23][7] = rla;
	newCPU->ops[0x33][7] = rla;
	newCPU->ops[0x27][4] = rla;
	newCPU->ops[0x37][5] = rla;
	newCPU->ops[0x3b][6] = rla;
	newCPU->ops[0x2f][5] = rla;
	newCPU->ops[0x3f][6] = rla;

	newCPU->ops[0x43][7] = sre;
	newCPU->ops[0x53][7] = sre;
	newCPU->ops[0x47][4] = sre;
	newCPU->ops[0x57][5] = sre;
	newCPU->ops[0x5b][6] = sre;
	newCPU->ops[0x4f][5] = sre;
	newCPU->ops[0x5f][6] = sre;

	newCPU->ops[0x63][7] = rra;
	newCPU->ops[0x73][7] = rra;
	newCPU->ops[0x67][4] = rra;
	newCPU->ops[0x77][5] = rra;
	newCPU->ops[0x7b][6] = rra;
	newCPU->ops[0x6f][5] = rra;
	newCPU->ops[0x7f][6] = rra;

	newCPU->ops[0xc3][7] = dcp;
	newCPU->ops[0xd3][7] = dcp;
	newCPU->ops[0xc7][4] = dcp;
	newCPU->ops[0xd7][5] = dcp;
	newCPU->ops[0xdb][6] = dcp;
	newCPU->ops[0xcf][5] = dcp;
	newCPU->ops[0xdf][6] = dcp;

	newCPU->ops[0xe3][7] = isc;
	newCPU->ops[0xf3][7] = isc;
	newCPU->ops[0xe7][4] = isc;
	newCPU->ops[0xf7][5] = isc;
	newCPU->ops[0xfb][6] = isc;
	newCPU->ops[0xef][5] = isc;
	newCPU->ops[0xff][6] = isc;

	newCPU->ops[0x91][5] = sta;
	newCPU->ops[0x81][5] = sta;	
	newCPU->ops[0x85][2] = sta;
	newCPU->ops[0x95][3] = sta;
	newCPU->ops[0x99][4] = sta;
	newCPU->ops[0x8d][3] = sta;
	newCPU->ops[0x9d][4] = sta;

	newCPU->ops[0x83][5] = aax;
	newCPU->ops[0x87][2] = aax;
	newCPU->ops[0x97][3] = aax;
	newCPU->ops[0x8f][3] = aax;

	newCPU->ops[0x86][2] = stx;
	newCPU->ops[0x96][3] = stx;
	newCPU->ops[0x8e][3] = stx;

	newCPU->ops[0x84][2] = sty;
	newCPU->ops[0x94][3] = sty;
	newCPU->ops[0x8c][3] = sty;

	newCPU->ops[0xe6][4] = inc;
	newCPU->ops[0xee][5] = inc;
	newCPU->ops[0xf6][5] = inc;
	newCPU->ops[0xfe][6] = inc;

	newCPU->ops[0xc6][4] = dec;
	newCPU->ops[0xd6][5] = dec;
	newCPU->ops[0xce][5] = dec;
	newCPU->ops[0xde][6] = dec;

	newCPU->ops[0x24][2] = bit;
	newCPU->ops[0x2c][3] = bit;

	newCPU->ops[0x10][2] = bpl;
	newCPU->ops[0x30][2] = bmi;
	newCPU->ops[0x50][2] = bvc;
	newCPU->ops[0x70][2] = bvs;
	newCPU->ops[0x90][2] = bcc;
	newCPU->ops[0xb0][2] = bcs;
	newCPU->ops[0xd0][2] = bne;
	newCPU->ops[0xf0][2] = beq;

	newCPU->ops[0x40][2] = ins;
	newCPU->ops[0x60][2] = ins;
	newCPU->ops[0x28][2] = ins;
	newCPU->ops[0x68][2] = ins;

	newCPU->ops[0x48][1] = readValue;
	newCPU->ops[0x68][1] = readValue;
	newCPU->ops[0x28][1] = readValue;
	newCPU->ops[0x08][1] = readValue;

	newCPU->ops[0x20][5] = jmp;
	newCPU->ops[0x4c][2] = jmp;

	newCPU->ops[0x00][2] = pushPCH;
	newCPU->ops[0x20][3] = pushPCH;

	newCPU->ops[0x00][3] = pushPCL;
	newCPU->ops[0x20][4] = pushPCL;

	newCPU->ops[0x40][4] = pullPCL;
	newCPU->ops[0x60][3] = pullPCL;

	newCPU->ops[0x60][4] = pullPCH;

	newCPU->ops[0x40][5] = rtiPullPCH;

	newCPU->ops[0xa8][1] = tay;
	newCPU->ops[0x98][1] = tya;
	newCPU->ops[0x8a][1] = txa;
	newCPU->ops[0x9a][1] = txs;
	newCPU->ops[0xaa][1] = tax;
	newCPU->ops[0xba][1] = tsx;	
	
	newCPU->ops[0x18][1] = clc;
	newCPU->ops[0x38][1] = sec;
	newCPU->ops[0x58][1] = cli;
	newCPU->ops[0x78][1] = sei;
	newCPU->ops[0xb8][1] = clv;
	newCPU->ops[0xd8][1] = cld;
	newCPU->ops[0xf8][1] = sed;
	
	newCPU->ops[0x88][1] = dey;
	newCPU->ops[0xc8][1] = iny;
	newCPU->ops[0xe8][1] = inx;
	newCPU->ops[0xca][1] = dex;

	newCPU->ops[0x1a][1] = nop;
	newCPU->ops[0x3a][1] = nop;
	newCPU->ops[0x5a][1] = nop;
	newCPU->ops[0x7a][1] = nop;
	newCPU->ops[0xda][1] = nop;
	newCPU->ops[0xea][1] = nop;
	newCPU->ops[0xfa][1] = nop;

	newCPU->ops[0x80][1] = nop;
	newCPU->ops[0x82][1] = nop;
	newCPU->ops[0x89][1] = nop;
	newCPU->ops[0x04][2] = nop;
	newCPU->ops[0x44][2] = nop;
	newCPU->ops[0x64][2] = nop;
	newCPU->ops[0x0c][3] = nop;

	newCPU->ops[0x08][2] = php;
	newCPU->ops[0x28][3] = plp;
	newCPU->ops[0x48][2] = pha;
	newCPU->ops[0x68][3] = pla;

	newCPU->ops[0xa0][1] = ldyImmediate;
	newCPU->ops[0xc0][1] = cpyImmediate;
	newCPU->ops[0xe0][1] = cpxImmediate;
	newCPU->ops[0xa2][1] = ldxImmediate;
	newCPU->ops[0x09][1] = oraImmediate;
	newCPU->ops[0x29][1] = andImmediate;
	newCPU->ops[0x49][1] = eorImmediate;
	newCPU->ops[0x69][1] = adcImmediate;
	newCPU->ops[0xa9][1] = ldaImmediate;
	newCPU->ops[0xc9][1] = cmpImmediate;
	newCPU->ops[0xe9][1] = sbcImmediate;
	newCPU->ops[0xeb][1] = sbcImmediate;
	
	newCPU->ops[0x0a][1] = aslAccumulator;
	newCPU->ops[0x2a][1] = rolAccumulator;
	newCPU->ops[0x4a][1] = lsrAccumulator;
	newCPU->ops[0x6a][1] = rorAccumulator;
	
	newCPU->ops[0x20][2] = readStack;
	newCPU->ops[0x6c][4] = jmpIndirect;
	newCPU->ops[0x00][4] = pushBStatus;
	newCPU->ops[0x00][5] = fetchPCLbrk;
	newCPU->ops[0x00][6] = fetchPCHbrk;	
	newCPU->ops[0x60][5] = incPC;	
	newCPU->ops[0x40][3] = pullFlags;

    return newCPU;
}

void cpu_reset(cpu *c)
{
    setInterrupt(&c->flags);
    *c->pcL = readMemory(c, 0xfffc);
    *c->pcH = readMemory(c, 0xfffd);
}

void cpu_printState(cpu *c)
{
    printf("OP:%s (%02hhx)", instrs[c->currentOp], c->currentOp);
	printf(" PC:%04hx", c->pc);
	printf(" A:%02hhx", c->accumulator);
	printf(" X:%02hhx", c->x);
	printf(" Y:%02hhx", c->y);
	if (checkNegative(c->flags)) printf(" N"); else printf(" n");
	if (checkOverflow(c->flags)) printf("V"); else printf("v");
    if (checkInterrupt(c->flags)) printf("I"); else printf("i");
	if (checkZero(c->flags)) printf("Z"); else printf("z");
	if (checkCarry(c->flags)) printf("C "); else printf("c ");
	printf(" Stack: ");
	for (int i = 0xff; i > c->stackPointer; i--)
	{
		printf("%02hhx, ", *c->memory_read[0x100 + i]);
	}
}

void cpu_executeCycle(cpu *c)
{
	c->tick++;
	c->write = false;
	c->ops[c->currentOp][c->tick](c);
	if (!c->nmi_pending && !c->nmi_executing && !c->nmi_starting)
	{
		c->nmi_pending = (*c->nmi && !c->nmi_prev);
	}
	c->irq_pending = *c->irq;
	c->nmi_prev = *c->nmi;
}
