#include "apu.h"
#include <stdio.h>
#include <stdlib.h>

enum {
    PULSE1_R0     = 0x4000,
    PULSE1_R1     = 0x4001,
    PULSE1_R2     = 0x4002,
    PULSE1_R3     = 0x4003,
    PULSE2_R0     = 0x4004,
    PULSE2_R1     = 0x4005,
    PULSE2_R2     = 0x4006,
    PULSE2_R3     = 0x4007,
    TRI_R0        = 0x4008,
    TRI_R1        = 0x400a,
    TRI_R2        = 0x400b,
    NOISE_R0      = 0x400c,
    NOISE_R1      = 0x400e,
    NOISE_R2      = 0x400f,
    DMC_R0        = 0x4010,
    DMC_R1        = 0x4011,
    DMC_R2        = 0x4012,
    DMC_R3        = 0x4013,
    APUCTRL       = 0x4015,
    FRAME_COUNTER = 0x4017
};

struct apu {
    // External signals
    uint16_t* cpu_address;
    bool* write;
    bool irq;

    uint16_t frame_tick;
    bool odd_cycle;
};

apu* apu_create() {
    apu* newAPU = malloc(sizeof(apu));
    newAPU->irq = false;
    return newAPU;
}

void apu_mapRW(apu* a, bool* pointer) {
    a->write = pointer;
}
void apu_mapAddress(apu* a, uint16_t* pointer) {
    a->cpu_address = pointer;
}
bool* apu_getIRQ(apu* a) {
    return &(a->irq);
}

void apu_executeCycle(apu* a) {
    a->odd_cycle = !a->odd_cycle;
    if (*a->cpu_address == APUCTRL)
        a->irq = false;
    switch (a->frame_tick++) {
        case 7457:
            break;
        case 14913:
            break;
        case 22371:
            break;
        case 29828:
            break;
        case 29829:
            break;
        case 29830:
            a->irq = true;
            a->frame_tick = 0;
            break;
    }
}
