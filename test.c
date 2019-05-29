#define SDL_MAIN_HANDLED
#include <stdio.h>
#include <stdlib.h>
#include <SDL2/SDL.h>
#include "nes.h"
#include "cpu.h"
#include "ppu.h"

int main(int n, char **args)
{
    if (n != 2)
    {
        printf("Usage: ./nes rom.nes\n");
        exit(1);
    }
    SDL_Surface *screen;
    SDL_Window *window;

    SDL_Init(SDL_INIT_VIDEO);
    window = SDL_CreateWindow("NES Emulator", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 256, 240, SDL_WINDOW_OPENGL);

    screen = SDL_GetWindowSurface(window);

    nes *nes = nes_create();
    nes_loadROM(nes, args[1]);
    nes_loadPalette(nes, "pal.pal");
    nes_setFramebuffer(nes, screen->pixels);
    cpu_reset(nes->cpu);
    bool quit = false;
    bool paused = true;
    while (!quit)
    {
        SDL_Event e;
        while (SDL_PollEvent(&e))
        {
            if (e.type == SDL_KEYDOWN)
            {
                if (paused)
                {
                    if (e.key.keysym.scancode == SDL_SCANCODE_SPACE)
                    {
                        nes_emulateFrame(nes);
                    }
                    else
                    {
                        cpu_printState(nes->cpu);
                        printf("\n");
                        ppu_print(nes->ppu);
                        nes_stepCycle(nes);
                    }
                }
                if (e.key.keysym.scancode == SDL_SCANCODE_P)
                {
                    paused = !paused;
                }
            }
            if (e.type == SDL_QUIT) quit = true;
            const uint8_t *key_state = SDL_GetKeyboardState(NULL);
            uint8_t pad_state = 0;
            if (key_state[SDL_SCANCODE_X]) pad_state |= 0x1;
            if (key_state[SDL_SCANCODE_Z]) pad_state |= 0x2;
            if (key_state[SDL_SCANCODE_S]) pad_state |= 0x4;
            if (key_state[SDL_SCANCODE_RETURN]) pad_state |= 0x8;
            if (key_state[SDL_SCANCODE_UP]) pad_state |= 0x10;
            if (key_state[SDL_SCANCODE_DOWN]) pad_state |= 0x20;
            if (key_state[SDL_SCANCODE_LEFT]) pad_state |= 0x40;
            if (key_state[SDL_SCANCODE_RIGHT]) pad_state |= 0x80;
            nes_updatePadState(nes, pad_state);
        }
        if (!paused)
        {
            nes_emulateFrame(nes);
            SDL_Delay(15);
        }
        SDL_UpdateWindowSurface(window);
    }
    SDL_DestroyWindow(window);
    SDL_Quit();
    return 0;
}
