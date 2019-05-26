#define SDL_MAIN_HANDLED
#include <stdio.h>
#include <stdlib.h>
#include <SDL2/SDL.h>
#include "nes.h"
#include "cpu.h"
#include "ppu.h"

int main(int n, char **args)
{
    SDL_Surface *screen;
    SDL_Window *window;

    SDL_Init(SDL_INIT_VIDEO);
    window = SDL_CreateWindow("NES Emulator", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 256, 240, SDL_WINDOW_OPENGL);

    screen = SDL_GetWindowSurface(window);

    nes *nes = nes_create();
    nes_loadROM(nes, "nestest.nes");
    nes_loadPalette(nes, "pal.pal");
    nes_setFramebuffer(nes, screen->pixels);
    cpu_reset(nes->cpu);
    // for (int x = 0; x < 100000; x++)
    // {
    //     //cpu_printState(cpu);
    //     //printf("\n");
    //     nes_stepCycle(nes);
    // }
    //nes_emulateFrame(nes);
    bool quit = false;
    while (!quit)
    {
        SDL_Event e;
        while (SDL_PollEvent(&e))
        {
            if (e.type == SDL_KEYDOWN)
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
            else if (e.type == SDL_QUIT) quit = true;
        }
        SDL_UpdateWindowSurface(window);
    }
    SDL_DestroyWindow(window);
    SDL_Quit();
    return 0;
}
