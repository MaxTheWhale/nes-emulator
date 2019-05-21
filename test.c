#define SDL_MAIN_HANDLED
#include <stdio.h>
#include <stdlib.h>
#include <SDL2/SDL.h>
#include "nes.h"
#include "cpu.h"

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
    cpu_reset(nes->cpu);
    for (int x = 0; x < 26300; x++)
    {
        //cpu_printState(cpu);
        //printf("\n");
        cpu_executeCycle(nes->cpu);
    }
    nes_emulateFrame(nes, screen->pixels);

    SDL_UpdateWindowSurface(window);

    SDL_Delay(2000);
    SDL_DestroyWindow(window);
    SDL_Quit();
    while (1)
    {
        cpu_printState(nes->cpu);
        getchar();
        cpu_executeCycle(nes->cpu);
    }
    return 0;
}
