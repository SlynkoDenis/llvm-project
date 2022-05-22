#include "sim.h"

#define LIVE_COLOR 0xFFFF0000
#define REPEAT_SIZE 255


int BlackWhiteToBin(int val) {
    if (val == LIVE_COLOR) {
        return 1;
    }
    return val & 1;
}


int GetPixel(int x, int y) {
    int addr = SIM_DISPLAY_MEM_ADDR + y * SIM_X_SIZE + x;
    return *((int *)addr);
}


void PrintRandomGrid() {
    for (int y = 0; y < SIM_Y_SIZE; ++y) {
        for (int x = 0; x < SIM_X_SIZE; ++x) {
            simSetPixel(x, y, LIVE_COLOR & (0 - simRand() % 2));
        }
    }
    simFlush();
}


int CheckNeighbour(int x, int y) {   
    if (x < 0) {
        x = SIM_X_SIZE - 1;
    }

    if (x >= SIM_X_SIZE) {
        x = 0;
    }

    if (y < 0) {
        y = SIM_Y_SIZE - 1;
    }

    if (y >= SIM_Y_SIZE) {
        y = 0;
    }

    return BlackWhiteToBin(GetPixel(x, y));
}


int CountNeighbours(int y, int x) {
    int neighboursNumber = 0;
    neighboursNumber += CheckNeighbour(x - 1, y - 1);
    neighboursNumber += CheckNeighbour(x, y - 1);
    neighboursNumber += CheckNeighbour(x + 1, y - 1);
    neighboursNumber += CheckNeighbour(x - 1, y);
    neighboursNumber += CheckNeighbour(x + 1, y);
    neighboursNumber += CheckNeighbour(x - 1, y + 1);
    neighboursNumber += CheckNeighbour(x, y + 1);
    neighboursNumber += CheckNeighbour(x + 1, y + 1);
    return neighboursNumber;
}


void PrintUpdatedGrid() {
    for (int y = 0; y < SIM_Y_SIZE; ++y) {
        for (int x = 0; x < SIM_X_SIZE; ++x) {
            int tmp = (CountNeighbours(y, x) << 1) | BlackWhiteToBin(GetPixel(x, y));
            simSetPixel(x, y, tmp);
        }
    }

    for (int y = 0; y < SIM_Y_SIZE; ++y) {
        for (int x = 0; x < SIM_X_SIZE; ++x) {
            int value = GetPixel(x, y);
            int oldValueBin = value & 0x1;
            int countedNeighbours = value >> 1;
            if (countedNeighbours > 8 || countedNeighbours < 0) {
                simBkpt();
            }

            if (oldValueBin == 0 && countedNeighbours == 3) {
                simSetPixel(x, y, LIVE_COLOR);
            }
            else if (oldValueBin == 1 && (countedNeighbours > 3 || countedNeighbours < 2)) {
                simSetPixel(x, y, 0);
            }
            else {
                simSetPixel(x, y, LIVE_COLOR & (0 - oldValueBin));
            }
        }
    }
    simFlush();
}


void TestOnStartup() {
    simSetPixel(12, 44, 0xABABA);
    if (GetPixel(12, 44) != 0xABABA) {
        simBkpt();
    }

    simSetPixel(43, 34, 0xBEDA);
    if (GetPixel(43, 34) != 0xBEDA) {
        simBkpt();
    }
}


int main() {
    TestOnStartup();

    PrintRandomGrid();
    for (int i = 0; i < REPEAT_SIZE; ++i) {
        PrintUpdatedGrid();
    }
    return 0;
}
