#include "sim.h"
#define REPEAT_SIZE 255

int main()
{
	int i = 0;
	int x = 0;
	int y = 0;
	for (i = 0; i < REPEAT_SIZE; i++)
	{
		for (y = 0; y < SIM_Y_SIZE; y++)
			for (x = 0; x < SIM_X_SIZE; x++)
				simSetPixel(x, y, 0xFFFF0000 + 0x101 * i);
		simFlush();
	}
	return 0;
}
