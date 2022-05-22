#define SIM_X_SIZE 256
#define SIM_Y_SIZE 128
#define SIM_DISPLAY_MEM_ADDR 128*256

extern void simSetPixel(int x, int y, int argb);
extern void simFlush();
extern void simBkpt();
extern int simRand();
