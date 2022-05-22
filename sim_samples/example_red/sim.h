#define SIM_X_SIZE 256
#define SIM_Y_SIZE 128
#define SIM_DISPLAY_MEM_ADDR 128*256

extern void simSetPixel(int x, int y, int argb);
extern void simFlush();
extern void simBkpt();
extern int simRand();

/*
MOVli [stack reg] 32767
BL [link reg] main
EXIT

simRand:
RAND [arg0 reg]
BR [link reg]

simFlush:
FLUSH
BR [link reg]

simBkpt:
BKPT
BR [link reg]

simSetPixel:
MULi [tmp reg] [arg1 reg] 256
ADDi [tmp reg] [tmp reg] 32767
ADDi [tmp reg] [tmp reg] 1
ST [arg2 reg] [tmp reg] [arg0 reg]
BR [link reg]
*/
