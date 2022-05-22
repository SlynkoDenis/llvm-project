MOVli r2 32767
BL r1 main
EXIT

simRand:
RAND r9
BR r1

simFlush:
FLUSH
BR r1

simBkpt:
BKPT
BR r1

simSetPixel:
MULi r13 r10 256
ADDi r13 r13 32767
ADDi r13 r13 1
ST r11 r13 r9
BR r1

BlackWhiteToBin:
	ADDi r2 r2 -3
	STi r3 r2 2
	ADDi r3 r2 3
	STi r9 r3 -3
	LDi r4 r3 -3
	MOVhi r9 65535
	ORi r9 r9 0
	B.NE r4 r9 .LBB0_2
	B .LBB0_1
.LBB0_1:
	MOVli r4 1
	STi r4 r3 -2
	B .LBB0_3
.LBB0_2:
	LDi r4 r3 -3
	ANDi r4 r4 1
	STi r4 r3 -2
	B .LBB0_3
.LBB0_3:
	LDi r9 r3 -2
	LDi r3 r2 2
	ADDi r2 r2 3
	BR r1

GetPixel:
	ADDi r2 r2 -4
	STi r3 r2 3
	ADDi r3 r2 4
	STi r9 r3 -2
	STi r10 r3 -3
	LDi r4 r3 -3
	SHLi r4 r4 8
	LDi r9 r3 -2
	ADD r4 r4 r9
	MOVli r9 32768
	ADD r4 r4 r9
	STi r4 r3 -4
	LDi r4 r3 -4
	LDi r9 r4 0
	LDi r3 r2 3
	ADDi r2 r2 4
	BR r1

PrintRandomGrid:
	ADDi r2 r2 -8
	STi r0 r2 7
	STi r1 r2 6
	STi r3 r2 5
	ADDi r3 r2 8
	MOVli r4 0
	STi r4 r3 -4
	B .LBB2_1
.LBB2_1:
	LDi r4 r3 -4
	MOVli r9 127
	B.GT r4 r9 .LBB2_8
	B .LBB2_2
.LBB2_2:
	MOVli r4 0
	STi r4 r3 -5
	B .LBB2_3
.LBB2_3:
	LDi r4 r3 -5
	MOVli r9 255
	B.GT r4 r9 .LBB2_6
	B .LBB2_4
.LBB2_4:
	LDi r4 r3 -5
	STi r4 r3 -8
	LDi r4 r3 -4
	STi r4 r3 -7
	BL r1 simRand
	LDi r10 r3 -7
	ADDi r11 r9 0
	LDi r9 r3 -8
	SHRi r4 r11 31
	ADD r4 r11 r4
	ANDi r4 r4 -2
	SUB r4 r4 r11
	MOVhi r11 65535
	ORi r11 r11 0
	AND r11 r4 r11
	STi r11 r3 -6
	LDi r9 r3 -8
	LDi r10 r3 -7
	LDi r11 r3 -6
	BL r1 simSetPixel
	B .LBB2_5
.LBB2_5:
	LDi r4 r3 -5
	ADDi r4 r4 1
	STi r4 r3 -5
	B .LBB2_3
.LBB2_6:
	B .LBB2_7
.LBB2_7:
	LDi r4 r3 -4
	ADDi r4 r4 1
	STi r4 r3 -4
	B .LBB2_1
.LBB2_8:
	BL r1 simFlush
	LDi r3 r2 5
	LDi r1 r2 6
	LDi r0 r2 7
	ADDi r2 r2 8
	BR r1

CheckNeighbour:
	ADDi r2 r2 -8
	STi r0 r2 7
	STi r1 r2 6
	STi r3 r2 5
	ADDi r3 r2 8
	STi r9 r3 -4
	STi r10 r3 -5
	LDi r4 r3 -4
	MOVhi r9 65535
	ORi r9 r9 65535
	B.GT r4 r9 .LBB3_2
	B .LBB3_1
.LBB3_1:
	MOVli r4 255
	STi r4 r3 -4
	B .LBB3_2
.LBB3_2:
	LDi r9 r3 -4
	MOVli r4 256
	B.GT r4 r9 .LBB3_4
	B .LBB3_3
.LBB3_3:
	MOVli r4 0
	STi r4 r3 -4
	B .LBB3_4
.LBB3_4:
	LDi r4 r3 -5
	MOVhi r9 65535
	ORi r9 r9 65535
	B.GT r4 r9 .LBB3_6
	B .LBB3_5
.LBB3_5:
	MOVli r4 127
	STi r4 r3 -5
	B .LBB3_6
.LBB3_6:
	LDi r9 r3 -5
	MOVli r4 128
	B.GT r4 r9 .LBB3_8
	B .LBB3_7
.LBB3_7:
	MOVli r4 0
	STi r4 r3 -5
	B .LBB3_8
.LBB3_8:
	LDi r9 r3 -4
	STi r9 r3 -8
	LDi r10 r3 -5
	STi r10 r3 -7
	LDi r9 r3 -8
	LDi r10 r3 -7
	BL r1 GetPixel
	STi r9 r3 -6
	LDi r9 r3 -6
	BL r1 BlackWhiteToBin
	LDi r3 r2 5
	LDi r1 r2 6
	LDi r0 r2 7
	ADDi r2 r2 8
	BR r1

CountNeighbours:
	ADDi r2 r2 -22
	STi r0 r2 21
	STi r1 r2 20
	STi r3 r2 19
	ADDi r3 r2 22
	STi r9 r3 -4
	STi r10 r3 -5
	MOVli r4 0
	STi r4 r3 -6
	LDi r4 r3 -5
	ADDi r9 r4 -1
	STi r9 r3 -22
	LDi r4 r3 -4
	ADDi r10 r4 -1
	STi r10 r3 -21
	LDi r9 r3 -22
	LDi r10 r3 -21
	BL r1 CheckNeighbour
	LDi r4 r3 -6
	ADD r4 r4 r9
	STi r4 r3 -6
	LDi r9 r3 -5
	STi r9 r3 -20
	LDi r4 r3 -4
	ADDi r10 r4 -1
	STi r10 r3 -19
	LDi r9 r3 -20
	LDi r10 r3 -19
	BL r1 CheckNeighbour
	LDi r4 r3 -6
	ADD r4 r4 r9
	STi r4 r3 -6
	LDi r4 r3 -5
	ADDi r9 r4 1
	STi r9 r3 -18
	LDi r4 r3 -4
	ADDi r10 r4 -1
	STi r10 r3 -17
	LDi r9 r3 -18
	LDi r10 r3 -17
	BL r1 CheckNeighbour
	LDi r4 r3 -6
	ADD r4 r4 r9
	STi r4 r3 -6
	LDi r4 r3 -5
	ADDi r9 r4 -1
	STi r9 r3 -16
	LDi r10 r3 -4
	STi r10 r3 -15
	LDi r9 r3 -16
	LDi r10 r3 -15
	BL r1 CheckNeighbour
	LDi r4 r3 -6
	ADD r4 r4 r9
	STi r4 r3 -6
	LDi r4 r3 -5
	ADDi r9 r4 1
	STi r9 r3 -14
	LDi r10 r3 -4
	STi r10 r3 -13
	LDi r9 r3 -14
	LDi r10 r3 -13
	BL r1 CheckNeighbour
	LDi r4 r3 -6
	ADD r4 r4 r9
	STi r4 r3 -6
	LDi r4 r3 -5
	ADDi r9 r4 -1
	STi r9 r3 -12
	LDi r4 r3 -4
	ADDi r10 r4 1
	STi r10 r3 -11
	LDi r9 r3 -12
	LDi r10 r3 -11
	BL r1 CheckNeighbour
	LDi r4 r3 -6
	ADD r4 r4 r9
	STi r4 r3 -6
	LDi r9 r3 -5
	STi r9 r3 -10
	LDi r4 r3 -4
	ADDi r10 r4 1
	STi r10 r3 -9
	LDi r9 r3 -10
	LDi r10 r3 -9
	BL r1 CheckNeighbour
	LDi r4 r3 -6
	ADD r4 r4 r9
	STi r4 r3 -6
	LDi r4 r3 -5
	ADDi r9 r4 1
	STi r9 r3 -8
	LDi r4 r3 -4
	ADDi r10 r4 1
	STi r10 r3 -7
	LDi r9 r3 -8
	LDi r10 r3 -7
	BL r1 CheckNeighbour
	LDi r4 r3 -6
	ADD r4 r4 r9
	STi r4 r3 -6
	LDi r9 r3 -6
	LDi r3 r2 19
	LDi r1 r2 20
	LDi r0 r2 21
	ADDi r2 r2 22
	BR r1

PrintUpdatedGrid:
	ADDi r2 r2 -31
	STi r0 r2 30
	STi r1 r2 29
	STi r3 r2 28
	ADDi r3 r2 31
	MOVli r4 0
	STi r4 r3 -4
	B .LBB5_1
.LBB5_1:
	LDi r4 r3 -4
	MOVli r9 127
	B.GT r4 r9 .LBB5_8
	B .LBB5_2
.LBB5_2:
	MOVli r4 0
	STi r4 r3 -5
	B .LBB5_3
.LBB5_3:
	LDi r4 r3 -5
	MOVli r9 255
	B.GT r4 r9 .LBB5_6
	B .LBB5_4
.LBB5_4:
	LDi r9 r3 -4
	STi r9 r3 -20
	LDi r10 r3 -5
	STi r10 r3 -19
	LDi r9 r3 -20
	LDi r10 r3 -19
	BL r1 CountNeighbours
	SHLi r4 r9 1
	STi r4 r3 -15
	LDi r9 r3 -5
	STi r9 r3 -18
	LDi r10 r3 -4
	STi r10 r3 -17
	LDi r9 r3 -18
	LDi r10 r3 -17
	BL r1 GetPixel
	STi r9 r3 -16
	LDi r9 r3 -16
	BL r1 BlackWhiteToBin
	LDi r4 r3 -15
	OR r4 r4 r9
	STi r4 r3 -6
	LDi r9 r3 -5
	STi r9 r3 -14
	LDi r10 r3 -4
	STi r10 r3 -13
	LDi r11 r3 -6
	STi r11 r3 -12
	LDi r9 r3 -14
	LDi r10 r3 -13
	LDi r11 r3 -12
	BL r1 simSetPixel
	B .LBB5_5
.LBB5_5:
	LDi r4 r3 -5
	ADDi r4 r4 1
	STi r4 r3 -5
	B .LBB5_3
.LBB5_6:
	B .LBB5_7
.LBB5_7:
	LDi r4 r3 -4
	ADDi r4 r4 1
	STi r4 r3 -4
	B .LBB5_1
.LBB5_8:
	MOVli r4 0
	STi r4 r3 -7
	B .LBB5_9
.LBB5_9:
	LDi r4 r3 -7
	MOVli r9 127
	B.GT r4 r9 .LBB5_28
	B .LBB5_10
.LBB5_10:
	MOVli r4 0
	STi r4 r3 -8
	B .LBB5_11
.LBB5_11:
	LDi r4 r3 -8
	MOVli r9 255
	B.GT r4 r9 .LBB5_26
	B .LBB5_12
.LBB5_12:
	LDi r9 r3 -8
	STi r9 r3 -22
	LDi r10 r3 -7
	STi r10 r3 -21
	LDi r9 r3 -22
	LDi r10 r3 -21
	BL r1 GetPixel
	STi r9 r3 -9
	LDi r4 r3 -9
	ANDi r4 r4 1
	STi r4 r3 -10
	LDi r4 r3 -9
	SHRAi r4 r4 1
	STi r4 r3 -11
	LDi r4 r3 -11
	MOVli r9 8
	B.GT r4 r9 .LBB5_14
	B .LBB5_13
.LBB5_13:
	LDi r4 r3 -11
	MOVhi r9 65535
	ORi r9 r9 65535
	B.GT r4 r9 .LBB5_15
	B .LBB5_14
.LBB5_14:
	BL r1 simBkpt
	B .LBB5_15
.LBB5_15:
	LDi r4 r3 -10
	MOVli r9 0
	B.NE r4 r9 .LBB5_18
	B .LBB5_16
.LBB5_16:
	LDi r4 r3 -11
	MOVli r9 3
	B.NE r4 r9 .LBB5_18
	B .LBB5_17
.LBB5_17:
	LDi r9 r3 -8
	STi r9 r3 -25
	LDi r10 r3 -7
	STi r10 r3 -24
	MOVhi r4 65535
	ORi r11 r4 0
	STi r11 r3 -23
	LDi r9 r3 -25
	LDi r10 r3 -24
	LDi r11 r3 -23
	BL r1 simSetPixel
	B .LBB5_24
.LBB5_18:
	LDi r4 r3 -10
	MOVli r9 1
	B.NE r4 r9 .LBB5_22
	B .LBB5_19
.LBB5_19:
	LDi r4 r3 -11
	MOVli r9 3
	B.GT r4 r9 .LBB5_21
	B .LBB5_20
.LBB5_20:
	LDi r4 r3 -11
	MOVli r9 1
	B.GT r4 r9 .LBB5_22
	B .LBB5_21
.LBB5_21:
	LDi r9 r3 -8
	STi r9 r3 -28
	LDi r10 r3 -7
	STi r10 r3 -27
	MOVli r11 0
	STi r11 r3 -26
	LDi r9 r3 -28
	LDi r10 r3 -27
	LDi r11 r3 -26
	BL r1 simSetPixel
	B .LBB5_23
.LBB5_22:
	LDi r9 r3 -8
	STi r9 r3 -31
	LDi r10 r3 -7
	STi r10 r3 -30
	LDi r11 r3 -10
	MOVli r4 0
	SUB r4 r4 r11
	MOVhi r11 65535
	ORi r11 r11 0
	AND r11 r4 r11
	STi r11 r3 -29
	LDi r9 r3 -31
	LDi r10 r3 -30
	LDi r11 r3 -29
	BL r1 simSetPixel
	B .LBB5_23
.LBB5_23:
	B .LBB5_24
.LBB5_24:
	B .LBB5_25
.LBB5_25:
	LDi r4 r3 -8
	ADDi r4 r4 1
	STi r4 r3 -8
	B .LBB5_11
.LBB5_26:
	B .LBB5_27
.LBB5_27:
	LDi r4 r3 -7
	ADDi r4 r4 1
	STi r4 r3 -7
	B .LBB5_9
.LBB5_28:
	BL r1 simFlush
	LDi r3 r2 28
	LDi r1 r2 29
	LDi r0 r2 30
	ADDi r2 r2 31
	BR r1

TestOnStartup:
	ADDi r2 r2 -9
	STi r0 r2 8
	STi r1 r2 7
	STi r3 r2 6
	ADDi r3 r2 9
	MOVhi r4 10
	ORi r11 r4 47802
	STi r11 r3 -4
	MOVli r9 12
	STi r9 r3 -6
	MOVli r10 44
	STi r10 r3 -5
	LDi r9 r3 -6
	LDi r10 r3 -5
	LDi r11 r3 -4
	BL r1 simSetPixel
	LDi r9 r3 -6
	LDi r10 r3 -5
	LDi r9 r3 -6
	LDi r10 r3 -5
	BL r1 GetPixel
	LDi r4 r3 -4
	B.EQ r9 r4 .LBB6_2
	B .LBB6_1
.LBB6_1:
	BL r1 simBkpt
	B .LBB6_2
.LBB6_2:
	MOVli r9 43
	STi r9 r3 -9
	MOVli r10 34
	STi r10 r3 -8
	MOVli r11 48858
	STi r11 r3 -7
	LDi r9 r3 -9
	LDi r10 r3 -8
	LDi r11 r3 -7
	BL r1 simSetPixel
	LDi r9 r3 -9
	LDi r10 r3 -8
	LDi r9 r3 -9
	LDi r10 r3 -8
	BL r1 GetPixel
	LDi r4 r3 -7
	B.EQ r9 r4 .LBB6_4
	B .LBB6_3
.LBB6_3:
	BL r1 simBkpt
	B .LBB6_4
.LBB6_4:
	LDi r3 r2 6
	LDi r1 r2 7
	LDi r0 r2 8
	ADDi r2 r2 9
	BR r1

main:
	ADDi r2 r2 -6
	STi r0 r2 5
	STi r1 r2 4
	STi r3 r2 3
	ADDi r3 r2 6
	MOVli r4 0
	STi r4 r3 -6
	STi r4 r3 -4
	BL r1 TestOnStartup
	BL r1 PrintRandomGrid
	LDi r4 r3 -6
	STi r4 r3 -5
	B .LBB7_1
.LBB7_1:
	LDi r4 r3 -5
	MOVli r9 254
	B.GT r4 r9 .LBB7_4
	B .LBB7_2
.LBB7_2:
	BL r1 PrintUpdatedGrid
	B .LBB7_3
.LBB7_3:
	LDi r4 r3 -5
	ADDi r4 r4 1
	STi r4 r3 -5
	B .LBB7_1
.LBB7_4:
	MOVli r9 0
	LDi r3 r2 3
	LDi r1 r2 4
	LDi r0 r2 5
	ADDi r2 r2 6
	BR r1
