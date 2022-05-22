	.text
	.file	"nested_loop.c"
	.globl	loop
	.type	loop,@function
loop:
	ADDi	%r1, %r1, -28
	STi	%r0, %r1, 24
	STi	%r3, %r1, 20
	ADDi	%r3, %r1, 28
	STi	%r10, %r3, -12
	LDi	%r4, %r3, -12
	SHLi	%r4, %r4, 1
	DIVi	%r4, %r4, 3
	STi	%r4, %r3, -16
	LDi	%r4, %r3, -12
	MULi	%r4, %r4, 3
	SHRAi	%r9, %r4, 31
	SHRi	%r9, %r9, 30
	ADD	%r4, %r4, %r9
	SHRAi	%r4, %r4, 2
	STi	%r4, %r3, -20
	MOVli	%r4, 0
	STi	%r4, %r3, -24
	B	.LBB0_1
.LBB0_1:
	LDi	%r9, %r3, -24
	LDi	%r4, %r3, -16
	BLE	%r4, %r9, .LBB0_8
	B	.LBB0_2
.LBB0_2:
	MOVli	%r4, 0
	STi	%r4, %r3, -28
	B	.LBB0_3
.LBB0_3:
	LDi	%r9, %r3, -28
	LDi	%r4, %r3, -20
	BLE	%r4, %r9, .LBB0_6
	B	.LBB0_4
.LBB0_4:
	LDi	%r4, %r3, -24
	LDi	%r9, %r3, -28
	SUB	%r9, %r4, %r9
	LDi	%r4, %r3, -12
	ADD	%r4, %r4, %r9
	STi	%r4, %r3, -12
	B	.LBB0_5
.LBB0_5:
	LDi	%r4, %r3, -28
	ADDi	%r4, %r4, 1
	STi	%r4, %r3, -28
	B	.LBB0_3
.LBB0_6:
	B	.LBB0_7
.LBB0_7:
	LDi	%r4, %r3, -24
	ADDi	%r4, %r4, 1
	STi	%r4, %r3, -24
	B	.LBB0_1
.LBB0_8:
	LDi	%r10, %r3, -12
	LDi	%r3, %r1, 20
	LDi	%r0, %r1, 24
	ADDi	%r1, %r1, 28
	BR	%r0
.Lfunc_end0:
	.size	loop, .Lfunc_end0-loop

	.globl	main
	.type	main,@function
main:
	ADDi	%r1, %r1, -24
	STi	%r0, %r1, 20
	STi	%r2, %r1, 16
	STi	%r3, %r1, 12
	ADDi	%r3, %r1, 24
	MOVli	%r4, 0
	STi	%r4, %r3, -16
	MOVli	%r10, 35
	STi	%r10, %r3, -24
	LDi	%r10, %r3, -24
	BL	%r0, loop
	STi	%r10, %r3, -20
	LDi	%r10, %r3, -20
	LDi	%r3, %r1, 12
	LDi	%r2, %r1, 16
	LDi	%r0, %r1, 20
	ADDi	%r1, %r1, 24
	BR	%r0
.Lfunc_end1:
	.size	main, .Lfunc_end1-main

	.ident	"clang version 14.0.0 (https://github.com/SlynkoDenis/llvm-project.git 32f5d6f862b3820149c3ee72278e6e99b8753770)"
	.section	".note.GNU-stack","",@progbits
