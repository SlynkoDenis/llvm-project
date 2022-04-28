	.text
	.file	"nested_loop.c"
	.globl	loop
	.type	loop,@function
loop:
	ADDi	%r1, %r1, -36
	STi	%r0, %r1, 32
	STi	%r2, %r1, 28
	STi	%r3, %r1, 24
	STi	%r5, %r1, 20
	STi	%r6, %r1, 16
	STi	%r7, %r1, 12
	STi	%r8, %r1, 8
	ADDi	%r3, %r1, 36
	MOVli	%r4, 2
	ADDi	%r5, %r10, 0
	BGT	%r4, %r10, .LBB0_5
	MULi	%r4, %r5, 3
	MOVli	%r9, 4
	BGT	%r9, %r4, .LBB0_5
	SHRAi	%r9, %r4, 31
	SHRi	%r9, %r9, 30
	ADD	%r4, %r4, %r9
	SHRAi	%r9, %r4, 2
	SHLi	%r4, %r5, 1
	DIVi	%r8, %r4, 3
	ADDi	%r10, %r9, -1
	STi	%r9, %r3, -32
	ADDi	%r12, %r9, -2
	MOVli	%r6, 0
	ADDi	%r11, %r6, 0
	ADDi	%r13, %r6, 0
	BLR	%r0, __muldi3
	SHLi	%r4, %r11, 31
	SHRi	%r9, %r10, 1
	OR	%r4, %r9, %r4
	STi	%r4, %r3, -36
	MOVli	%r7, 1
	BGT	%r8, %r7, .LBB0_4
	ADDi	%r8, %r7, 0
.LBB0_4:
	ADDi	%r10, %r8, -1
	ADDi	%r12, %r8, -2
	ADDi	%r8, %r10, 0
	ADDi	%r11, %r6, 0
	ADDi	%r13, %r6, 0
	BLR	%r0, __muldi3
	SHLi	%r4, %r11, 31
	SHRi	%r9, %r10, 1
	OR	%r4, %r9, %r4
	ADDi	%r4, %r4, -1
	LDi	%r9, %r3, -32
	MUL	%r4, %r9, %r4
	ADD	%r4, %r4, %r5
	LDi	%r10, %r3, -36
	SUB	%r9, %r7, %r10
	MUL	%r9, %r8, %r9
	ADD	%r4, %r4, %r9
	SUB	%r4, %r4, %r10
	ADDi	%r5, %r4, 1
.LBB0_5:
	ADDi	%r10, %r5, 0
	LDi	%r8, %r1, 8
	LDi	%r7, %r1, 12
	LDi	%r6, %r1, 16
	LDi	%r5, %r1, 20
	LDi	%r3, %r1, 24
	LDi	%r2, %r1, 28
	LDi	%r0, %r1, 32
	ADDi	%r1, %r1, 36
	BR	%r0
.Lfunc_end0:
	.size	loop, .Lfunc_end0-loop

	.globl	main
	.type	main,@function
main:
	ADDi	%r1, %r1, -8
	STi	%r0, %r1, 4
	STi	%r3, %r1, 0
	ADDi	%r3, %r1, 8
	MOVhi	%r4, 65535
	ORi	%r10, %r4, 64674
	LDi	%r3, %r1, 0
	LDi	%r0, %r1, 4
	ADDi	%r1, %r1, 8
	BR	%r0
.Lfunc_end1:
	.size	main, .Lfunc_end1-main

	.ident	"clang version 14.0.0 (https://github.com/SlynkoDenis/llvm-project.git 32f5d6f862b3820149c3ee72278e6e99b8753770)"
	.section	".note.GNU-stack","",@progbits
