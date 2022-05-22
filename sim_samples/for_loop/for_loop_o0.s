	.text
	.file	"for_loop.c"
	.globl	main
	.type	main,@function
main:
	ADDi	%r1, %r1, -20
	STi	%r0, %r1, 16
	STi	%r3, %r1, 12
	ADDi	%r3, %r1, 20
	MOVli	%r4, 0
	STi	%r4, %r3, -12
	STi	%r4, %r3, -16
	STi	%r4, %r3, -20
	B	.LBB0_1
.LBB0_1:
	LDi	%r4, %r3, -20
	MOVli	%r9, 99
	BGT	%r4, %r9, .LBB0_4
	B	.LBB0_2
.LBB0_2:
	LDi	%r4, %r3, -20
	SHRi	%r9, %r4, 31
	ADD	%r9, %r4, %r9
	SHRAi	%r9, %r9, 1
	SUB	%r9, %r4, %r9
	LDi	%r4, %r3, -16
	ADD	%r4, %r4, %r9
	STi	%r4, %r3, -16
	B	.LBB0_3
.LBB0_3:
	LDi	%r4, %r3, -20
	ADDi	%r4, %r4, 1
	STi	%r4, %r3, -20
	B	.LBB0_1
.LBB0_4:
	LDi	%r10, %r3, -16
	LDi	%r3, %r1, 12
	LDi	%r0, %r1, 16
	ADDi	%r1, %r1, 20
	BR	%r0
.Lfunc_end0:
	.size	main, .Lfunc_end0-main

	.ident	"clang version 14.0.0 (https://github.com/SlynkoDenis/llvm-project.git 32f5d6f862b3820149c3ee72278e6e99b8753770)"
	.section	".note.GNU-stack","",@progbits
