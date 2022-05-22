	.text
	.file	"main3.c"
	.globl	mult
	.type	mult,@function
mult:
	ADDi	%r1, %r1, -12
	STi	%r0, %r1, 8
	STi	%r3, %r1, 4
	ADDi	%r3, %r1, 12
	STi	%r10, %r3, -12
	LDi	%r4, %r3, -12
	MULi	%r10, %r4, 3
	LDi	%r3, %r1, 4
	LDi	%r0, %r1, 8
	ADDi	%r1, %r1, 12
	BR	%r0
.Lfunc_end0:
	.size	mult, .Lfunc_end0-mult

	.globl	main
	.type	main,@function
main:
	ADDi	%r1, %r1, -28
	STi	%r0, %r1, 24
	STi	%r2, %r1, 20
	STi	%r3, %r1, 16
	ADDi	%r3, %r1, 28
	MOVli	%r4, 0
	STi	%r4, %r3, -16
	MOVli	%r4, 2
	STi	%r4, %r3, -20
	LDi	%r10, %r3, -20
	STi	%r10, %r3, -28
	LDi	%r10, %r3, -28
	BL	%r0, mult
	STi	%r10, %r3, -24
	LDi	%r10, %r3, -24
	LDi	%r3, %r1, 16
	LDi	%r2, %r1, 20
	LDi	%r0, %r1, 24
	ADDi	%r1, %r1, 28
	BR	%r0
.Lfunc_end1:
	.size	main, .Lfunc_end1-main

	.ident	"clang version 14.0.0 (https://github.com/SlynkoDenis/llvm-project.git 32f5d6f862b3820149c3ee72278e6e99b8753770)"
	.section	".note.GNU-stack","",@progbits
