	.text
	.file	"main3.c"
	.globl	mult
	.type	mult,@function
mult:
	ADDi	%r1, %r1, -8
	STi	%r3, %r1, 4
	ADDi	%r3, %r1, 8
	STi	%r10, %r3, -8
	LDi	%r4, %r3, -8
	MULi	%r10, %r4, 3
	LDi	%r3, %r1, 4
	ADDi	%r1, %r1, 8
	BR	%r0
.Lfunc_end0:
	.size	mult, .Lfunc_end0-mult

	.globl	main
	.type	main,@function
main:
	ADDi	%r1, %r1, -24
	STi	%r2, %r1, 20
	STi	%r3, %r1, 16
	ADDi	%r3, %r1, 24
	MOVli	%r4, 0
	STi	%r4, %r3, -12
	MOVli	%r4, 2
	STi	%r4, %r3, -16
	LDi	%r10, %r3, -16
	STi	%r10, %r3, -24
	LDi	%r10, %r3, -24
	BL	%r0, mult
	STi	%r10, %r3, -20
	LDi	%r10, %r3, -20
	LDi	%r3, %r1, 16
	LDi	%r2, %r1, 20
	ADDi	%r1, %r1, 24
	BR	%r0
.Lfunc_end1:
	.size	main, .Lfunc_end1-main

	.ident	"clang version 14.0.0 (https://github.com/SlynkoDenis/llvm-project 4ad82873f8380d934883199e4712ffd9e32aa363)"
	.section	".note.GNU-stack","",@progbits
