	.text
	.file	"main4.c"
	.globl	main
	.type	main,@function
main:
	ADDi	%r1, %r1, -20
	STi	%r3, %r1, 16
	ADDi	%r3, %r1, 20
	MOVli	%r4, 0
	STi	%r4, %r3, -8
	MOVli	%r4, 10
	STi	%r4, %r3, -12
	LDi	%r4, %r3, -12
	DIVi	%r4, %r4, 3
	STi	%r4, %r3, -16
	LDi	%r4, %r3, -16
	MULi	%r4, %r4, 5
	DIVi	%r4, %r4, 7
	ADDi	%r4, %r4, 1
	STi	%r4, %r3, -20
	LDi	%r4, %r3, -20
	REMi	%r4, %r4, 3
	STi	%r4, %r3, -20
	LDi	%r10, %r3, -20
	LDi	%r3, %r1, 16
	ADDi	%r1, %r1, 20
	BR	%r0
.Lfunc_end0:
	.size	main, .Lfunc_end0-main

	.ident	"clang version 14.0.0 (https://github.com/SlynkoDenis/llvm-project 4ad82873f8380d934883199e4712ffd9e32aa363)"
	.section	".note.GNU-stack","",@progbits
