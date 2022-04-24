	.text
	.file	"main5.c"
	.globl	foo
	.type	foo,@function
foo:
	ADDi	%r1, %r1, -12
	STi	%r3, %r1, 8
	ADDi	%r3, %r1, 12
	STi	%r10, %r3, -12
	LDi	%r4, %r3, -12
	MOVli	%r9, 6
	BLT	%r4, %r9, .LBB0_2
	B	.LBB0_1
.LBB0_1:
	LDi	%r4, %r3, -12
	DIVi	%r4, %r4, 5
	STi	%r4, %r3, -8
	B	.LBB0_3
.LBB0_2:
	LDi	%r4, %r3, -12
	STi	%r4, %r3, -8
	B	.LBB0_3
.LBB0_3:
	LDi	%r10, %r3, -8
	LDi	%r3, %r1, 8
	ADDi	%r1, %r1, 12
	BR	%r0
.Lfunc_end0:
	.size	foo, .Lfunc_end0-foo

	.globl	main
	.type	main,@function
main:
	ADDi	%r1, %r1, -28
	STi	%r2, %r1, 24
	STi	%r3, %r1, 20
	ADDi	%r3, %r1, 28
	MOVli	%r4, 0
	STi	%r4, %r3, -12
	MOVli	%r10, 3
	STi	%r10, %r3, -28
	LDi	%r10, %r3, -28
	BL	%r0, foo
	STi	%r10, %r3, -16
	MOVli	%r10, 7
	STi	%r10, %r3, -24
	LDi	%r10, %r3, -24
	BL	%r0, foo
	STi	%r10, %r3, -20
	LDi	%r4, %r3, -16
	LDi	%r9, %r3, -20
	MUL	%r10, %r4, %r9
	LDi	%r3, %r1, 20
	LDi	%r2, %r1, 24
	ADDi	%r1, %r1, 28
	BR	%r0
.Lfunc_end1:
	.size	main, .Lfunc_end1-main

	.ident	"clang version 14.0.0 (https://github.com/SlynkoDenis/llvm-project 4ad82873f8380d934883199e4712ffd9e32aa363)"
	.section	".note.GNU-stack","",@progbits
