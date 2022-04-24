	.text
	.file	"main5.c"
	.globl	foo
	.type	foo,@function
foo:
	ADDi	%r1, %r1, -4
	STi	%r3, %r1, 0
	ADDi	%r3, %r1, 4
	MOVli	%r4, 5
	BLT	%r10, %r4, .LBB0_2
	DIVi	%r10, %r10, 5
.LBB0_2:
	LDi	%r3, %r1, 0
	ADDi	%r1, %r1, 4
	BR	%r0
.Lfunc_end0:
	.size	foo, .Lfunc_end0-foo

	.globl	main
	.type	main,@function
main:
	ADDi	%r1, %r1, -4
	STi	%r3, %r1, 0
	ADDi	%r3, %r1, 4
	MOVli	%r10, 3
	LDi	%r3, %r1, 0
	ADDi	%r1, %r1, 4
	BR	%r0
.Lfunc_end1:
	.size	main, .Lfunc_end1-main

	.ident	"clang version 14.0.0 (https://github.com/SlynkoDenis/llvm-project 4ad82873f8380d934883199e4712ffd9e32aa363)"
	.section	".note.GNU-stack","",@progbits
