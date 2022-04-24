	.text
	.file	"main.c"
	.globl	main
	.type	main,@function
main:
	ADDi	%r1, %r1, -8
	STi	%r3, %r1, 4
	ADDi	%r3, %r1, 8
	MOVli	%r10, 0
	STi	%r10, %r3, -8
	LDi	%r3, %r1, 4
	ADDi	%r1, %r1, 8
	BR	%r0
.Lfunc_end0:
	.size	main, .Lfunc_end0-main

	.ident	"clang version 14.0.0 (https://github.com/SlynkoDenis/llvm-project 4ad82873f8380d934883199e4712ffd9e32aa363)"
	.section	".note.GNU-stack","",@progbits
