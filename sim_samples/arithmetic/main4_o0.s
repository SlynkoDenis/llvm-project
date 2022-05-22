	.text
	.file	"main4.c"
	.globl	main
	.type	main,@function
main:
	ADDi	%r1, %r1, -24
	STi	%r0, %r1, 20
	STi	%r3, %r1, 16
	ADDi	%r3, %r1, 24
	MOVli	%r4, 0
	STi	%r4, %r3, -12
	MOVli	%r4, 10
	STi	%r4, %r3, -16
	LDi	%r4, %r3, -16
	DIVi	%r4, %r4, 3
	STi	%r4, %r3, -20
	LDi	%r4, %r3, -20
	MULi	%r4, %r4, 5
	DIVi	%r4, %r4, 7
	ADDi	%r4, %r4, 1
	STi	%r4, %r3, -24
	LDi	%r4, %r3, -24
	REMi	%r4, %r4, 3
	STi	%r4, %r3, -24
	LDi	%r10, %r3, -24
	LDi	%r3, %r1, 16
	LDi	%r0, %r1, 20
	ADDi	%r1, %r1, 24
	BR	%r0
.Lfunc_end0:
	.size	main, .Lfunc_end0-main

	.ident	"clang version 14.0.0 (https://github.com/SlynkoDenis/llvm-project.git 32f5d6f862b3820149c3ee72278e6e99b8753770)"
	.section	".note.GNU-stack","",@progbits
