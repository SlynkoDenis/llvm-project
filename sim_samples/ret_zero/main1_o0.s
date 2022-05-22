	.text
	.file	"main1.c"
	.globl	main
	.type	main,@function
main:
	ADDi	%r1, %r1, -12
	STi	%r0, %r1, 8
	STi	%r3, %r1, 4
	ADDi	%r3, %r1, 12
	MOVli	%r10, 0
	STi	%r10, %r3, -12
	LDi	%r3, %r1, 4
	LDi	%r0, %r1, 8
	ADDi	%r1, %r1, 12
	BR	%r0
.Lfunc_end0:
	.size	main, .Lfunc_end0-main

	.ident	"clang version 14.0.0 (https://github.com/SlynkoDenis/llvm-project.git 32f5d6f862b3820149c3ee72278e6e99b8753770)"
	.section	".note.GNU-stack","",@progbits
