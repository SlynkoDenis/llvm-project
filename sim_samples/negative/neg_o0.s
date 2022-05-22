	.text
	.file	"neg.c"
	.globl	get_neg
	.type	get_neg,@function
get_neg:
	ADDi	%r1, %r1, -8
	STi	%r3, %r1, 4
	ADDi	%r3, %r1, 8
	STi	%r10, %r3, -8
	LDi	%r9, %r3, -8
	MOVli	%r4, 0
	SUB	%r10, %r4, %r9
	LDi	%r3, %r1, 4
	ADDi	%r1, %r1, 8
	BR	%r0
.Lfunc_end0:
	.size	get_neg, .Lfunc_end0-get_neg

	.globl	main
	.type	main,@function
main:
	ADDi	%r1, %r1, -36
	STi	%r0, %r1, 32
	STi	%r2, %r1, 28
	STi	%r3, %r1, 24
	ADDi	%r3, %r1, 36
	MOVli	%r4, 0
	STi	%r4, %r3, -16
	MOVli	%r10, 9
	STi	%r10, %r3, -36
	LDi	%r10, %r3, -36
	BL	%r0, get_neg
	STi	%r10, %r3, -20
	MOVhi	%r4, 65535
	ORi	%r10, %r4, 65527
	STi	%r10, %r3, -32
	LDi	%r10, %r3, -32
	BL	%r0, get_neg
	STi	%r10, %r3, -24
	LDi	%r4, %r3, -20
	LDi	%r9, %r3, -24
	ADD	%r4, %r4, %r9
	STi	%r4, %r3, -28
	LDi	%r10, %r3, -28
	LDi	%r3, %r1, 24
	LDi	%r2, %r1, 28
	LDi	%r0, %r1, 32
	ADDi	%r1, %r1, 36
	BR	%r0
.Lfunc_end1:
	.size	main, .Lfunc_end1-main

	.ident	"clang version 14.0.0 (https://github.com/SlynkoDenis/llvm-project.git ca69b4d588678142eb03f83fd786e4f727f24270)"
	.section	".note.GNU-stack","",@progbits
