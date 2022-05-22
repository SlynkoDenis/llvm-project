	.text
	.file	"neg.c"
	.globl	get_neg
	.type	get_neg,@function
get_neg:
	ADDi	%r1, %r1, -4
	STi	%r3, %r1, 0
	ADDi	%r3, %r1, 4
	MOVli	%r4, 0
	SUB	%r10, %r4, %r10
	LDi	%r3, %r1, 0
	ADDi	%r1, %r1, 4
	BR	%r0
.Lfunc_end0:
	.size	get_neg, .Lfunc_end0-get_neg

	.globl	main
	.type	main,@function
main:
	ADDi	%r1, %r1, -4
	STi	%r3, %r1, 0
	ADDi	%r3, %r1, 4
	MOVli	%r10, 0
	LDi	%r3, %r1, 0
	ADDi	%r1, %r1, 4
	BR	%r0
.Lfunc_end1:
	.size	main, .Lfunc_end1-main

	.ident	"clang version 14.0.0 (https://github.com/SlynkoDenis/llvm-project.git ca69b4d588678142eb03f83fd786e4f727f24270)"
	.section	".note.GNU-stack","",@progbits
