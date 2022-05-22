	.text
	.file	"main5.c"
	.globl	foo
	.type	foo,@function
foo:
	ADDi	%r1, %r1, -8
	STi	%r0, %r1, 4
	STi	%r3, %r1, 0
	ADDi	%r3, %r1, 8
	MOVli	%r4, 5
	BLE	%r10, %r4, .LBB0_2
	DIVi	%r10, %r10, 5
.LBB0_2:
	LDi	%r3, %r1, 0
	LDi	%r0, %r1, 4
	ADDi	%r1, %r1, 8
	BR	%r0
.Lfunc_end0:
	.size	foo, .Lfunc_end0-foo

	.globl	main
	.type	main,@function
main:
	ADDi	%r1, %r1, -8
	STi	%r0, %r1, 4
	STi	%r3, %r1, 0
	ADDi	%r3, %r1, 8
	MOVli	%r10, 3
	LDi	%r3, %r1, 0
	LDi	%r0, %r1, 4
	ADDi	%r1, %r1, 8
	BR	%r0
.Lfunc_end1:
	.size	main, .Lfunc_end1-main

	.ident	"clang version 14.0.0 (https://github.com/SlynkoDenis/llvm-project.git 32f5d6f862b3820149c3ee72278e6e99b8753770)"
	.section	".note.GNU-stack","",@progbits
