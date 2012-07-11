	.section	__TEXT,__text,regular,pure_instructions
	.globl	_p1
	.align	4, 0x90
_p1:                                    ## @p1
## BB#0:
	subq	$24, %rsp
	movl	%edi, 20(%rsp)
	movq	%rsi, 8(%rsp)
	movq	___stdoutp@GOTPCREL(%rip), %rax
	movq	(%rax), %rdi
	movl	20(%rsp), %edx
	leaq	L_.str(%rip), %rsi
	xorb	%al, %al
	callq	_fprintf
	movq	8(%rsp), %rax
	movl	$1, (%rax)
	addq	$24, %rsp
	ret

	.globl	_p2
	.align	4, 0x90
_p2:                                    ## @p2
## BB#0:
	subq	$24, %rsp
	movl	%edi, 20(%rsp)
	movl	%esi, 16(%rsp)
	movq	%rdx, 8(%rsp)
	movq	___stdoutp@GOTPCREL(%rip), %rax
	movq	(%rax), %rdi
	movl	16(%rsp), %ecx
	movl	20(%rsp), %edx
	leaq	L_.str1(%rip), %rsi
	xorb	%al, %al
	callq	_fprintf
	movq	8(%rsp), %rax
	movl	$0, (%rax)
	addq	$24, %rsp
	ret

	.globl	_print_array
	.align	4, 0x90
_print_array:                           ## @print_array
## BB#0:
	pushq	%r14
	pushq	%rbx
	subq	$40, %rsp
	movl	%edi, 36(%rsp)
	movq	%rsi, 24(%rsp)
	movq	%rdx, 16(%rsp)
	movl	$0, 12(%rsp)
	movq	___stdoutp@GOTPCREL(%rip), %r14
	leaq	L_.str2(%rip), %rbx
	jmp	LBB2_1
	.align	4, 0x90
LBB2_2:                                 ##   in Loop: Header=BB2_1 Depth=1
	movslq	12(%rsp), %rax
	movq	24(%rsp), %rcx
	movl	(%rcx,%rax,4), %edx
	movq	(%r14), %rdi
	movq	%rbx, %rsi
	xorb	%al, %al
	callq	_fprintf
	incl	12(%rsp)
LBB2_1:                                 ## =>This Inner Loop Header: Depth=1
	movl	12(%rsp), %eax
	cmpl	36(%rsp), %eax
	jl	LBB2_2
## BB#3:
	movq	___stdoutp@GOTPCREL(%rip), %rax
	movq	(%rax), %rdi
	leaq	L_.str3(%rip), %rsi
	xorb	%al, %al
	callq	_fprintf
	movq	16(%rsp), %rax
	movl	12(%rsp), %ecx
	movl	%ecx, (%rax)
	addq	$40, %rsp
	popq	%rbx
	popq	%r14
	ret

	.globl	_print_array2
	.align	4, 0x90
_print_array2:                          ## @print_array2
## BB#0:
	pushq	%r15
	pushq	%r14
	pushq	%rbx
	subq	$32, %rsp
	movl	%edi, 28(%rsp)
	movq	%rsi, 16(%rsp)
	movq	%rdx, 8(%rsp)
	movl	28(%rsp), %ebx
	movl	$0, 4(%rsp)
	movl	$0, (%rsp)
	shlq	$2, %rbx
	movq	___stdoutp@GOTPCREL(%rip), %r15
	leaq	L_.str2(%rip), %r14
	jmp	LBB3_1
	.align	4, 0x90
LBB3_5:                                 ##   in Loop: Header=BB3_1 Depth=1
	incl	4(%rsp)
LBB3_1:                                 ## =>This Loop Header: Depth=1
                                        ##     Child Loop BB3_3 Depth 2
	movl	4(%rsp), %eax
	cmpl	28(%rsp), %eax
	jge	LBB3_6
## BB#2:                                ##   in Loop: Header=BB3_1 Depth=1
	movl	$0, (%rsp)
	jmp	LBB3_3
	.align	4, 0x90
LBB3_4:                                 ##   in Loop: Header=BB3_3 Depth=2
	movslq	4(%rsp), %rax
	imulq	%rbx, %rax
	andq	$-4, %rax
	addq	16(%rsp), %rax
	movslq	(%rsp), %rcx
	movl	(%rax,%rcx,4), %edx
	movq	(%r15), %rdi
	movq	%r14, %rsi
	xorb	%al, %al
	callq	_fprintf
	incl	(%rsp)
LBB3_3:                                 ##   Parent Loop BB3_1 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	movl	(%rsp), %eax
	cmpl	28(%rsp), %eax
	jge	LBB3_5
	jmp	LBB3_4
LBB3_6:
	movq	___stdoutp@GOTPCREL(%rip), %rax
	movq	(%rax), %rdi
	leaq	L_.str3(%rip), %rsi
	xorb	%al, %al
	callq	_fprintf
	movl	4(%rsp), %eax
	addl	(%rsp), %eax
	movq	8(%rsp), %rcx
	movl	%eax, (%rcx)
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	ret

	.globl	_print_array3
	.align	4, 0x90
_print_array3:                          ## @print_array3
## BB#0:
	pushq	%r15
	pushq	%r14
	pushq	%r12
	pushq	%rbx
	subq	$40, %rsp
	movl	%edi, 36(%rsp)
	movq	%rsi, 24(%rsp)
	movq	%rdx, 16(%rsp)
	movl	36(%rsp), %r15d
	leaq	(,%r15,4), %rbx
	imulq	%rbx, %r15
	movl	$0, 12(%rsp)
	movl	$0, 8(%rsp)
	movl	$0, 4(%rsp)
	movq	___stdoutp@GOTPCREL(%rip), %r12
	leaq	L_.str2(%rip), %r14
	jmp	LBB4_1
	.align	4, 0x90
LBB4_8:                                 ##   in Loop: Header=BB4_1 Depth=1
	incl	12(%rsp)
LBB4_1:                                 ## =>This Loop Header: Depth=1
                                        ##     Child Loop BB4_3 Depth 2
                                        ##       Child Loop BB4_5 Depth 3
	movl	12(%rsp), %eax
	cmpl	36(%rsp), %eax
	jge	LBB4_9
## BB#2:                                ##   in Loop: Header=BB4_1 Depth=1
	movl	$0, 8(%rsp)
	jmp	LBB4_3
	.align	4, 0x90
LBB4_7:                                 ##   in Loop: Header=BB4_3 Depth=2
	incl	8(%rsp)
LBB4_3:                                 ##   Parent Loop BB4_1 Depth=1
                                        ## =>  This Loop Header: Depth=2
                                        ##       Child Loop BB4_5 Depth 3
	movl	8(%rsp), %eax
	cmpl	36(%rsp), %eax
	jge	LBB4_8
## BB#4:                                ##   in Loop: Header=BB4_3 Depth=2
	movl	$0, 4(%rsp)
	jmp	LBB4_5
	.align	4, 0x90
LBB4_6:                                 ##   in Loop: Header=BB4_5 Depth=3
	movslq	8(%rsp), %rax
	imulq	%rbx, %rax
	andq	$-4, %rax
	movslq	12(%rsp), %rcx
	imulq	%r15, %rcx
	andq	$-4, %rcx
	addq	24(%rsp), %rcx
	addq	%rax, %rcx
	movslq	4(%rsp), %rax
	movl	(%rcx,%rax,4), %edx
	movq	(%r12), %rdi
	movq	%r14, %rsi
	xorb	%al, %al
	callq	_fprintf
	incl	4(%rsp)
LBB4_5:                                 ##   Parent Loop BB4_1 Depth=1
                                        ##     Parent Loop BB4_3 Depth=2
                                        ## =>    This Inner Loop Header: Depth=3
	movl	4(%rsp), %eax
	cmpl	36(%rsp), %eax
	jge	LBB4_7
	jmp	LBB4_6
LBB4_9:
	movq	___stdoutp@GOTPCREL(%rip), %rax
	movq	(%rax), %rdi
	leaq	L_.str3(%rip), %rsi
	xorb	%al, %al
	callq	_fprintf
	movl	12(%rsp), %eax
	addl	8(%rsp), %eax
	addl	4(%rsp), %eax
	movq	16(%rsp), %rcx
	movl	%eax, (%rcx)
	addq	$40, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	ret

	.section	__TEXT,__const
	.align	4                       ## @.str
L_.str:
	.asciz	 "Got a single value: %d\n"

	.align	4                       ## @.str1
L_.str1:
	.asciz	 "Got values: %d, %d\n"

L_.str2:                                ## @.str2
	.asciz	 "%d\t"

L_.str3:                                ## @.str3
	.asciz	 "\n"


.subsections_via_symbols
