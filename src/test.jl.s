	.text
	.globl	main
main:
	movq %rsp, %rbp
	pushq $1
	pushq $3
	pushq $1
	pushq $2
	popq %rbx
	popq %rax
	popq %rdx
	popq %rcx
	cmpq %rax, $1
	jne exit
	cmpq %rcx, $1
	jne exit
	pushq $1
	addq %rcx, %rax
	pushq %rax
	movq $0, %rax
	ret
print_int:
	movq %rdi, %rsi
	movq $.Sprint_int, %rdi
	movq $0, %rax
	call printf
	ret
	pushq %rbp
	movq %rsp, %rbp
	popq %rbx
	popq %rax
	movq %rbp, %rsp
	popq %rbp
	ret
	.data
.Sprint_int:
	.string "%d\n"
