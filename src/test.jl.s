	.text
	.globl	main
main:
	movq %rsp, %rbp
	pushq $1
	pushq $2
	pushq $1
	pushq $3
	popq %rbx
	popq %rax
	popq %rdx
	popq %rcx
	cmpq $1, %rax
	jne exit
	cmpq $1, %rcx
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
	.data
.Sprint_int:
	.string "%d\n"
