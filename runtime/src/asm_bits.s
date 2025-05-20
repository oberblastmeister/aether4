.text
.globl c0_runtime_call
c0_runtime_call:
    pushq %rbx
    pushq %r12
    pushq %r13
    pushq %r14
    pushq %r15
    # save the function pointer
    movq %rdi, %r11
    # move arguments into c0 calling convention
    movq %rsi, %rax
    movq %rdx, %rbx
    movq %rcx, %rdi
    callq *%r11
    popq %r15
    popq %r14
    popq %r13
    popq %r12
    popq %rbx
    ret
    