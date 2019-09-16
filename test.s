.intel_syntax noprefix
.data
msg:
    .ascii  "hello world\n"
.text
.globl main
main:
    mov rax, 1
    mov rdi, 1
    mov rsi, msg
    mov rdx, 20
    syscall
    ret
