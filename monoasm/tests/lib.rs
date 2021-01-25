//#![feature(proc_macro_hygiene)]
#![feature(asm)]
extern crate monoasm;
extern crate monoasm_macro;
use monoasm::test;
use monoasm::JitMemory;
use monoasm_macro::monoasm;
mod fact;
mod fibo;

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    #[ignore]
    fn mul() {
        let mut jit: JitMemory = JitMemory::new();
        monoasm!(jit,
            imul rax, rax;
            imul rax, rcx;
            imul rax, rdx;
            imul rax, rbx;
            imul rax, rsp;
            imul rax, rbp;
            imul rax, rsi;
            imul rax, rdi;
            imul rax, r8;
            imul rax, r9;
            imul rax, r10;
            imul rax, r11;
            imul rax, r12;
            imul rax, r13;
            imul rax, r14;
            imul rax, r15;
            imul rax, [rax];
            imul rax, [rcx];
            imul rax, [rdx];
            imul rax, [rbx];
            imul rax, [rsp];
            imul rax, [rbp];
            imul rax, [rsi];
            imul rax, [rdi];
            imul rax, [r8];
            imul rax, [r9];
            imul rax, [r10];
            imul rax, [r11];
            imul rax, [r12];
            imul rax, [r13];
            imul rax, [r14];
            imul rax, [r15];

            imul R(0), [rax];
            imul R(0), [rcx];
            imul R(0), [rdx];
            imul R(0), [rbx];
            imul R(0), [rsp];
            imul R(0), [rbp];
            imul R(0), [rsi];
            imul R(0), [rdi];
            imul R(0), [r8];
            imul R(0), [r9];
            imul R(0), [r10];
            imul R(0), [r11];
            imul R(0), [r12];
            imul R(0), [r13];
            imul R(0), [r14];
            imul R(0), [r15];

            ret;
        );
        let func = jit.finalize::<u64, u64>();
        func(0);
    }
}

fn syscall() -> fn(()) -> u64 {
    let hello = "こんにちは世界\n\0";
    let mut jit: JitMemory = JitMemory::new();
    monoasm!(jit,
        movq rdi, 1;
        movq rsi, (hello.as_ptr() as u64);
        movq rdx, (hello.len() as u64);
        movq rax, 1;
        syscall;
        ret;
    );

    jit.finalize()
}

fn hello() -> fn(()) -> () {
    let hello = "hello world!\n\0";
    let mut jit: JitMemory = JitMemory::new();
    let label = jit.label();
    let dump_addr: u64 = test::DUMP as u64;

    monoasm!(jit,
        // prologue
        pushq rbp;
        movq rbp, rsp;
        movq rax, 0x10;
        movq rsi, 0x12;
        movq rdx, 0x13;
        movq rcx, 0x14;
        movq r8, 0x15;
        movq rdi, (dump_addr);
        call rdi;
        movq r15, (hello.as_ptr() as u64);
    label:
        movq rdi, [r15];
        movq rax, (test::PUTC as u64);
        call rax;
        addq r15, 1;
        cmpq [r15], 0x00;
        jne label;
        movq rdi, ('\n' as u64);
        movq rax, (test::PUTC as u64);
        call rax;

        // epilogue
        movq rsp, rbp;
        popq rbp;
        ret;
    );
    jit.finalize()
    //jit.p();
}

#[test]
fn system_call() {
    let func = syscall();
    let ret = func(());
    assert_eq!(23, ret);
}

#[test]
fn hello_world() {
    let func = hello();
    func(());
}
