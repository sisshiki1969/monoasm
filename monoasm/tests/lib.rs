//#![feature(proc_macro_hygiene)]
#![feature(asm)]
extern crate monoasm;
extern crate monoasm_macro;
use monoasm::test;
use monoasm::JitMemory;
use monoasm_macro::monoasm;
mod fact;
mod fibo;

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
