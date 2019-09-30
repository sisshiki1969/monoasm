#![feature(proc_macro_hygiene)]
extern crate monoasm;
extern crate monoasm_macro;
use monoasm::{JitMemory};
use monoasm_macro::monoasm;

fn hello() -> (fn() -> i64) {
    let hello = "Hello World! Are you angry?\n\0";
    let mut jit: JitMemory = JitMemory::new();
    let label = jit.label();
    let putchar_addr = libc::putchar as *const u8 as u64;

    monoasm!(
        movq rdi, 1;
        movq rsi, (hello.as_ptr() as u64);
        movq rdx, (hello.len() as u64);
        movq rax, 1;
        syscall;
        movq r15, (jit.get_mem_addr() + 256);
        movq [r15], 0x40;
    label:
        addq [r15], 1;
        movq rax, (putchar_addr);
        movq rdi, [r15];
        call rax;
        cmpq [r15], 0x60;
        jne label;
        movq rax, (putchar_addr);
        movq rdi, ('\n' as u64);
        call rax;
        ret;
    );

    jit.finalize()
    //jit.p();
}

fn fac() -> (fn() -> i64) {
    let fmt = "%d\n\0";
    let mut jit: JitMemory = JitMemory::new();
    let printf_addr = libc::printf as u64;
    let fac = jit.label();
    let l2 = jit.label();
    let l3 = jit.label();

    // main()
    monoasm!(
        pushq rbp;
        movq rbp, rsp;
        movq rdi, 10;
        call fac;

        movq rsi, rax;
        movq rdi, (fmt.as_ptr() as u64);
        movq rcx, (printf_addr);
        movq rax, 0;
        call rcx;
        popq rbp;
        ret;

    fac:
        pushq rbp;
        movq rbp, rsp;
        subq rsp, 16;
        movq [rbp-8], rdi;
        cmpq [rbp-8], 1;
        jne l2;
        movq rax, 1;
        jmp l3;
    l2:
        movq rax, [rbp-8];
        subq rax, 1;
        movq rdi, rax;
        call fac;
        imull rax, [rbp-8];
    l3:
        movq rsp, rbp;
        popq rbp;
        ret;
    );

    jit.finalize()
}

#[test]
fn factorial() {
    let func = fac();
    let ret = func();
    println!("returned value:{}", ret);
}

#[test]
fn hello_world() {
    let func = hello();
    let ret = func();
    println!("returned value:{}", ret);
}
