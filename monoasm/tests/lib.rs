#![feature(proc_macro_hygiene)]
extern crate monoasm;
extern crate monoasm_macro;
use monoasm::{Dest, JitMemory, Or, Reg};
use monoasm_macro::monoasm;

fn hello() -> (fn() -> i64) {
    let hello = "Hello World! Are you angry?\n";
    let mut jit: JitMemory = JitMemory::new();

    monoasm!(
        movq rdi, 1;
        movq r8, (jit.get_mem_addr() + 256);
        movq rax, (hello.as_ptr() as u64);
        movq [r8], rax;
        movq rsi, [r8];
        movq rdx, (hello.len() as u64);
        movq rax, 1;
        syscall;
        movq r15, (jit.get_mem_addr() + 256);
        movq [r15], 0x40;
    );

    let label = jit.label();
    jit.bind_label(label);
    let putchar_addr = libc::putchar as *const u8 as u64;

    monoasm!(
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

    // main()
    jit.pushq(Reg::Rbp);
    jit.movq(Or::Reg(Reg::Rbp), Or::Reg(Reg::Rsp));

    jit.movq(Or::Reg(Reg::Rdi), Or::Imm(10));
    let fac = jit.label();
    jit.call(Dest::Rel(fac));

    monoasm!(
        movq rsi, rax;
        movq rdi, (fmt.as_ptr() as u64);
        movq rcx, (printf_addr);
        movq rax, 0;
        call rcx;
    );

    jit.popq(Reg::Rbp);
    jit.ret();

    // fac()
    jit.bind_label(fac);
    jit.pushq(Reg::Rbp);
    jit.movq(Or::Reg(Reg::Rbp), Or::Reg(Reg::Rsp));
    jit.subq(Or::Reg(Reg::Rsp), Or::Imm(16));

    let l2 = jit.label();
    let l3 = jit.label();

    jit.movq(Or::IndD8(Reg::Rbp, -8), Or::Reg(Reg::Rdi));
    jit.cmpq(Or::IndD8(Reg::Rbp, -8), Or::Imm(1));
    jit.jne(l2);
    jit.movq(Or::Reg(Reg::Rax), Or::Imm(1));
    jit.jmp(l3);

    jit.bind_label(l2);
    jit.movq(Or::Reg(Reg::Rax), Or::IndD8(Reg::Rbp, -8));
    jit.subq(Or::Reg(Reg::Rax), Or::Imm(1));
    jit.movq(Or::Reg(Reg::Rdi), Or::Reg(Reg::Rax));
    jit.call(Dest::Rel(fac));
    jit.imull(Or::Reg(Reg::Rax), Or::IndD8(Reg::Rbp, -8));

    jit.bind_label(l3);
    jit.movq(Or::Reg(Reg::Rsp), Or::Reg(Reg::Rbp));
    jit.popq(Reg::Rbp);
    jit.ret();

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
