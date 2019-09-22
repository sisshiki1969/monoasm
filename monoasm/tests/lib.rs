#![feature(proc_macro_hygiene)]
extern crate monoasm;
extern crate monoasm_macro;
use monoasm::{Dest, JitMemory, Or, Reg};
use monoasm_macro::monoasm;

fn hello() -> (fn() -> i64) {
    let hello = "Hello World! Are you angry?\n";
    let mut jit: JitMemory = JitMemory::new();

    monoasm!(
        movq rdi, Or::Imm(1);
        movq r8, Or::Imm(jit.get_mem_addr() + 256);
        movq rax, Or::Imm(hello.as_ptr() as u64);

        movq r8, rax;
        movq rsi, r8;
        movq rdx, Or::Imm(hello.len() as u64);
        movq rax, Or::Imm(1);
        syscall;
    );

    jit.movq(Or::Reg(Reg::R15), Or::Imm(jit.get_mem_addr() as u64 + 256));
    jit.movq(Or::Ind(Reg::R15), Or::Imm(0x40));
    let label = jit.label();
    jit.bind_label(label);
    let putchar_addr = libc::putchar as *const u8 as u64;
    jit.addq(Or::Ind(Reg::R15), Or::Imm(1));
    jit.movq(Or::Reg(Reg::Rax), Or::Imm(putchar_addr));
    jit.movq(Or::Reg(Reg::Rdi), Or::Ind(Reg::R15));
    jit.call(Dest::Reg(Reg::Rax));

    jit.cmpq(Or::Ind(Reg::R15), Or::Imm(0x60));
    jit.jne(label);

    jit.movq(Or::Reg(Reg::Rax), Or::Imm(putchar_addr));
    jit.movq(Or::Reg(Reg::Rdi), Or::Imm('\n' as u64));
    jit.call(Dest::Reg(Reg::Rax));

    jit.ret();
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

    jit.movq(Or::Reg(Reg::Rsi), Or::Reg(Reg::Rax));
    jit.movq(Or::Reg(Reg::Rdi), Or::Imm(fmt.as_ptr() as u64));
    jit.movq(Or::Reg(Reg::Rcx), Or::Imm(printf_addr));
    jit.movq(Or::Reg(Reg::Rax), Or::Imm(0));
    jit.call(Dest::Reg(Reg::Rcx));

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
