#![feature(proc_macro_hygiene)]
extern crate monoasm;
extern crate monoasm_macro;
use monoasm::JitMemory;
use monoasm_macro::monoasm;

fn fac() -> fn(()) -> u64 {
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

fn main() {
    let func = fac();
    let ret = func(());
    println!("returned value:{}", ret);
}
