#![feature(proc_macro_hygiene)]
extern crate monoasm;
extern crate monoasm_macro;
use monoasm::{JitMemory};
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

fn main() {
    let func = hello();
    let ret = func();
    println!("returned value:{}", ret);
}
