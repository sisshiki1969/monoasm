# monoasm

monoasm is a x86-64 run-time code generator engine written in Rust.

This tool generates x86-64 machine codes on memory and you can exexute them as a Rust function.
You can write x64 assembly in your Rust code using intel-like syntax with the aid of procedual macro, which is a powerful feature of Rust 2018.


```Rust
#![feature(proc_macro_hygiene)]
extern crate monoasm; // runtime lib crate.
extern crate monoasm_macro; // procedual macro lib crate
use monoasm::JitMemory;
use monoasm_macro::monoasm;

fn fac() -> (fn() -> i64) {
    let fmt = "%d\n\0";
    let mut jit: JitMemory = JitMemory::new();
    let printf_addr = libc::printf as u64;
    let fac = jit.label(); // Currently, labels must be declared in advance of using them.
    let l2 = jit.label();
    let l3 = jit.label();

    // main()
    monoasm!( // macro
        pushq rbp;  // ';' is neccesary at the end of lines.
        movq rbp, rsp;
        movq rdi, 10;
        call fac;

        movq rsi, rax;
        movq rdi, (fmt.as_ptr() as u64); // In macro, Rust expression needs ().
        movq rcx, (printf_addr);
        movq rax, 0;
        call rcx;
        popq rbp;
        ret;

    fac: // label
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

    jit.finalize() // finalize() returns function pointer(fn() -> i64).
}

fn main() {
    let func = fac();  // fac() returns function pointer.
    let ret = func();
    println!("returned value:{}", ret);
}
```
