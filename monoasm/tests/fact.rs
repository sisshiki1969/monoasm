extern crate monoasm;
extern crate monoasm_macro;
use monoasm::*;
use monoasm_macro::monoasm;

#[test]
fn factorial() {
    let func = fac();
    let ret = func(10);
    assert_eq!(3628800, ret);
}

fn fac() -> fn(u64) -> u64 {
    let fmt = "%d\n\0";
    let mut jit: JitMemory = JitMemory::new();
    let printf_addr = libc::printf as u64;
    let fac = jit.label();
    let l2 = jit.label();
    let l3 = jit.label();
    let i: i32 = 8;

    // main()
    monoasm!(jit,
        // prologue
        pushq rbp;
        movq rbp, rsp;

        //movq rdi, 10;
        // fac(rdi) -> rax
        call fac;

        movq r15, rax;
        movq rsi, rax;
        movq rdi, (fmt.as_ptr() as u64);
        movq rax, 0;
        movq rcx, (printf_addr);
        // printf(fmt, rsi)
        call rcx;
        // epilogue
        popq rbp;
        movq rax, r15;
        ret;

    // fac(arg:i64) -> rax
    fac:
        // prologue
        pushq rbp;
        movq rbp, rsp;
        // local variables
        // 0:arg
        // 8:
        subq rsp, 16;

        movq [rbp-(i)], rdi;
        cmpq [rbp-(i)], 1;
        // if arg != 1 then goto l2
        jne l2;
        // else return 1
        movq rax, 1;
        jmp l3;
    l2:
        // fac(arg - 1) * [rbp - 8]->
        movq rdi, [rbp-(i)];
        subq rdi, 1;
        call fac;
        imul rax, [rbp-(i)];
    l3:
        // epilogue
        movq rsp, rbp;
        popq rbp;
        ret;
    );

    jit.finalize()
}
