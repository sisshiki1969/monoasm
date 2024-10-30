extern crate monoasm;
extern crate monoasm_macro;

use monoasm::*;
use monoasm_macro::monoasm;
pub type ReturnFunc = extern "C" fn() -> u64;

#[test]
fn idivl() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    monoasm!(&mut jit,
        begin:
            xorl rdx, rdx;
            xorl rax, rax;
            movq rax, rdi;
            movq r12, rsi;
            cdq;
            idivl r12; // eax = -7, edx = 0
            ret;
    );
    jit.finalize();

    let f = jit.get_label_addr2::<i32, i32, i32>(begin);
    let ret = f(-7777777, 1111111); // rax contains (-7)
    assert_eq!(ret, -7);
}

#[test]
fn idivl_rem() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    monoasm!(&mut jit,
        begin:
            xorl rdx, rdx;
            xorl rax, rax;
            movq rax, rdi;
            movq r12, rsi;
            cdq;
            idivl r12; // eax = -5, edx = -3
            movl rax, rdx;
            ret;
    );
    jit.finalize();

    let f = jit.get_label_addr2::<i32, i32, i32>(begin);
    let ret = f(-23, 4); // rax contains (-3)
    assert_eq!(ret, -3);
}
