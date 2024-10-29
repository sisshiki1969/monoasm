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
            movq rax, (-7777777);
            movq r12, (1111111);
            cdq;
            idivl r12; // eax = -7, edx = 0
            ret;
    );
    jit.finalize();

    let f: ReturnFunc = unsafe { std::mem::transmute(jit.get_label_u64(begin)) };
    let ret = f() as i32; // rax contains (-7)
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
            movq rax, (-23);
            movq r12, (4);
            cdq;
            idivl r12; // eax = -5, edx = -3
            movl rax, rdx;
            ret;
    );
    jit.finalize();

    let f: ReturnFunc = unsafe { std::mem::transmute(jit.get_label_u64(begin)) };
    let ret = f() as i32; // rax contains (-3)
    assert_eq!(ret, -3);
}
