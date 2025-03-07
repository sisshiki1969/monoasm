extern crate monoasm;
extern crate monoasm_macro;

use monoasm::*;
use monoasm_macro::monoasm;
pub type ReturnFunc = extern "C" fn() -> u64;

#[test]
fn cdq() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    monoasm!(&mut jit,
        begin:
            movq rax, rdi;
            cdq;
            movl rax, rdx;
            ret;
    );
    jit.finalize();

    let f = jit.get_label_addr::<u64, u64>(&begin);
    let ret = f(0x80000000); // rax contains 0x00000000FFFFFFFF
    assert_eq!(ret, 0x00000000FFFFFFFF);
}

#[test]
fn notq() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    monoasm!(&mut jit,
        begin:
            notq rdi;
            movq rax, rdi;
            ret;
    );
    jit.finalize();

    let f = jit.get_label_addr::<i64, i64>(&begin);
    assert_eq!(f(354), !354);
    assert_eq!(f(-354), !(-354));
}
