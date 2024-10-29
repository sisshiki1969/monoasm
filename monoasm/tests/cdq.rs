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
            xorl rdx, rdx;
            xorl rax, rax;
            movq rax, (0x80000000);
            cdq;
            movl rax, rdx;
            ret;
    );
    jit.finalize();

    let f: ReturnFunc = unsafe { std::mem::transmute(jit.get_label_u64(begin)) };
    let ret = f() as u64; // rax contains 0x00000000FFFFFFFF
    assert_eq!(ret, 0x00000000FFFFFFFF);
}
