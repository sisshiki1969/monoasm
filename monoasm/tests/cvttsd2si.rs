extern crate monoasm;
extern crate monoasm_macro;

use monoasm::*;
use monoasm_macro::monoasm;
pub type ReturnFunc = extern "C" fn() -> u64;

#[test]
fn cvttsd2si() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    let f64_repr = 3.141592653589793f64.to_bits();
    monoasm!(&mut jit,
        begin:
            movq rax, (f64_repr);
            movq xmm0, rax; // move the bit representation to xmm0
            cvttsd2siq rax, xmm0;
            ret;
    );
    jit.finalize();

    let f: ReturnFunc = unsafe { std::mem::transmute(jit.get_label_u64(begin)) };
    let ret = f() as i64;
    assert_eq!(ret, 3);
}

