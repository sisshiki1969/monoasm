extern crate monoasm;
extern crate monoasm_macro;

use monoasm::*;
use monoasm_macro::monoasm;
use std::ops::Neg;

#[test]
fn xorpd() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    let pi = 3.141592653589793f64;
    let mask = 0x8000000000000000u64; // mask for the negation
    monoasm!(&mut jit,
        begin:
            movq rax, (mask);
            movq xmm1, rax;

            xorpd xmm0, xmm1;
            ret;
            movq rax, (foo);
    );
    jit.finalize();

    let f = jit.get_label_addr::<f64, f64>(&begin);
    let ret = f(pi);
    assert_eq!(ret, pi.neg());
}

extern "C" fn foo() {}
