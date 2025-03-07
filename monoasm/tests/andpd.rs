extern crate monoasm;
extern crate monoasm_macro;

use monoasm::*;
use monoasm_macro::monoasm;

#[test]
fn andpd() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    let pi = -3.141592653589793;
    let mask = 0x7fffffffffffffffu64; // mask for the sign bit
    monoasm!(&mut jit,
        begin:
            movq rax, (mask);
            movq xmm1, rax;

            andpd xmm0, xmm1;
            ret;
    );
    jit.finalize();

    let f = jit.get_label_addr::<f64, f64>(&begin);
    let ret = f(pi);
    assert_eq!(ret, pi.abs());
}
