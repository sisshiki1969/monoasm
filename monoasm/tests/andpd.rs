extern crate monoasm;
extern crate monoasm_macro;

use monoasm::*;
use monoasm_macro::monoasm;
pub type ReturnFunc = extern "C" fn() -> u64;

#[test]
fn andpd() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    let f64_repr = (-3.141592653589793f64).to_bits();
    let mask = 0x7fffffffffffffffu64; // mask for the sign bit
    monoasm!(&mut jit,
        begin:
            xorq rax, rax;

            movq rax, (f64_repr);
            movq xmm0, rax;
            
            movq rax, (mask);
            movq xmm1, rax;

            andpd xmm0, xmm1;
            movq rax, xmm0;
            ret;
    );
    jit.finalize();

    let f: ReturnFunc = unsafe { std::mem::transmute(jit.get_label_u64(begin)) };
    let ret = f64::from_bits(f() as u64);
    assert_eq!(ret, f64::from_bits(f64_repr).abs());
}