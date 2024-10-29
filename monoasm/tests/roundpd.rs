extern crate monoasm;
extern crate monoasm_macro;

use monoasm::*;
use monoasm_macro::monoasm;
pub type ReturnFunc = extern "C" fn() -> u64;

#[test]
fn roundpd() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    let f64_repr = (3.141592653589793f64).to_bits();
    monoasm!(&mut jit,
        begin:
            xorq rax, rax;

            movq rax, (f64_repr);
            movq xmm0, rax;
            
            roundpd xmm0, xmm0, (0x02); // 0x02 -> ceil
            movq rax, xmm0;
            ret;
    );
    jit.finalize();

    let f: ReturnFunc = unsafe { std::mem::transmute(jit.get_label_u64(begin)) };
    let ret = f64::from_bits(f() as u64);
    assert_eq!(ret, f64::from_bits(f64_repr).ceil());
}