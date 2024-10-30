extern crate monoasm;
extern crate monoasm_macro;

use monoasm::*;
use monoasm_macro::monoasm;
pub type ReturnFunc = extern "C" fn() -> u64;

#[test]
fn cvttsd2si() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    let pi = 3.141592653589793f64;
    monoasm!(&mut jit,
        begin:
            cvttsd2siq rax, xmm0;
            ret;
    );
    jit.finalize();

    let f = jit.get_label_addr::<f64, i64>(begin);
    let ret = f(pi);
    assert_eq!(ret, 3);
}

