extern crate monoasm;
extern crate monoasm_macro;

use monoasm::*;
use monoasm_macro::monoasm;

#[test]
fn roundpd() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    let pi = 3.141592653589793f64;
    monoasm!(&mut jit,
        begin:
            roundpd xmm0, xmm0, (0x02); // 0x02 -> ceil
            ret;
    );
    jit.finalize();

    let f = jit.get_label_addr::<f64, f64>(&begin);
    let ret = f(pi);
    assert_eq!(ret, pi.ceil());
}
