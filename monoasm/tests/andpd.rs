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

#[test]
fn andpd2() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    let pi = jit.const_f64(-3.141592653589793);
    let mask = 0x7fffffffffffffffu64; // mask for the sign bit
    monoasm! {&mut jit,
        begin:
    }
    for _ in 0..10 {
        monoasm! {&mut jit,
            movq xmm0, [rip + pi];
        }
    }
    monoasm! {&mut jit,
            movq rax, (mask);
            movq xmm1, rax;
            andpd xmm0, xmm1;
            ret;
    }
    for _ in 0..10 {
        monoasm! {&mut jit,
            jmp begin;
            call begin;
        }
    }
    jit.finalize();

    let f = jit.get_label_addr::<(), f64>(&begin);
    let ret = f(());
    assert_eq!(ret, 3.141592653589793_f64.abs());
}
