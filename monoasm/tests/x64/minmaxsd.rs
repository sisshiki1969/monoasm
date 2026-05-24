use monoasm::*;
use monoasm_macro::monoasm;

#[test]
fn minmaxsd() {
    let mut jit = JitMemory::new();
    let label0 = jit.label();
    let label1 = jit.label();
    let data = jit.data(64);
    monoasm! { &mut jit,
    label0:
      lea  rsi, [rip + data];
      movq rcx, 8;
      movq [rsi + rcx * 2 + 4], xmm1;
      minsd xmm0, [rsi + rcx * 2 + 4];
      ret;
    label1:
      lea  rsi, [rip + data];
      movq rcx, 8;
      movq [rsi + rcx * 2 + 4], xmm1;
      maxsd xmm0, [rsi + rcx * 2 + 4];
      ret;
    }
    jit.finalize();
    eprintln!("{}", jit.dump_code().unwrap());

    let f1 = jit.get_label_addr2::<f64, f64, f64>(&label0);
    let f2 = jit.get_label_addr2::<f64, f64, f64>(&label1);
    assert_eq!(-0.1, dbg!(f1(0.02, -0.1)));
    assert_eq!(0.02, dbg!(f2(0.02, -0.1)));
}
