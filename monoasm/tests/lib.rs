use monoasm::*;
use monoasm_macro::*;

#[test]
fn test() {
    let mut jit = JitMemory::new();
    let label = jit.label();
    monoasm! { &mut jit,
    label:
      int3;
      ret;
    }
    jit.finalize();
    eprintln!("{}", jit.dump_code().unwrap());
    let f = jit.get_label_addr::<(), ()>(label);
    f(());
}
