use monoasm::*;
use monoasm_macro::*;

#[test]
fn test() {
    let mut jit = JitMemory::new();
    monoasm! { &mut jit,
      testb rax, rax;
      testb rax, 1;
      testb rax, rdi;
      testb rdi, 7;
      testb [r14 + rax * 2 + 16], rax;
      testb [r15 + r15 * 8 + 20], r15;
      ret;
    }
    jit.finalize();
    eprintln!("{}", jit.dump_code().unwrap());
}
