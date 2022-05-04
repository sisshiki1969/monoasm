  extern crate monoasm;
  extern crate monoasm_macro;
  use std::io::Write;

  use monoasm::*;
  use monoasm_macro::monoasm;

  #[test]
  fn pushq() {
      let mut jit: JitMemory = JitMemory::new();
      monoasm!(
          jit,
	pushq rax;
	pushq rcx;
	pushq rdx;
	pushq rbx;
	pushq rsp;
	pushq rbp;
	pushq rsi;
	pushq rdi;
	pushq r8;
	pushq r9;
	pushq r10;
	pushq r11;
	pushq r12;
	pushq r13;
	pushq r14;
	pushq r15;
	pushq [rax];
	pushq [rax + 16];
	pushq [rax + 512];
	pushq [rcx];
	pushq [rcx + 16];
	pushq [rcx + 512];
	pushq [rdx];
	pushq [rdx + 16];
	pushq [rdx + 512];
	pushq [rbx];
	pushq [rbx + 16];
	pushq [rbx + 512];
	pushq [rsp];
	pushq [rsp + 16];
	pushq [rsp + 512];
	pushq [rbp];
	pushq [rbp + 16];
	pushq [rbp + 512];
	pushq [rsi];
	pushq [rsi + 16];
	pushq [rsi + 512];
	pushq [rdi];
	pushq [rdi + 16];
	pushq [rdi + 512];
	pushq [r8];
	pushq [r8 + 16];
	pushq [r8 + 512];
	pushq [r9];
	pushq [r9 + 16];
	pushq [r9 + 512];
	pushq [r10];
	pushq [r10 + 16];
	pushq [r10 + 512];
	pushq [r11];
	pushq [r11 + 16];
	pushq [r11 + 512];
	pushq [r12];
	pushq [r12 + 16];
	pushq [r12 + 512];
	pushq [r13];
	pushq [r13 + 16];
	pushq [r13 + 512];
	pushq [r14];
	pushq [r14 + 16];
	pushq [r14 + 512];
	pushq [r15];
	pushq [r15 + 16];
	pushq [r15 + 512];
	pushq [rip];
	pushq [rip + 16];
	pushq [rip + 512];
      );
      jit.finalize();
      let mut buf = std::fs::File::create("tests/pushq_monoasm.bin").unwrap();
      buf.write_all(jit.as_slice()).unwrap();
  }
