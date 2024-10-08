  extern crate monoasm;
  extern crate monoasm_macro;
  use std::io::Write;

  use monoasm::*;
  use monoasm_macro::monoasm;

  #[test]
  fn shrq() {
      let mut jit: JitMemory = JitMemory::new();
      monoasm!{
          &mut jit,
	shrq rax, 1;
	shrq rax, 18;
	shrq rcx, 1;
	shrq rcx, 18;
	shrq rdx, 1;
	shrq rdx, 18;
	shrq rbx, 1;
	shrq rbx, 18;
	shrq rsp, 1;
	shrq rsp, 18;
	shrq rbp, 1;
	shrq rbp, 18;
	shrq rsi, 1;
	shrq rsi, 18;
	shrq rdi, 1;
	shrq rdi, 18;
	shrq r8, 1;
	shrq r8, 18;
	shrq r9, 1;
	shrq r9, 18;
	shrq r10, 1;
	shrq r10, 18;
	shrq r11, 1;
	shrq r11, 18;
	shrq r12, 1;
	shrq r12, 18;
	shrq r13, 1;
	shrq r13, 18;
	shrq r14, 1;
	shrq r14, 18;
	shrq r15, 1;
	shrq r15, 18;
	shrq [rax], 1;
	shrq [rax], 18;
	shrq [rax + 16], 1;
	shrq [rax + 16], 18;
	shrq [rax + 512], 1;
	shrq [rax + 512], 18;
	shrq [rcx], 1;
	shrq [rcx], 18;
	shrq [rcx + 16], 1;
	shrq [rcx + 16], 18;
	shrq [rcx + 512], 1;
	shrq [rcx + 512], 18;
	shrq [rdx], 1;
	shrq [rdx], 18;
	shrq [rdx + 16], 1;
	shrq [rdx + 16], 18;
	shrq [rdx + 512], 1;
	shrq [rdx + 512], 18;
	shrq [rbx], 1;
	shrq [rbx], 18;
	shrq [rbx + 16], 1;
	shrq [rbx + 16], 18;
	shrq [rbx + 512], 1;
	shrq [rbx + 512], 18;
	shrq [rsp], 1;
	shrq [rsp], 18;
	shrq [rsp + 16], 1;
	shrq [rsp + 16], 18;
	shrq [rsp + 512], 1;
	shrq [rsp + 512], 18;
	shrq [rbp], 1;
	shrq [rbp], 18;
	shrq [rbp + 16], 1;
	shrq [rbp + 16], 18;
	shrq [rbp + 512], 1;
	shrq [rbp + 512], 18;
	shrq [rsi], 1;
	shrq [rsi], 18;
	shrq [rsi + 16], 1;
	shrq [rsi + 16], 18;
	shrq [rsi + 512], 1;
	shrq [rsi + 512], 18;
	shrq [rdi], 1;
	shrq [rdi], 18;
	shrq [rdi + 16], 1;
	shrq [rdi + 16], 18;
	shrq [rdi + 512], 1;
	shrq [rdi + 512], 18;
	shrq [r8], 1;
	shrq [r8], 18;
	shrq [r8 + 16], 1;
	shrq [r8 + 16], 18;
	shrq [r8 + 512], 1;
	shrq [r8 + 512], 18;
	shrq [r9], 1;
	shrq [r9], 18;
	shrq [r9 + 16], 1;
	shrq [r9 + 16], 18;
	shrq [r9 + 512], 1;
	shrq [r9 + 512], 18;
	shrq [r10], 1;
	shrq [r10], 18;
	shrq [r10 + 16], 1;
	shrq [r10 + 16], 18;
	shrq [r10 + 512], 1;
	shrq [r10 + 512], 18;
	shrq [r11], 1;
	shrq [r11], 18;
	shrq [r11 + 16], 1;
	shrq [r11 + 16], 18;
	shrq [r11 + 512], 1;
	shrq [r11 + 512], 18;
	shrq [r12], 1;
	shrq [r12], 18;
	shrq [r12 + 16], 1;
	shrq [r12 + 16], 18;
	shrq [r12 + 512], 1;
	shrq [r12 + 512], 18;
	shrq [r13], 1;
	shrq [r13], 18;
	shrq [r13 + 16], 1;
	shrq [r13 + 16], 18;
	shrq [r13 + 512], 1;
	shrq [r13 + 512], 18;
	shrq [r14], 1;
	shrq [r14], 18;
	shrq [r14 + 16], 1;
	shrq [r14 + 16], 18;
	shrq [r14 + 512], 1;
	shrq [r14 + 512], 18;
	shrq [r15], 1;
	shrq [r15], 18;
	shrq [r15 + 16], 1;
	shrq [r15 + 16], 18;
	shrq [r15 + 512], 1;
	shrq [r15 + 512], 18;
	shrq [rip], 1;
	shrq [rip], 18;
	shrq [rip + 16], 1;
	shrq [rip + 16], 18;
	shrq [rip + 512], 1;
	shrq [rip + 512], 18;
	shrq [rax + rax * 1], 1;
	shrq [rax + rax * 1], 18;
	shrq [rax + rax * 1 + 20], 1;
	shrq [rax + rax * 1 + 20], 18;
	shrq [rax + rax * 8], 1;
	shrq [rax + rax * 8], 18;
	shrq [rax + rax * 8 + 20], 1;
	shrq [rax + rax * 8 + 20], 18;
	shrq [rax + r15 * 1], 1;
	shrq [rax + r15 * 1], 18;
	shrq [rax + r15 * 1 + 20], 1;
	shrq [rax + r15 * 1 + 20], 18;
	shrq [rax + r15 * 8], 1;
	shrq [rax + r15 * 8], 18;
	shrq [rax + r15 * 8 + 20], 1;
	shrq [rax + r15 * 8 + 20], 18;
	shrq [rcx + rax * 1], 1;
	shrq [rcx + rax * 1], 18;
	shrq [rcx + rax * 1 + 20], 1;
	shrq [rcx + rax * 1 + 20], 18;
	shrq [rcx + rax * 8], 1;
	shrq [rcx + rax * 8], 18;
	shrq [rcx + rax * 8 + 20], 1;
	shrq [rcx + rax * 8 + 20], 18;
	shrq [rcx + r15 * 1], 1;
	shrq [rcx + r15 * 1], 18;
	shrq [rcx + r15 * 1 + 20], 1;
	shrq [rcx + r15 * 1 + 20], 18;
	shrq [rcx + r15 * 8], 1;
	shrq [rcx + r15 * 8], 18;
	shrq [rcx + r15 * 8 + 20], 1;
	shrq [rcx + r15 * 8 + 20], 18;
	shrq [rdx + rax * 1], 1;
	shrq [rdx + rax * 1], 18;
	shrq [rdx + rax * 1 + 20], 1;
	shrq [rdx + rax * 1 + 20], 18;
	shrq [rdx + rax * 8], 1;
	shrq [rdx + rax * 8], 18;
	shrq [rdx + rax * 8 + 20], 1;
	shrq [rdx + rax * 8 + 20], 18;
	shrq [rdx + r15 * 1], 1;
	shrq [rdx + r15 * 1], 18;
	shrq [rdx + r15 * 1 + 20], 1;
	shrq [rdx + r15 * 1 + 20], 18;
	shrq [rdx + r15 * 8], 1;
	shrq [rdx + r15 * 8], 18;
	shrq [rdx + r15 * 8 + 20], 1;
	shrq [rdx + r15 * 8 + 20], 18;
	shrq [rbx + rax * 1], 1;
	shrq [rbx + rax * 1], 18;
	shrq [rbx + rax * 1 + 20], 1;
	shrq [rbx + rax * 1 + 20], 18;
	shrq [rbx + rax * 8], 1;
	shrq [rbx + rax * 8], 18;
	shrq [rbx + rax * 8 + 20], 1;
	shrq [rbx + rax * 8 + 20], 18;
	shrq [rbx + r15 * 1], 1;
	shrq [rbx + r15 * 1], 18;
	shrq [rbx + r15 * 1 + 20], 1;
	shrq [rbx + r15 * 1 + 20], 18;
	shrq [rbx + r15 * 8], 1;
	shrq [rbx + r15 * 8], 18;
	shrq [rbx + r15 * 8 + 20], 1;
	shrq [rbx + r15 * 8 + 20], 18;
	shrq [rsp + rax * 1], 1;
	shrq [rsp + rax * 1], 18;
	shrq [rsp + rax * 1 + 20], 1;
	shrq [rsp + rax * 1 + 20], 18;
	shrq [rsp + rax * 8], 1;
	shrq [rsp + rax * 8], 18;
	shrq [rsp + rax * 8 + 20], 1;
	shrq [rsp + rax * 8 + 20], 18;
	shrq [rsp + r15 * 1], 1;
	shrq [rsp + r15 * 1], 18;
	shrq [rsp + r15 * 1 + 20], 1;
	shrq [rsp + r15 * 1 + 20], 18;
	shrq [rsp + r15 * 8], 1;
	shrq [rsp + r15 * 8], 18;
	shrq [rsp + r15 * 8 + 20], 1;
	shrq [rsp + r15 * 8 + 20], 18;
	shrq [rbp + rax * 1], 1;
	shrq [rbp + rax * 1], 18;
	shrq [rbp + rax * 1 + 20], 1;
	shrq [rbp + rax * 1 + 20], 18;
	shrq [rbp + rax * 8], 1;
	shrq [rbp + rax * 8], 18;
	shrq [rbp + rax * 8 + 20], 1;
	shrq [rbp + rax * 8 + 20], 18;
	shrq [rbp + r15 * 1], 1;
	shrq [rbp + r15 * 1], 18;
	shrq [rbp + r15 * 1 + 20], 1;
	shrq [rbp + r15 * 1 + 20], 18;
	shrq [rbp + r15 * 8], 1;
	shrq [rbp + r15 * 8], 18;
	shrq [rbp + r15 * 8 + 20], 1;
	shrq [rbp + r15 * 8 + 20], 18;
	shrq [rsi + rax * 1], 1;
	shrq [rsi + rax * 1], 18;
	shrq [rsi + rax * 1 + 20], 1;
	shrq [rsi + rax * 1 + 20], 18;
	shrq [rsi + rax * 8], 1;
	shrq [rsi + rax * 8], 18;
	shrq [rsi + rax * 8 + 20], 1;
	shrq [rsi + rax * 8 + 20], 18;
	shrq [rsi + r15 * 1], 1;
	shrq [rsi + r15 * 1], 18;
	shrq [rsi + r15 * 1 + 20], 1;
	shrq [rsi + r15 * 1 + 20], 18;
	shrq [rsi + r15 * 8], 1;
	shrq [rsi + r15 * 8], 18;
	shrq [rsi + r15 * 8 + 20], 1;
	shrq [rsi + r15 * 8 + 20], 18;
	shrq [rdi + rax * 1], 1;
	shrq [rdi + rax * 1], 18;
	shrq [rdi + rax * 1 + 20], 1;
	shrq [rdi + rax * 1 + 20], 18;
	shrq [rdi + rax * 8], 1;
	shrq [rdi + rax * 8], 18;
	shrq [rdi + rax * 8 + 20], 1;
	shrq [rdi + rax * 8 + 20], 18;
	shrq [rdi + r15 * 1], 1;
	shrq [rdi + r15 * 1], 18;
	shrq [rdi + r15 * 1 + 20], 1;
	shrq [rdi + r15 * 1 + 20], 18;
	shrq [rdi + r15 * 8], 1;
	shrq [rdi + r15 * 8], 18;
	shrq [rdi + r15 * 8 + 20], 1;
	shrq [rdi + r15 * 8 + 20], 18;
	shrq [r8 + rax * 1], 1;
	shrq [r8 + rax * 1], 18;
	shrq [r8 + rax * 1 + 20], 1;
	shrq [r8 + rax * 1 + 20], 18;
	shrq [r8 + rax * 8], 1;
	shrq [r8 + rax * 8], 18;
	shrq [r8 + rax * 8 + 20], 1;
	shrq [r8 + rax * 8 + 20], 18;
	shrq [r8 + r15 * 1], 1;
	shrq [r8 + r15 * 1], 18;
	shrq [r8 + r15 * 1 + 20], 1;
	shrq [r8 + r15 * 1 + 20], 18;
	shrq [r8 + r15 * 8], 1;
	shrq [r8 + r15 * 8], 18;
	shrq [r8 + r15 * 8 + 20], 1;
	shrq [r8 + r15 * 8 + 20], 18;
	shrq [r9 + rax * 1], 1;
	shrq [r9 + rax * 1], 18;
	shrq [r9 + rax * 1 + 20], 1;
	shrq [r9 + rax * 1 + 20], 18;
	shrq [r9 + rax * 8], 1;
	shrq [r9 + rax * 8], 18;
	shrq [r9 + rax * 8 + 20], 1;
	shrq [r9 + rax * 8 + 20], 18;
	shrq [r9 + r15 * 1], 1;
	shrq [r9 + r15 * 1], 18;
	shrq [r9 + r15 * 1 + 20], 1;
	shrq [r9 + r15 * 1 + 20], 18;
	shrq [r9 + r15 * 8], 1;
	shrq [r9 + r15 * 8], 18;
	shrq [r9 + r15 * 8 + 20], 1;
	shrq [r9 + r15 * 8 + 20], 18;
	shrq [r10 + rax * 1], 1;
	shrq [r10 + rax * 1], 18;
	shrq [r10 + rax * 1 + 20], 1;
	shrq [r10 + rax * 1 + 20], 18;
	shrq [r10 + rax * 8], 1;
	shrq [r10 + rax * 8], 18;
	shrq [r10 + rax * 8 + 20], 1;
	shrq [r10 + rax * 8 + 20], 18;
	shrq [r10 + r15 * 1], 1;
	shrq [r10 + r15 * 1], 18;
	shrq [r10 + r15 * 1 + 20], 1;
	shrq [r10 + r15 * 1 + 20], 18;
	shrq [r10 + r15 * 8], 1;
	shrq [r10 + r15 * 8], 18;
	shrq [r10 + r15 * 8 + 20], 1;
	shrq [r10 + r15 * 8 + 20], 18;
	shrq [r11 + rax * 1], 1;
	shrq [r11 + rax * 1], 18;
	shrq [r11 + rax * 1 + 20], 1;
	shrq [r11 + rax * 1 + 20], 18;
	shrq [r11 + rax * 8], 1;
	shrq [r11 + rax * 8], 18;
	shrq [r11 + rax * 8 + 20], 1;
	shrq [r11 + rax * 8 + 20], 18;
	shrq [r11 + r15 * 1], 1;
	shrq [r11 + r15 * 1], 18;
	shrq [r11 + r15 * 1 + 20], 1;
	shrq [r11 + r15 * 1 + 20], 18;
	shrq [r11 + r15 * 8], 1;
	shrq [r11 + r15 * 8], 18;
	shrq [r11 + r15 * 8 + 20], 1;
	shrq [r11 + r15 * 8 + 20], 18;
	shrq [r12 + rax * 1], 1;
	shrq [r12 + rax * 1], 18;
	shrq [r12 + rax * 1 + 20], 1;
	shrq [r12 + rax * 1 + 20], 18;
	shrq [r12 + rax * 8], 1;
	shrq [r12 + rax * 8], 18;
	shrq [r12 + rax * 8 + 20], 1;
	shrq [r12 + rax * 8 + 20], 18;
	shrq [r12 + r15 * 1], 1;
	shrq [r12 + r15 * 1], 18;
	shrq [r12 + r15 * 1 + 20], 1;
	shrq [r12 + r15 * 1 + 20], 18;
	shrq [r12 + r15 * 8], 1;
	shrq [r12 + r15 * 8], 18;
	shrq [r12 + r15 * 8 + 20], 1;
	shrq [r12 + r15 * 8 + 20], 18;
	shrq [r13 + rax * 1], 1;
	shrq [r13 + rax * 1], 18;
	shrq [r13 + rax * 1 + 20], 1;
	shrq [r13 + rax * 1 + 20], 18;
	shrq [r13 + rax * 8], 1;
	shrq [r13 + rax * 8], 18;
	shrq [r13 + rax * 8 + 20], 1;
	shrq [r13 + rax * 8 + 20], 18;
	shrq [r13 + r15 * 1], 1;
	shrq [r13 + r15 * 1], 18;
	shrq [r13 + r15 * 1 + 20], 1;
	shrq [r13 + r15 * 1 + 20], 18;
	shrq [r13 + r15 * 8], 1;
	shrq [r13 + r15 * 8], 18;
	shrq [r13 + r15 * 8 + 20], 1;
	shrq [r13 + r15 * 8 + 20], 18;
	shrq [r14 + rax * 1], 1;
	shrq [r14 + rax * 1], 18;
	shrq [r14 + rax * 1 + 20], 1;
	shrq [r14 + rax * 1 + 20], 18;
	shrq [r14 + rax * 8], 1;
	shrq [r14 + rax * 8], 18;
	shrq [r14 + rax * 8 + 20], 1;
	shrq [r14 + rax * 8 + 20], 18;
	shrq [r14 + r15 * 1], 1;
	shrq [r14 + r15 * 1], 18;
	shrq [r14 + r15 * 1 + 20], 1;
	shrq [r14 + r15 * 1 + 20], 18;
	shrq [r14 + r15 * 8], 1;
	shrq [r14 + r15 * 8], 18;
	shrq [r14 + r15 * 8 + 20], 1;
	shrq [r14 + r15 * 8 + 20], 18;
	shrq [r15 + rax * 1], 1;
	shrq [r15 + rax * 1], 18;
	shrq [r15 + rax * 1 + 20], 1;
	shrq [r15 + rax * 1 + 20], 18;
	shrq [r15 + rax * 8], 1;
	shrq [r15 + rax * 8], 18;
	shrq [r15 + rax * 8 + 20], 1;
	shrq [r15 + rax * 8 + 20], 18;
	shrq [r15 + r15 * 1], 1;
	shrq [r15 + r15 * 1], 18;
	shrq [r15 + r15 * 1 + 20], 1;
	shrq [r15 + r15 * 1 + 20], 18;
	shrq [r15 + r15 * 8], 1;
	shrq [r15 + r15 * 8], 18;
	shrq [r15 + r15 * 8 + 20], 1;
	shrq [r15 + r15 * 8 + 20], 18;
	shrq rax, 1;
	shrq rax, 18;
	shrq rcx, 1;
	shrq rcx, 18;
	shrq rdx, 1;
	shrq rdx, 18;
	shrq rbx, 1;
	shrq rbx, 18;
	shrq rsp, 1;
	shrq rsp, 18;
	shrq rbp, 1;
	shrq rbp, 18;
	shrq rsi, 1;
	shrq rsi, 18;
	shrq rdi, 1;
	shrq rdi, 18;
	shrq r8, 1;
	shrq r8, 18;
	shrq r9, 1;
	shrq r9, 18;
	shrq r10, 1;
	shrq r10, 18;
	shrq r11, 1;
	shrq r11, 18;
	shrq r12, 1;
	shrq r12, 18;
	shrq r13, 1;
	shrq r13, 18;
	shrq r14, 1;
	shrq r14, 18;
	shrq r15, 1;
	shrq r15, 18;
	shrq [rax], 1;
	shrq [rax], 18;
	shrq [rax + 16], 1;
	shrq [rax + 16], 18;
	shrq [rax + 512], 1;
	shrq [rax + 512], 18;
	shrq [rcx], 1;
	shrq [rcx], 18;
	shrq [rcx + 16], 1;
	shrq [rcx + 16], 18;
	shrq [rcx + 512], 1;
	shrq [rcx + 512], 18;
	shrq [rdx], 1;
	shrq [rdx], 18;
	shrq [rdx + 16], 1;
	shrq [rdx + 16], 18;
	shrq [rdx + 512], 1;
	shrq [rdx + 512], 18;
	shrq [rbx], 1;
	shrq [rbx], 18;
	shrq [rbx + 16], 1;
	shrq [rbx + 16], 18;
	shrq [rbx + 512], 1;
	shrq [rbx + 512], 18;
	shrq [rsp], 1;
	shrq [rsp], 18;
	shrq [rsp + 16], 1;
	shrq [rsp + 16], 18;
	shrq [rsp + 512], 1;
	shrq [rsp + 512], 18;
	shrq [rbp], 1;
	shrq [rbp], 18;
	shrq [rbp + 16], 1;
	shrq [rbp + 16], 18;
	shrq [rbp + 512], 1;
	shrq [rbp + 512], 18;
	shrq [rsi], 1;
	shrq [rsi], 18;
	shrq [rsi + 16], 1;
	shrq [rsi + 16], 18;
	shrq [rsi + 512], 1;
	shrq [rsi + 512], 18;
	shrq [rdi], 1;
	shrq [rdi], 18;
	shrq [rdi + 16], 1;
	shrq [rdi + 16], 18;
	shrq [rdi + 512], 1;
	shrq [rdi + 512], 18;
	shrq [r8], 1;
	shrq [r8], 18;
	shrq [r8 + 16], 1;
	shrq [r8 + 16], 18;
	shrq [r8 + 512], 1;
	shrq [r8 + 512], 18;
	shrq [r9], 1;
	shrq [r9], 18;
	shrq [r9 + 16], 1;
	shrq [r9 + 16], 18;
	shrq [r9 + 512], 1;
	shrq [r9 + 512], 18;
	shrq [r10], 1;
	shrq [r10], 18;
	shrq [r10 + 16], 1;
	shrq [r10 + 16], 18;
	shrq [r10 + 512], 1;
	shrq [r10 + 512], 18;
	shrq [r11], 1;
	shrq [r11], 18;
	shrq [r11 + 16], 1;
	shrq [r11 + 16], 18;
	shrq [r11 + 512], 1;
	shrq [r11 + 512], 18;
	shrq [r12], 1;
	shrq [r12], 18;
	shrq [r12 + 16], 1;
	shrq [r12 + 16], 18;
	shrq [r12 + 512], 1;
	shrq [r12 + 512], 18;
	shrq [r13], 1;
	shrq [r13], 18;
	shrq [r13 + 16], 1;
	shrq [r13 + 16], 18;
	shrq [r13 + 512], 1;
	shrq [r13 + 512], 18;
	shrq [r14], 1;
	shrq [r14], 18;
	shrq [r14 + 16], 1;
	shrq [r14 + 16], 18;
	shrq [r14 + 512], 1;
	shrq [r14 + 512], 18;
	shrq [r15], 1;
	shrq [r15], 18;
	shrq [r15 + 16], 1;
	shrq [r15 + 16], 18;
	shrq [r15 + 512], 1;
	shrq [r15 + 512], 18;
	shrq [rip], 1;
	shrq [rip], 18;
	shrq [rip + 16], 1;
	shrq [rip + 16], 18;
	shrq [rip + 512], 1;
	shrq [rip + 512], 18;
	shrq [rax + rax * 1], 1;
	shrq [rax + rax * 1], 18;
	shrq [rax + rax * 1 + 20], 1;
	shrq [rax + rax * 1 + 20], 18;
	shrq [rax + rax * 8], 1;
	shrq [rax + rax * 8], 18;
	shrq [rax + rax * 8 + 20], 1;
	shrq [rax + rax * 8 + 20], 18;
	shrq [rax + r15 * 1], 1;
	shrq [rax + r15 * 1], 18;
	shrq [rax + r15 * 1 + 20], 1;
	shrq [rax + r15 * 1 + 20], 18;
	shrq [rax + r15 * 8], 1;
	shrq [rax + r15 * 8], 18;
	shrq [rax + r15 * 8 + 20], 1;
	shrq [rax + r15 * 8 + 20], 18;
	shrq [rcx + rax * 1], 1;
	shrq [rcx + rax * 1], 18;
	shrq [rcx + rax * 1 + 20], 1;
	shrq [rcx + rax * 1 + 20], 18;
	shrq [rcx + rax * 8], 1;
	shrq [rcx + rax * 8], 18;
	shrq [rcx + rax * 8 + 20], 1;
	shrq [rcx + rax * 8 + 20], 18;
	shrq [rcx + r15 * 1], 1;
	shrq [rcx + r15 * 1], 18;
	shrq [rcx + r15 * 1 + 20], 1;
	shrq [rcx + r15 * 1 + 20], 18;
	shrq [rcx + r15 * 8], 1;
	shrq [rcx + r15 * 8], 18;
	shrq [rcx + r15 * 8 + 20], 1;
	shrq [rcx + r15 * 8 + 20], 18;
	shrq [rdx + rax * 1], 1;
	shrq [rdx + rax * 1], 18;
	shrq [rdx + rax * 1 + 20], 1;
	shrq [rdx + rax * 1 + 20], 18;
	shrq [rdx + rax * 8], 1;
	shrq [rdx + rax * 8], 18;
	shrq [rdx + rax * 8 + 20], 1;
	shrq [rdx + rax * 8 + 20], 18;
	shrq [rdx + r15 * 1], 1;
	shrq [rdx + r15 * 1], 18;
	shrq [rdx + r15 * 1 + 20], 1;
	shrq [rdx + r15 * 1 + 20], 18;
	shrq [rdx + r15 * 8], 1;
	shrq [rdx + r15 * 8], 18;
	shrq [rdx + r15 * 8 + 20], 1;
	shrq [rdx + r15 * 8 + 20], 18;
	shrq [rbx + rax * 1], 1;
	shrq [rbx + rax * 1], 18;
	shrq [rbx + rax * 1 + 20], 1;
	shrq [rbx + rax * 1 + 20], 18;
	shrq [rbx + rax * 8], 1;
	shrq [rbx + rax * 8], 18;
	shrq [rbx + rax * 8 + 20], 1;
	shrq [rbx + rax * 8 + 20], 18;
	shrq [rbx + r15 * 1], 1;
	shrq [rbx + r15 * 1], 18;
	shrq [rbx + r15 * 1 + 20], 1;
	shrq [rbx + r15 * 1 + 20], 18;
	shrq [rbx + r15 * 8], 1;
	shrq [rbx + r15 * 8], 18;
	shrq [rbx + r15 * 8 + 20], 1;
	shrq [rbx + r15 * 8 + 20], 18;
	shrq [rsp + rax * 1], 1;
	shrq [rsp + rax * 1], 18;
	shrq [rsp + rax * 1 + 20], 1;
	shrq [rsp + rax * 1 + 20], 18;
	shrq [rsp + rax * 8], 1;
	shrq [rsp + rax * 8], 18;
	shrq [rsp + rax * 8 + 20], 1;
	shrq [rsp + rax * 8 + 20], 18;
	shrq [rsp + r15 * 1], 1;
	shrq [rsp + r15 * 1], 18;
	shrq [rsp + r15 * 1 + 20], 1;
	shrq [rsp + r15 * 1 + 20], 18;
	shrq [rsp + r15 * 8], 1;
	shrq [rsp + r15 * 8], 18;
	shrq [rsp + r15 * 8 + 20], 1;
	shrq [rsp + r15 * 8 + 20], 18;
	shrq [rbp + rax * 1], 1;
	shrq [rbp + rax * 1], 18;
	shrq [rbp + rax * 1 + 20], 1;
	shrq [rbp + rax * 1 + 20], 18;
	shrq [rbp + rax * 8], 1;
	shrq [rbp + rax * 8], 18;
	shrq [rbp + rax * 8 + 20], 1;
	shrq [rbp + rax * 8 + 20], 18;
	shrq [rbp + r15 * 1], 1;
	shrq [rbp + r15 * 1], 18;
	shrq [rbp + r15 * 1 + 20], 1;
	shrq [rbp + r15 * 1 + 20], 18;
	shrq [rbp + r15 * 8], 1;
	shrq [rbp + r15 * 8], 18;
	shrq [rbp + r15 * 8 + 20], 1;
	shrq [rbp + r15 * 8 + 20], 18;
	shrq [rsi + rax * 1], 1;
	shrq [rsi + rax * 1], 18;
	shrq [rsi + rax * 1 + 20], 1;
	shrq [rsi + rax * 1 + 20], 18;
	shrq [rsi + rax * 8], 1;
	shrq [rsi + rax * 8], 18;
	shrq [rsi + rax * 8 + 20], 1;
	shrq [rsi + rax * 8 + 20], 18;
	shrq [rsi + r15 * 1], 1;
	shrq [rsi + r15 * 1], 18;
	shrq [rsi + r15 * 1 + 20], 1;
	shrq [rsi + r15 * 1 + 20], 18;
	shrq [rsi + r15 * 8], 1;
	shrq [rsi + r15 * 8], 18;
	shrq [rsi + r15 * 8 + 20], 1;
	shrq [rsi + r15 * 8 + 20], 18;
	shrq [rdi + rax * 1], 1;
	shrq [rdi + rax * 1], 18;
	shrq [rdi + rax * 1 + 20], 1;
	shrq [rdi + rax * 1 + 20], 18;
	shrq [rdi + rax * 8], 1;
	shrq [rdi + rax * 8], 18;
	shrq [rdi + rax * 8 + 20], 1;
	shrq [rdi + rax * 8 + 20], 18;
	shrq [rdi + r15 * 1], 1;
	shrq [rdi + r15 * 1], 18;
	shrq [rdi + r15 * 1 + 20], 1;
	shrq [rdi + r15 * 1 + 20], 18;
	shrq [rdi + r15 * 8], 1;
	shrq [rdi + r15 * 8], 18;
	shrq [rdi + r15 * 8 + 20], 1;
	shrq [rdi + r15 * 8 + 20], 18;
	shrq [r8 + rax * 1], 1;
	shrq [r8 + rax * 1], 18;
	shrq [r8 + rax * 1 + 20], 1;
	shrq [r8 + rax * 1 + 20], 18;
	shrq [r8 + rax * 8], 1;
	shrq [r8 + rax * 8], 18;
	shrq [r8 + rax * 8 + 20], 1;
	shrq [r8 + rax * 8 + 20], 18;
	shrq [r8 + r15 * 1], 1;
	shrq [r8 + r15 * 1], 18;
	shrq [r8 + r15 * 1 + 20], 1;
	shrq [r8 + r15 * 1 + 20], 18;
	shrq [r8 + r15 * 8], 1;
	shrq [r8 + r15 * 8], 18;
	shrq [r8 + r15 * 8 + 20], 1;
	shrq [r8 + r15 * 8 + 20], 18;
	shrq [r9 + rax * 1], 1;
	shrq [r9 + rax * 1], 18;
	shrq [r9 + rax * 1 + 20], 1;
	shrq [r9 + rax * 1 + 20], 18;
	shrq [r9 + rax * 8], 1;
	shrq [r9 + rax * 8], 18;
	shrq [r9 + rax * 8 + 20], 1;
	shrq [r9 + rax * 8 + 20], 18;
	shrq [r9 + r15 * 1], 1;
	shrq [r9 + r15 * 1], 18;
	shrq [r9 + r15 * 1 + 20], 1;
	shrq [r9 + r15 * 1 + 20], 18;
	shrq [r9 + r15 * 8], 1;
	shrq [r9 + r15 * 8], 18;
	shrq [r9 + r15 * 8 + 20], 1;
	shrq [r9 + r15 * 8 + 20], 18;
	shrq [r10 + rax * 1], 1;
	shrq [r10 + rax * 1], 18;
	shrq [r10 + rax * 1 + 20], 1;
	shrq [r10 + rax * 1 + 20], 18;
	shrq [r10 + rax * 8], 1;
	shrq [r10 + rax * 8], 18;
	shrq [r10 + rax * 8 + 20], 1;
	shrq [r10 + rax * 8 + 20], 18;
	shrq [r10 + r15 * 1], 1;
	shrq [r10 + r15 * 1], 18;
	shrq [r10 + r15 * 1 + 20], 1;
	shrq [r10 + r15 * 1 + 20], 18;
	shrq [r10 + r15 * 8], 1;
	shrq [r10 + r15 * 8], 18;
	shrq [r10 + r15 * 8 + 20], 1;
	shrq [r10 + r15 * 8 + 20], 18;
	shrq [r11 + rax * 1], 1;
	shrq [r11 + rax * 1], 18;
	shrq [r11 + rax * 1 + 20], 1;
	shrq [r11 + rax * 1 + 20], 18;
	shrq [r11 + rax * 8], 1;
	shrq [r11 + rax * 8], 18;
	shrq [r11 + rax * 8 + 20], 1;
	shrq [r11 + rax * 8 + 20], 18;
	shrq [r11 + r15 * 1], 1;
	shrq [r11 + r15 * 1], 18;
	shrq [r11 + r15 * 1 + 20], 1;
	shrq [r11 + r15 * 1 + 20], 18;
	shrq [r11 + r15 * 8], 1;
	shrq [r11 + r15 * 8], 18;
	shrq [r11 + r15 * 8 + 20], 1;
	shrq [r11 + r15 * 8 + 20], 18;
	shrq [r12 + rax * 1], 1;
	shrq [r12 + rax * 1], 18;
	shrq [r12 + rax * 1 + 20], 1;
	shrq [r12 + rax * 1 + 20], 18;
	shrq [r12 + rax * 8], 1;
	shrq [r12 + rax * 8], 18;
	shrq [r12 + rax * 8 + 20], 1;
	shrq [r12 + rax * 8 + 20], 18;
	shrq [r12 + r15 * 1], 1;
	shrq [r12 + r15 * 1], 18;
	shrq [r12 + r15 * 1 + 20], 1;
	shrq [r12 + r15 * 1 + 20], 18;
	shrq [r12 + r15 * 8], 1;
	shrq [r12 + r15 * 8], 18;
	shrq [r12 + r15 * 8 + 20], 1;
	shrq [r12 + r15 * 8 + 20], 18;
	shrq [r13 + rax * 1], 1;
	shrq [r13 + rax * 1], 18;
	shrq [r13 + rax * 1 + 20], 1;
	shrq [r13 + rax * 1 + 20], 18;
	shrq [r13 + rax * 8], 1;
	shrq [r13 + rax * 8], 18;
	shrq [r13 + rax * 8 + 20], 1;
	shrq [r13 + rax * 8 + 20], 18;
	shrq [r13 + r15 * 1], 1;
	shrq [r13 + r15 * 1], 18;
	shrq [r13 + r15 * 1 + 20], 1;
	shrq [r13 + r15 * 1 + 20], 18;
	shrq [r13 + r15 * 8], 1;
	shrq [r13 + r15 * 8], 18;
	shrq [r13 + r15 * 8 + 20], 1;
	shrq [r13 + r15 * 8 + 20], 18;
	shrq [r14 + rax * 1], 1;
	shrq [r14 + rax * 1], 18;
	shrq [r14 + rax * 1 + 20], 1;
	shrq [r14 + rax * 1 + 20], 18;
	shrq [r14 + rax * 8], 1;
	shrq [r14 + rax * 8], 18;
	shrq [r14 + rax * 8 + 20], 1;
	shrq [r14 + rax * 8 + 20], 18;
	shrq [r14 + r15 * 1], 1;
	shrq [r14 + r15 * 1], 18;
	shrq [r14 + r15 * 1 + 20], 1;
	shrq [r14 + r15 * 1 + 20], 18;
	shrq [r14 + r15 * 8], 1;
	shrq [r14 + r15 * 8], 18;
	shrq [r14 + r15 * 8 + 20], 1;
	shrq [r14 + r15 * 8 + 20], 18;
	shrq [r15 + rax * 1], 1;
	shrq [r15 + rax * 1], 18;
	shrq [r15 + rax * 1 + 20], 1;
	shrq [r15 + rax * 1 + 20], 18;
	shrq [r15 + rax * 8], 1;
	shrq [r15 + rax * 8], 18;
	shrq [r15 + rax * 8 + 20], 1;
	shrq [r15 + rax * 8 + 20], 18;
	shrq [r15 + r15 * 1], 1;
	shrq [r15 + r15 * 1], 18;
	shrq [r15 + r15 * 1 + 20], 1;
	shrq [r15 + r15 * 1 + 20], 18;
	shrq [r15 + r15 * 8], 1;
	shrq [r15 + r15 * 8], 18;
	shrq [r15 + r15 * 8 + 20], 1;
	shrq [r15 + r15 * 8 + 20], 18;
      }
      jit.finalize();
      let mut buf = std::fs::File::create("tests/shrq_monoasm.bin").unwrap();
      buf.write_all(jit.as_slice()).unwrap();
  }
