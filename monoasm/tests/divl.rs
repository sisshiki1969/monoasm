extern crate monoasm;
extern crate monoasm_macro;

use monoasm::*;
use monoasm_macro::monoasm;

#[test]
fn divl() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    monoasm!(&mut jit,
        begin:
            xorl rdx, rdx;
            xorl rax, rax;
            movq rax, rdi;
            cdq;
            divl rsi; // eax = 7, edx = 0
            ret;
    );
    jit.finalize();
    eprintln!("{}", jit.dump_code().unwrap());

    let f = jit.get_label_addr2::<i32, i32, i32>(begin);
    let ret = f(7777777, 1111111); // rax contains (7)
    assert_eq!(ret, 7);
}

#[test]
fn divl_rem() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    monoasm!(&mut jit,
        begin:
            xorl rdx, rdx;
            xorl rax, rax;
            movq rax, rdi;
            cdq;
            divl rsi; // eax = 2, edx = 1
            movl rax, rdx;
            ret;
    );
    jit.finalize();
    eprintln!("{}", jit.dump_code().unwrap());

    let f = jit.get_label_addr2::<i32, i32, i32>(begin);
    let ret = f(7, 3); // rax contains (1)
    assert_eq!(ret, 1);
}
