extern crate monoasm;
extern crate monoasm_macro;

use monoasm::*;
use monoasm_macro::monoasm;
pub type ReturnFunc = extern "C" fn() -> u64;

#[test]
fn lzcntl() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    let val = 0x000FF0F0;
    monoasm!(&mut jit,
        begin:
            xorq rax, rax;
            movl rax, (val);
            lzcntl rax, rax;
            ret;
    );
    jit.finalize();

    let f: ReturnFunc = unsafe { std::mem::transmute(jit.get_label_u64(begin)) };
    let ret = f() as u64;
    assert_eq!(ret, 12);
}

#[test]
fn tzcntl() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    let val = 0x000F0F40; // 0000 0000 0000 1111 0000 1111 0100 0000
    monoasm!(&mut jit,
        begin:
            xorq rax, rax;
            movl rax, (val);
            tzcntl rax, rax;
            ret;
    );
    jit.finalize();

    let f: ReturnFunc = unsafe { std::mem::transmute(jit.get_label_u64(begin)) };
    let ret = f() as u64;
    assert_eq!(ret, 6);
}

#[test]
fn popcntl() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    let val = 0xDEADBEEFu32; // 1101 1110 1010 1101 1011 1110 1110 1111
    monoasm!(&mut jit,
        begin:
            xorq rax, rax;
            movq rax, (val);
            popcntl rax, rax;
            ret;
    );
    jit.finalize();

    let f: ReturnFunc = unsafe { std::mem::transmute(jit.get_label_u64(begin)) };
    let ret = f() as u64;
    assert_eq!(ret, 24);
}
