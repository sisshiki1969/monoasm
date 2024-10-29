extern crate monoasm;
extern crate monoasm_macro;

use monoasm::*;
use monoasm_macro::monoasm;
pub type ReturnFunc = extern "C" fn() -> u64;

#[test]
fn shll() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    let val = 0x0000000000000040u64; // 0000 ... 0000 0100 0000
    monoasm!(&mut jit,
        begin:
            movb cl, (0x1F); // 31, oops, its all gone since it is shll
            movl rax, (val);
            shll rax, cl;
            ret;
    );
    jit.finalize();

    let f: ReturnFunc = unsafe { std::mem::transmute(jit.get_label_u64(begin)) };
    assert_eq!(f(), 0x0000000000000000u64);
}

#[test]
fn shrl() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    let val = 0x0000000000000040u64; // 0000 ... 0000 0100 0000
    monoasm!(&mut jit,
        begin:
            movb cl, (0x01);
            movl rax, (val);
            shrl rax, cl;
            ret;
    );
    jit.finalize();

    let f: ReturnFunc = unsafe { std::mem::transmute(jit.get_label_u64(begin)) };
    assert_eq!(f(), 0x0000000000000020u64);
}

#[test]
fn sall() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    let val = 0x0000000000000001u64; // 0000 ... 0000 0001
    monoasm!(&mut jit,
        begin:
            movb cl, (0x1F);
            movl rax, (val);
            sall rax, cl;
            ret;
    );
    jit.finalize();

    let f: ReturnFunc = unsafe { std::mem::transmute(jit.get_label_u64(begin)) };
    assert_eq!(f(), 0x0000000080000000u64);
}

#[test]
fn sarl() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    let val = 0x0000000080000000u64; // 0000 ... 0000 0001
    monoasm!(&mut jit,
        begin:
            movb cl, (0x1F);
            movq rax, (val);
            sarl rax, cl; // dword operation, so it should carry the sign bit
            ret;
    );
    jit.finalize();

    let f: ReturnFunc = unsafe { std::mem::transmute(jit.get_label_u64(begin)) };
    assert_eq!(f(), 0x00000000FFFFFFFFu64);
}

#[test]
fn roll() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    let val = 0x0000000000000001u64; // 0000 ... 0000 0001
    monoasm!(&mut jit,
        begin:
            movb cl, (0x04);
            movl rax, (val);
            roll rax, cl;
            ret;
    );
    jit.finalize();

    let f: ReturnFunc = unsafe { std::mem::transmute(jit.get_label_u64(begin)) };
    assert_eq!(f(), 0x0000000000000010u64);
}

#[test]
fn rorl() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    let val = 0x0000000000000010u64; // 0000 ... 0000 0001
    monoasm!(&mut jit,
        begin:
            movb cl, (0x04);
            movl rax, (val);
            rorl rax, cl;
            ret;
    );
    jit.finalize();

    let f: ReturnFunc = unsafe { std::mem::transmute(jit.get_label_u64(begin)) };
    assert_eq!(f(), 0x0000000000000001u64);
}
