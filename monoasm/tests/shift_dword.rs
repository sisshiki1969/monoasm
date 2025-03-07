extern crate monoasm;
extern crate monoasm_macro;

use monoasm::*;
use monoasm_macro::monoasm;

#[test]
fn shll() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    let val = 0x0000000000000040u64; // 0000 ... 0000 0100 0000
    monoasm!(&mut jit,
        begin:
            movq rax, rdi;
            movb cl, (0x1F); // 31, oops, its all gone since it is shll
            shll rax, cl;
            ret;
    );
    jit.finalize();
    eprintln!("{}", jit.dump_code().unwrap());

    let f = jit.get_label_addr::<u64, u64>(&begin);
    let ret = f(val);
    assert_eq!(ret, 0x0000000000000000u64);
}

#[test]
fn shrl() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    let val = 0x0000000000000040u64; // 0000 ... 0000 0100 0000
    monoasm!(&mut jit,
        begin:
            movq rax, rdi;
            movb cl, (0x01);
            shrl rax, cl;
            ret;
    );
    jit.finalize();
    eprintln!("{}", jit.dump_code().unwrap());

    let f = jit.get_label_addr::<u64, u64>(&begin);
    let ret = f(val);
    assert_eq!(ret, 0x0000000000000020u64);
}

#[test]
fn sall() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    let val = 0x0000000000000001u64; // 0000 ... 0000 0001
    monoasm!(&mut jit,
        begin:
            movq rax, rdi;
            movb cl, (0x1F);
            sall rax, cl;
            ret;
    );
    jit.finalize();
    eprintln!("{}", jit.dump_code().unwrap());

    let f = jit.get_label_addr::<u64, u64>(&begin);
    let ret = f(val);
    assert_eq!(ret, 0x0000000080000000u64);
}

#[test]
fn sarl() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    let val = 0x0000000080000000u64; // 0000 ... 0000 0001
    monoasm!(&mut jit,
        begin:
            movq rax, rdi;
            movb cl, (0x1F);
            sarl rax, cl; // dword operation, so it should carry the sign bit
            ret;
    );
    jit.finalize();
    eprintln!("{}", jit.dump_code().unwrap());

    let f = jit.get_label_addr::<u64, u64>(&begin);
    let ret = f(val);
    assert_eq!(ret, 0x00000000FFFFFFFFu64);
}

#[test]
fn roll() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    let val = 0x0000000000000001u64; // 0000 ... 0000 0001
    monoasm!(&mut jit,
        begin:
            movq rax, rdi;
            movb cl, (0x04);
            roll rax, cl;
            ret;
    );
    jit.finalize();
    eprintln!("{}", jit.dump_code().unwrap());

    let f = jit.get_label_addr::<u64, u64>(&begin);
    let ret = f(val);
    assert_eq!(ret, 0x0000000000000010u64);
}

#[test]
fn rorl() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    let val = 0x0000000000000010u64; // 0000 ... 0000 0001
    monoasm!(&mut jit,
        begin:
            movq rax, rdi;
            movb cl, (0x04);
            rorl rax, cl;
            ret;
    );
    jit.finalize();
    eprintln!("{}", jit.dump_code().unwrap());

    let f = jit.get_label_addr::<u64, u64>(&begin);
    let ret = f(val);
    assert_eq!(ret, 0x0000000000000001u64);
}
