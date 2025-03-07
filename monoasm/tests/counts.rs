extern crate monoasm;
extern crate monoasm_macro;

use monoasm::*;
use monoasm_macro::monoasm;

#[test]
fn popcntq() {
    let mut jit = JitMemory::new();
    let label = jit.label();
    let data = jit.data(64);
    let magic = 0x1234_5678_9abc_def0u64;
    monoasm! { &mut jit,
    label:
      lea  rsi, [rip + data];
      movq rcx, 8;
      movq [rsi + rcx * 2 + 4], rdi;
      popcntq rax, [rsi + rcx * 2 + 4];
      ret;
    }
    jit.finalize();
    eprintln!("{}", jit.dump_code().unwrap());

    let f = jit.get_label_addr::<u64, u64>(&label);
    assert_eq!(magic.count_ones() as u64, dbg!(f(magic)));
}

#[test]
fn lzcntq() {
    let mut jit = JitMemory::new();
    let label = jit.label();
    let data = jit.data(64);
    let magic = 0x0034_5678_9abc_def0u64;
    monoasm! { &mut jit,
    label:
      lea  rsi, [rip + data];
      movq rcx, 8;
      movq [rsi + rcx * 2 + 4], rdi;
      lzcntq rax, [rsi + rcx * 2 + 4];
      ret;
    }
    jit.finalize();
    eprintln!("{}", jit.dump_code().unwrap());

    let f = jit.get_label_addr::<u64, u64>(&label);
    assert_eq!(magic.leading_zeros() as u64, dbg!(f(magic)));
}

#[test]
fn tzcntq() {
    let mut jit = JitMemory::new();
    let label = jit.label();
    let data = jit.data(64);
    let magic = 0x0012_3456_7800_0000u64;
    monoasm! { &mut jit,
    label:
      lea  rsi, [rip + data];
      movq rcx, 8;
      movq [rsi + rcx * 2 + 4], rdi;
      tzcntq rax, [rsi + rcx * 2 + 4];
      ret;
    }
    jit.finalize();
    eprintln!("{}", jit.dump_code().unwrap());

    let f = jit.get_label_addr::<u64, u64>(&label);
    assert_eq!(magic.trailing_zeros() as u64, dbg!(f(magic)));
}

#[test]
fn lzcntl() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    let val = 0x8000_0000_000F_F0F0;
    monoasm!(&mut jit,
        begin:
            movq rax, rdi;
            lzcntl rax, rax;
            ret;
    );
    jit.finalize();
    eprintln!("{}", jit.dump_code().unwrap());

    let f = jit.get_label_addr::<u64, u64>(&begin);
    let ret = dbg!(f(val));
    assert_eq!(ret, (val as u32).leading_zeros() as _);
}

#[test]
fn tzcntl() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    let val = 0x8000_0000_000F_0F40; // 0000 0000 0000 1111 0000 1111 0100 0000
    monoasm!(&mut jit,
        begin:
            movq rax, rdi;
            tzcntl rax, rax;
            ret;
    );
    jit.finalize();
    eprintln!("{}", jit.dump_code().unwrap());

    let f = jit.get_label_addr::<u64, u64>(&begin);
    let ret = dbg!(f(val));
    assert_eq!(ret, (val as u32).trailing_zeros() as _);
}

#[test]
fn popcntl() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    let val = 0xCAFE_BABE_DEAD_BEEFu64; // 1101 1110 1010 1101 1011 1110 1110 1111
    monoasm!(&mut jit,
        begin:
            movq rax, rdi;
            popcntl rax, rax;
            ret;
    );
    jit.finalize();
    eprintln!("{}", jit.dump_code().unwrap());

    let f = jit.get_label_addr::<u64, u64>(&begin);
    let ret = dbg!(f(val));
    assert_eq!(ret, (val as u32).count_ones() as _);
}
