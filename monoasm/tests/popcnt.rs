use monoasm::*;
use monoasm_macro::monoasm;

#[test]
fn popcnt() {
    let mut jit = JitMemory::new();
    let label = jit.label();
    let data = jit.data(64);
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
    let f = jit.get_label_addr::<u64, u64>(label);
    let magic: u64 = 0x123456789abcdef0;
    assert_eq!(magic.count_ones() as u64, dbg!(f(magic)));
}

#[test]
fn lzcnt() {
    let mut jit = JitMemory::new();
    let label = jit.label();
    let data = jit.data(64);
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
    let f = jit.get_label_addr::<u64, u64>(label);
    let magic: u64 = 0x3456789abcdef0;
    assert_eq!(magic.leading_zeros() as u64, dbg!(f(magic)));
}

#[test]
fn tzcnt() {
    let mut jit = JitMemory::new();
    let label = jit.label();
    let data = jit.data(64);
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
    let f = jit.get_label_addr::<u64, u64>(label);
    let magic: u64 = 0x12345678000000;
    assert_eq!(magic.trailing_zeros() as u64, dbg!(f(magic)));
}

#[repr(C)]
#[derive(Debug)]
struct UnsignedDiv {
    quotient: u64,
    remainder: u64,
}

#[test]
fn udiv() {
    let mut jit = JitMemory::new();
    let label = jit.label();
    monoasm! { &mut jit,
    label:
      movq rcx, rdx;
      movq rax, rdi;
      movq rdx, rsi;
      div  rcx;
      ret;
    }
    jit.finalize();
    eprintln!("{}", jit.dump_code().unwrap());
    let f = jit.get_label_addr2::<u128, u64, UnsignedDiv>(label);
    let dividend = 0x0123456789abcdef;
    let divisor = 0x56789a;
    let res = dbg!(f(dividend, divisor));
    assert_eq!(
        dividend,
        res.quotient as u128 * divisor as u128 + res.remainder as u128
    );
}

#[repr(C)]
#[derive(Debug)]
struct SignedDiv {
    quotient: i64,
    remainder: i64,
}

#[test]
fn idiv() {
    let mut jit = JitMemory::new();
    let label = jit.label();
    monoasm! { &mut jit,
    label:
      movq rcx, rdx;
      movq rax, rdi;
      movq rdx, rsi;
      idiv rcx;
      ret;
    }
    jit.finalize();
    eprintln!("{}", jit.dump_code().unwrap());
    let f = jit.get_label_addr2::<i128, i64, SignedDiv>(label);
    let dividend = 0x0123456789abcdef;
    let divisor = 0x56789a;
    let res = dbg!(f(dividend, divisor));
    assert_eq!(
        dividend,
        res.quotient as i128 * divisor as i128 + res.remainder as i128
    );
    let res = dbg!(f(-dividend, divisor));
    assert_eq!(
        -dividend,
        res.quotient as i128 * divisor as i128 + res.remainder as i128
    );
    let res = dbg!(f(dividend, -divisor));
    assert_eq!(
        dividend,
        res.quotient as i128 * (-divisor) as i128 + res.remainder as i128
    );
    let res = dbg!(f(-dividend, -divisor));
    assert_eq!(
        -dividend,
        res.quotient as i128 * (-divisor) as i128 + res.remainder as i128
    );
}

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
    let f1 = jit.get_label_addr2::<f64, f64, f64>(label0);
    let f2 = jit.get_label_addr2::<f64, f64, f64>(label1);
    assert_eq!(-0.1, dbg!(f1(0.02, -0.1)));
    assert_eq!(0.02, dbg!(f2(0.02, -0.1)));
}
