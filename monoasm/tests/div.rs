use monoasm::*;
use monoasm_macro::monoasm;

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

    let f = jit.get_label_addr2::<u128, u64, UnsignedDiv>(&label);
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

    let f = jit.get_label_addr2::<i128, i64, SignedDiv>(&label);
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
