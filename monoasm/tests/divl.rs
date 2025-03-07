extern crate monoasm;
extern crate monoasm_macro;

use monoasm::*;
use monoasm_macro::monoasm;

#[repr(C)]
#[derive(Debug)]
struct UnsignedDiv {
    quotient: u64,  // eax
    remainder: u64, // edx
}

#[test]
fn divl() {
    let mut jit: JitMemory = JitMemory::new();
    let begin = jit.label();
    monoasm!(&mut jit,
        begin:
            movq rax, (0xffff_ffff_ffff_ffffu64);
            movq rdx, rax;
            movl rax, rdi;
            cdq;
            divl rsi;
            ret;
    );
    jit.finalize();
    eprintln!("{}", jit.dump_code().unwrap());

    let dividend = 7777777;
    let divisor = 111112;
    let f = jit.get_label_addr2::<u32, u32, UnsignedDiv>(&begin);
    let ret = dbg!(f(dividend, divisor)); // rax contains (7)
    assert_eq!(ret.quotient as u32, dividend / divisor);
    assert_eq!(ret.remainder as u32, dividend % divisor);
}
