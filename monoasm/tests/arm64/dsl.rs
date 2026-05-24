//! Tests for the `monoasm_arm64!` assembly DSL macro. These assemble code
//! through the macro (rather than the builder methods directly) and run it,
//! so they require the aarch64 target (under qemu on an x86-64 host):
//!
//! ```text
//! cargo test --target aarch64-unknown-linux-gnu --test arm64
//! ```

use monoasm::*;
use monoasm_macro::monoasm_arm64;

#[test]
fn dsl_returns_constant() {
    let mut jit = JitMemory::new();
    let entry = jit.label();
    monoasm_arm64!(&mut jit,
    entry:
        mov x0, (0x1234_5678_9abc_def0u64);
        ret;
    );
    jit.finalize();
    let f: extern "C" fn() -> u64 = unsafe { std::mem::transmute(jit.get_label_u64(&entry)) };
    assert_eq!(f(), 0x1234_5678_9abc_def0);
}

#[test]
fn dsl_movz_movk() {
    let mut jit = JitMemory::new();
    let entry = jit.label();
    monoasm_arm64!(&mut jit,
    entry:
        movz x0, #0x1234;
        movk x0, #0x5678, lsl #16;
        ret;
    );
    jit.finalize();
    let f: extern "C" fn() -> u64 = unsafe { std::mem::transmute(jit.get_label_u64(&entry)) };
    assert_eq!(f(), 0x5678_1234);
}

#[test]
fn dsl_arithmetic() {
    // (a + b) * c - 1
    let mut jit = JitMemory::new();
    let entry = jit.label();
    monoasm_arm64!(&mut jit,
    entry:
        add x0, x0, x1;
        mul x0, x0, x2;
        sub x0, x0, #1;
        ret;
    );
    jit.finalize();
    let f: extern "C" fn(u64, u64, u64) -> u64 =
        unsafe { std::mem::transmute(jit.get_label_u64(&entry)) };
    assert_eq!(f(3, 4, 5), (3 + 4) * 5 - 1);
    assert_eq!(f(10, 20, 2), (10 + 20) * 2 - 1);
}

#[test]
fn dsl_add_shifted_and_logical() {
    // (a & b) | (c << 2)
    let mut jit = JitMemory::new();
    let entry = jit.label();
    monoasm_arm64!(&mut jit,
    entry:
        and x0, x0, x1;
        lsl x2, x2, #2;
        orr x0, x0, x2;
        ret;
    );
    jit.finalize();
    let f: extern "C" fn(u64, u64, u64) -> u64 =
        unsafe { std::mem::transmute(jit.get_label_u64(&entry)) };
    assert_eq!(f(0b1100, 0b1010, 0b1), (0b1100 & 0b1010) | (0b1 << 2));
}

#[test]
fn dsl_signed_division() {
    let mut jit = JitMemory::new();
    let entry = jit.label();
    monoasm_arm64!(&mut jit,
    entry:
        sdiv x0, x0, x1;
        ret;
    );
    jit.finalize();
    let f: extern "C" fn(i64, i64) -> i64 =
        unsafe { std::mem::transmute(jit.get_label_u64(&entry)) };
    assert_eq!(f(100, 7), 14);
    assert_eq!(f(-100, 7), -14);
}

#[test]
fn dsl_sum_loop() {
    // Sum 1..=n using a backward branch and a forward conditional exit.
    let mut jit = JitMemory::new();
    let entry = jit.label();
    let loop_top = jit.label();
    let done = jit.label();
    monoasm_arm64!(&mut jit,
    entry:
        mov x1, #0;
    loop_top:
        cbz x0, done;
        add x1, x1, x0;
        sub x0, x0, #1;
        b loop_top;
    done:
        mov x0, x1;
        ret;
    );
    jit.finalize();
    let f: extern "C" fn(u64) -> u64 = unsafe { std::mem::transmute(jit.get_label_u64(&entry)) };
    assert_eq!(f(0), 0);
    assert_eq!(f(10), 55);
    assert_eq!(f(100), 5050);
}

#[test]
fn dsl_max_via_csel() {
    let mut jit = JitMemory::new();
    let entry = jit.label();
    monoasm_arm64!(&mut jit,
    entry:
        cmp x0, x1;
        csel x0, x0, x1, gt;
        ret;
    );
    jit.finalize();
    let f: extern "C" fn(i64, i64) -> i64 =
        unsafe { std::mem::transmute(jit.get_label_u64(&entry)) };
    assert_eq!(f(3, 7), 7);
    assert_eq!(f(7, 3), 7);
    assert_eq!(f(-5, -9), -5);
}

#[test]
fn dsl_max_via_bcond() {
    let mut jit = JitMemory::new();
    let entry = jit.label();
    let skip = jit.label();
    monoasm_arm64!(&mut jit,
    entry:
        cmp x0, x1;
        b.ge skip;
        mov x0, x1;
    skip:
        ret;
    );
    jit.finalize();
    let f: extern "C" fn(i64, i64) -> i64 =
        unsafe { std::mem::transmute(jit.get_label_u64(&entry)) };
    assert_eq!(f(3, 7), 7);
    assert_eq!(f(7, 3), 7);
    assert_eq!(f(-5, -9), -5);
}

#[test]
fn dsl_stack_pair() {
    // Push the two args, clobber the regs, pop into different regs, add.
    let mut jit = JitMemory::new();
    let entry = jit.label();
    monoasm_arm64!(&mut jit,
    entry:
        stp x0, x1, [sp, #-16]!;
        mov x0, #0;
        mov x1, #0;
        ldp x2, x3, [sp], #16;
        add x0, x2, x3;
        ret;
    );
    jit.finalize();
    let f: extern "C" fn(u64, u64) -> u64 =
        unsafe { std::mem::transmute(jit.get_label_u64(&entry)) };
    assert_eq!(f(40, 2), 42);
    assert_eq!(f(1000, 337), 1337);
}

#[test]
fn dsl_load_store_offset() {
    // Round-trip the first arg through a stack slot, then add the second.
    let mut jit = JitMemory::new();
    let entry = jit.label();
    monoasm_arm64!(&mut jit,
    entry:
        sub sp, sp, #16;
        str x0, [sp, #8];
        ldr x2, [sp, #8];
        add x0, x2, x1;
        add sp, sp, #16;
        ret;
    );
    jit.finalize();
    let f: extern "C" fn(u64, u64) -> u64 =
        unsafe { std::mem::transmute(jit.get_label_u64(&entry)) };
    assert_eq!(f(40, 2), 42);
}

#[test]
fn dsl_floating_point() {
    // (a + b) / c
    let mut jit = JitMemory::new();
    let entry = jit.label();
    monoasm_arm64!(&mut jit,
    entry:
        fadd d0, d0, d1;
        fdiv d0, d0, d2;
        ret;
    );
    jit.finalize();
    let f: extern "C" fn(f64, f64, f64) -> f64 =
        unsafe { std::mem::transmute(jit.get_label_u64(&entry)) };
    assert_eq!(f(3.0, 5.0, 2.0), 4.0);
    assert_eq!(f(1.0, 1.0, 4.0), 0.5);
}

#[test]
fn dsl_int_float_conversion() {
    // trunc(x * 2.5)
    let mut jit = JitMemory::new();
    let entry = jit.label();
    monoasm_arm64!(&mut jit,
    entry:
        scvtf d0, x0;
        mov x1, (0x4004_0000_0000_0000u64); // 2.5 as f64 bits
        fmov d1, x1;
        fmul d0, d0, d1;
        fcvtzs x0, d0;
        ret;
    );
    jit.finalize();
    let f: extern "C" fn(i64) -> i64 = unsafe { std::mem::transmute(jit.get_label_u64(&entry)) };
    assert_eq!(f(4), 10);
    assert_eq!(f(3), 7);
    assert_eq!(f(0), 0);
}

#[test]
fn dsl_function_call_via_bl() {
    // entry: save fp/lr, bl callee, restore, ret. callee returns 42.
    let mut jit = JitMemory::new();
    let entry = jit.label();
    let callee = jit.label();
    monoasm_arm64!(&mut jit,
    entry:
        stp fp, lr, [sp, #-16]!;
        bl callee;
        ldp fp, lr, [sp], #16;
        ret;
    callee:
        mov x0, #42;
        ret;
    );
    jit.finalize();
    let f: extern "C" fn() -> u64 = unsafe { std::mem::transmute(jit.get_label_u64(&entry)) };
    assert_eq!(f(), 42);
}
