//! End-to-end execution tests for the AArch64 backend.
//!
//! These actually *call* the generated machine code, so they only make
//! sense when the test binary itself targets AArch64. On an x86-64
//! development host run them under emulation:
//!
//! ```text
//! cargo test --target aarch64-unknown-linux-gnu --test arm64
//! ```
//!
//! (see `.cargo/config.toml` for the qemu runner wiring).

use monoasm::*;
use monoasm_macro::monoasm_arm64;

/// Assemble a function body, finalize, and return both the live
/// [`JitMemory`] (kept alive so the code stays mapped) and the entry
/// address.
fn jit_fn(emit: impl FnOnce(&mut JitMemory)) -> (JitMemory, u64) {
    let mut jit = JitMemory::new();
    let entry = jit.label();
    jit.bind_label(entry.clone());
    emit(&mut jit);
    jit.finalize();
    let addr = jit.get_label_u64(&entry);
    (jit, addr)
}

#[test]
fn returns_constant() {
    let (_jit, addr) = jit_fn(|j| {
        monoasm_arm64!(&mut *j,
            mov x0, (0x1234_5678_9abc_def0u64);
            ret;
        );
    });
    let f: extern "C" fn() -> u64 = unsafe { std::mem::transmute(addr) };
    assert_eq!(f(), 0x1234_5678_9abc_def0);
    let (_jit, addr) = jit_fn(|j| {
        monoasm_arm64!(&mut *j,
            mov x0, (u64::MAX);
            ret;
        );
    });
    let f: extern "C" fn() -> u64 = unsafe { std::mem::transmute(addr) };
    assert_eq!(f(), u64::MAX);
}

#[test]
fn arithmetic() {
    // (a + b) * c - 1
    let (_jit, addr) = jit_fn(|j| {
        monoasm_arm64!(&mut *j,
            add x0, x0, x1;
            mul x0, x0, x2;
            sub x0, x0, #1;
            ret;
        );
    });
    let f: extern "C" fn(u64, u64, u64) -> u64 = unsafe { std::mem::transmute(addr) };
    assert_eq!(f(3, 4, 5), (3 + 4) * 5 - 1);
    assert_eq!(f(10, 20, 2), (10 + 20) * 2 - 1);
}

#[test]
fn signed_division() {
    let (_jit, addr) = jit_fn(|j| {
        monoasm_arm64!(&mut *j,
            sdiv x0, x0, x1;
            ret;
        );
    });
    let f: extern "C" fn(i64, i64) -> i64 = unsafe { std::mem::transmute(addr) };
    assert_eq!(f(100, 7), 14);
    assert_eq!(f(-100, 7), -14);
}

#[test]
fn multiply_high() {
    // smulh: high 64 bits of the signed 64x64 product.
    let (_jit, addr) = jit_fn(|j| {
        monoasm_arm64!(&mut *j,
            smulh x0, x0, x1;
            ret;
        );
    });
    let f: extern "C" fn(i64, i64) -> i64 = unsafe { std::mem::transmute(addr) };
    let smulh = |a: i64, b: i64| ((a as i128 * b as i128) >> 64) as i64;
    assert_eq!(f(1 << 62, 4), smulh(1 << 62, 4));
    assert_eq!(f(-1, 1), smulh(-1, 1));
    assert_eq!(f(i64::MAX, i64::MAX), smulh(i64::MAX, i64::MAX));
    assert_eq!(f(i64::MIN, i64::MIN), smulh(i64::MIN, i64::MIN));

    // umulh: high 64 bits of the unsigned 64x64 product.
    let (_jit, addr) = jit_fn(|j| {
        monoasm_arm64!(&mut *j,
            umulh x0, x0, x1;
            ret;
        );
    });
    let f: extern "C" fn(u64, u64) -> u64 = unsafe { std::mem::transmute(addr) };
    let umulh = |a: u64, b: u64| ((a as u128 * b as u128) >> 64) as u64;
    assert_eq!(f(1 << 63, 4), umulh(1 << 63, 4));
    assert_eq!(f(u64::MAX, u64::MAX), umulh(u64::MAX, u64::MAX));
}

#[test]
fn sum_loop() {
    // Sum 1..=n using a backward branch and a forward conditional exit,
    // exercising relocation in both directions.
    let (_jit, addr) = jit_fn(|j| {
        let loop_top = j.label();
        let done = j.label();
        monoasm_arm64!(&mut *j,
            mov x1, #0;          // acc = 0
        loop_top:
            cbz x0, done;        // forward branch
            add x1, x1, x0;
            sub x0, x0, #1;
            b loop_top;          // backward branch
        done:
            mov x0, x1;
            ret;
        );
    });
    let f: extern "C" fn(u64) -> u64 = unsafe { std::mem::transmute(addr) };
    assert_eq!(f(0), 0);
    assert_eq!(f(1), 1);
    assert_eq!(f(10), 55);
    assert_eq!(f(100), 5050);
}

#[test]
fn signed_max_via_csel() {
    let (_jit, addr) = jit_fn(|j| {
        monoasm_arm64!(&mut *j,
            cmp x0, x1;
            csel x0, x0, x1, gt;
            ret;
        );
    });
    let f: extern "C" fn(i64, i64) -> i64 = unsafe { std::mem::transmute(addr) };
    assert_eq!(f(3, 7), 7);
    assert_eq!(f(7, 3), 7);
    assert_eq!(f(-5, -9), -5);
}

#[test]
fn stack_roundtrip() {
    // Push the two arguments, clobber the registers, pop them back into
    // different registers, and add.
    let (_jit, addr) = jit_fn(|j| {
        monoasm_arm64!(&mut *j,
            stp x0, x1, [sp, #-16]!;
            mov x0, #0;
            mov x1, #0;
            ldp x2, x3, [sp], #16;
            add x0, x2, x3;
            ret;
        );
    });
    let f: extern "C" fn(u64, u64) -> u64 = unsafe { std::mem::transmute(addr) };
    assert_eq!(f(40, 2), 42);
    assert_eq!(f(1000, 337), 1337);
}

#[test]
fn function_call_via_bl() {
    // entry: save fp/lr, bl callee, restore, ret. callee returns 42.
    let (_jit, addr) = jit_fn(|j| {
        let callee = j.label();
        monoasm_arm64!(&mut *j,
            stp fp, lr, [sp, #-16]!;
            bl callee;
            ldp fp, lr, [sp], #16;
            ret;
        callee:
            mov x0, #42;
            ret;
        );
    });
    let f: extern "C" fn() -> u64 = unsafe { std::mem::transmute(addr) };
    assert_eq!(f(), 42);
}

#[test]
fn floating_point() {
    // (a + b) / c
    let (_jit, addr) = jit_fn(|j| {
        monoasm_arm64!(&mut *j,
            fadd d0, d0, d1;
            fdiv d0, d0, d2;
            ret;
        );
    });
    let f: extern "C" fn(f64, f64, f64) -> f64 = unsafe { std::mem::transmute(addr) };
    assert_eq!(f(3.0, 5.0, 2.0), 4.0);
    assert_eq!(f(1.0, 1.0, 4.0), 0.5);
}

#[test]
fn int_float_conversion() {
    // trunc(x * 2.5) using scvtf / fmov-from-gpr / fmul / fcvtzs.
    let (_jit, addr) = jit_fn(|j| {
        monoasm_arm64!(&mut *j,
            scvtf d0, x0;
            mov x1, (0x4004_0000_0000_0000u64); // 2.5 as f64 bits
            fmov d1, x1;
            fmul d0, d0, d1;
            fcvtzs x0, d0;
            ret;
        );
    });
    let f: extern "C" fn(i64) -> i64 = unsafe { std::mem::transmute(addr) };
    assert_eq!(f(4), 10); // 4 * 2.5 = 10
    assert_eq!(f(3), 7); // 3 * 2.5 = 7.5 -> trunc 7
    assert_eq!(f(0), 0);
}
