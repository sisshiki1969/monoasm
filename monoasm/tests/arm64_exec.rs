//! End-to-end execution tests for the AArch64 backend.
//!
//! These actually *call* the generated machine code, so they only make
//! sense when the test binary itself targets AArch64. On an x86-64
//! development host run them under emulation:
//!
//! ```text
//! cargo test --target aarch64-unknown-linux-gnu --test arm64_exec
//! ```
//!
//! (see `.cargo/config.toml` for the qemu runner wiring).
#![cfg(target_arch = "aarch64")]

use monoasm::*;

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
        j.mov_imm(X0, 0x1234_5678_9abc_def0);
        j.ret();
    });
    let f: extern "C" fn() -> u64 = unsafe { std::mem::transmute(addr) };
    assert_eq!(f(), 0x1234_5678_9abc_def0);
    let (_jit, addr) = jit_fn(|j| {
        j.mov_imm(X0, u64::MAX);
        j.ret();
    });
    let f: extern "C" fn() -> u64 = unsafe { std::mem::transmute(addr) };
    assert_eq!(f(), u64::MAX);
}

#[test]
fn arithmetic() {
    // (a + b) * c - 1
    let (_jit, addr) = jit_fn(|j| {
        j.add(X0, X0, X1);
        j.mul(X0, X0, X2);
        j.sub_imm(X0, X0, 1, 0);
        j.ret();
    });
    let f: extern "C" fn(u64, u64, u64) -> u64 = unsafe { std::mem::transmute(addr) };
    assert_eq!(f(3, 4, 5), (3 + 4) * 5 - 1);
    assert_eq!(f(10, 20, 2), (10 + 20) * 2 - 1);
}

#[test]
fn signed_division() {
    let (_jit, addr) = jit_fn(|j| {
        j.sdiv(X0, X0, X1);
        j.ret();
    });
    let f: extern "C" fn(i64, i64) -> i64 = unsafe { std::mem::transmute(addr) };
    assert_eq!(f(100, 7), 14);
    assert_eq!(f(-100, 7), -14);
}

#[test]
fn sum_loop() {
    // Sum 1..=n using a backward branch and a forward conditional exit,
    // exercising relocation in both directions.
    let (_jit, addr) = jit_fn(|j| {
        let loop_top = j.label();
        let done = j.label();
        j.mov_imm(X1, 0); // acc = 0
        j.bind_label(loop_top.clone());
        j.cbz_label(X0, &done); // forward branch
        j.add(X1, X1, X0);
        j.sub_imm(X0, X0, 1, 0);
        j.b_label(&loop_top); // backward branch
        j.bind_label(done.clone());
        j.mov(X0, X1);
        j.ret();
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
        j.cmp(X0, X1);
        j.csel(X0, X0, X1, Cond::Gt);
        j.ret();
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
        j.push_pair(X0, X1);
        j.mov_imm(X0, 0);
        j.mov_imm(X1, 0);
        j.pop_pair(X2, X3);
        j.add(X0, X2, X3);
        j.ret();
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
        let end = j.label();
        j.push_pair(FP, LR);
        j.bl_label(&callee);
        j.pop_pair(FP, LR);
        j.ret();
        j.bind_label(callee.clone());
        j.mov_imm(X0, 42);
        j.ret();
        j.bind_label(end.clone());
    });
    let f: extern "C" fn() -> u64 = unsafe { std::mem::transmute(addr) };
    assert_eq!(f(), 42);
}

#[test]
fn floating_point() {
    // (a + b) / c
    let (_jit, addr) = jit_fn(|j| {
        j.fadd(D0, D0, D1);
        j.fdiv(D0, D0, D2);
        j.ret();
    });
    let f: extern "C" fn(f64, f64, f64) -> f64 = unsafe { std::mem::transmute(addr) };
    assert_eq!(f(3.0, 5.0, 2.0), 4.0);
    assert_eq!(f(1.0, 1.0, 4.0), 0.5);
}

#[test]
fn int_float_conversion() {
    // trunc(x * 2.5) using scvtf / fmov-from-gpr / fmul / fcvtzs.
    let (_jit, addr) = jit_fn(|j| {
        j.scvtf(D0, X0);
        j.mov_imm(X1, 0x4004_0000_0000_0000); // 2.5 as f64 bits
        j.fmov_from_gpr(D1, X1);
        j.fmul(D0, D0, D1);
        j.fcvtzs(X0, D0);
        j.ret();
    });
    let f: extern "C" fn(i64) -> i64 = unsafe { std::mem::transmute(addr) };
    assert_eq!(f(4), 10); // 4 * 2.5 = 10
    assert_eq!(f(3), 7); // 3 * 2.5 = 7.5 -> trunc 7
    assert_eq!(f(0), 0);
}
