//! Encoding tests for the AArch64 backend.
//!
//! These assemble through the `monoasm_arm64!` macro and only *read* the
//! emitted bytes from the JIT page (they never execute them), so they need
//! no AArch64 hardware or emulator — but the backend and the macro-generated
//! code are compiled only when targeting AArch64, so build the tests for
//! that target to run them:
//!
//! ```text
//! cargo test --target aarch64-unknown-linux-gnu --test arm64
//! ```
//!
//! Expected encodings are taken from `llvm-mc --triple=aarch64`
//! (instruction-level `--show-encoding` plus object disassembly for
//! resolved branches).

use monoasm::*;
use monoasm_macro::monoasm_arm64;

/// Emit via `f`, finalize, and return the first `len` raw machine-code
/// bytes of the generated block.
fn assemble(f: impl FnOnce(&mut JitMemory), len: usize) -> Vec<u8> {
    let mut jit = JitMemory::new();
    let label = jit.label();
    jit.bind_label(label.clone());
    f(&mut jit);
    jit.finalize();
    let addr = jit.get_label_u64(&label) as *const u8;
    // SAFETY: `addr` points at the start of the just-emitted, finalized
    // code block in the RWX JIT page; `len` bytes were written there.
    unsafe { std::slice::from_raw_parts(addr, len).to_vec() }
}

fn le(word: u32) -> [u8; 4] {
    word.to_le_bytes()
}

/// Assemble a single instruction and return its 32-bit encoding.
fn word(f: impl FnOnce(&mut JitMemory)) -> u32 {
    let b = assemble(f, 4);
    u32::from_le_bytes([b[0], b[1], b[2], b[3]])
}

/// Assemble `n` instructions and return their 32-bit encodings.
fn words(f: impl FnOnce(&mut JitMemory), n: usize) -> Vec<u32> {
    assemble(f, n * 4)
        .chunks(4)
        .map(|c| u32::from_le_bytes([c[0], c[1], c[2], c[3]]))
        .collect()
}

#[test]
fn movz_variants() {
    assert_eq!(
        assemble(|j| monoasm_arm64!(&mut *j, movz x0, #0x1234, lsl #0;), 4),
        le(0xd282_4680)
    );
    assert_eq!(
        assemble(|j| monoasm_arm64!(&mut *j, movz x5, #0xabcd, lsl #16;), 4),
        le(0xd2b5_79a5)
    );
    assert_eq!(
        assemble(|j| monoasm_arm64!(&mut *j, movz x13, #0x1, lsl #48;), 4),
        le(0xd2e0_002d)
    );
}

#[test]
fn movk_movn() {
    assert_eq!(
        assemble(|j| monoasm_arm64!(&mut *j, movk x0, #0xffff, lsl #32;), 4),
        le(0xf2df_ffe0)
    );
    assert_eq!(
        assemble(|j| monoasm_arm64!(&mut *j, movn x7, #0, lsl #0;), 4),
        le(0x9280_0007)
    );
    assert_eq!(
        assemble(|j| monoasm_arm64!(&mut *j, movn x2, #0x10, lsl #16;), 4),
        le(0x92a0_0202)
    );
}

#[test]
fn mov_reg_and_ret() {
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, mov x0, x1;)), 0xaa01_03e0);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, mov x9, x20;)), 0xaa14_03e9);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, ret;)), 0xd65f_03c0);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, ret x10;)), 0xd65f_0140);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, br x0;)), 0xd61f_0000);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, blr x5;)), 0xd63f_00a0);
}

#[test]
fn mov_imm_sequences() {
    assert_eq!(
        assemble(|j| monoasm_arm64!(&mut *j, mov x0, #0x1234;), 4),
        le(0xd282_4680)
    );
    assert_eq!(
        assemble(|j| monoasm_arm64!(&mut *j, mov x0, #0;), 4),
        le(0xd280_0000)
    );
    let bytes = assemble(
        |j| monoasm_arm64!(&mut *j, mov x0, (0x1234_5678_9abc_def0u64);),
        16,
    );
    let mut expected = Vec::new();
    expected.extend_from_slice(&le(0xd280_0000 | (0xdef0 << 5)));
    expected.extend_from_slice(&le(0xf2a0_0000 | (0x9abc << 5)));
    expected.extend_from_slice(&le(0xf2c0_0000 | (0x5678 << 5)));
    expected.extend_from_slice(&le(0xf2e0_0000 | (0x1234 << 5)));
    assert_eq!(bytes, expected);
}

#[test]
fn add_sub_immediate() {
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, add x0, x1, #16;)), 0x9100_4020);
    assert_eq!(
        word(|j| monoasm_arm64!(&mut *j, add x0, x1, #1, lsl #12;)),
        0x9140_0420
    );
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, adds x2, x3, #1;)), 0xb100_0462);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, sub x4, x5, #256;)), 0xd104_00a4);
    assert_eq!(
        word(|j| monoasm_arm64!(&mut *j, subs x6, x7, #4095;)),
        0xf13f_fce6
    );
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, cmp x8, #10;)), 0xf100_291f);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, cmn x9, #3;)), 0xb100_0d3f);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, add sp, sp, #32;)), 0x9100_83ff);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, sub sp, sp, #16;)), 0xd100_43ff);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, mov sp, x5;)), 0x9100_00bf);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, mov x3, sp;)), 0x9100_03e3);
}

#[test]
fn add_sub_register() {
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, add x0, x1, x2;)), 0x8b02_0020);
    assert_eq!(
        word(|j| monoasm_arm64!(&mut *j, add x0, x1, x2, lsl #3;)),
        0x8b02_0c20
    );
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, sub x10, x11, x12;)), 0xcb0c_016a);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, subs x13, x14, x15;)), 0xeb0f_01cd);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, cmp x16, x17;)), 0xeb11_021f);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, neg x18, x19;)), 0xcb13_03f2);
}

#[test]
fn logical_register() {
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, and x0, x1, x2;)), 0x8a02_0020);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, orr x3, x4, x5;)), 0xaa05_0083);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, eor x6, x7, x8;)), 0xca08_00e6);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, ands x9, x10, x11;)), 0xea0b_0149);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, mvn x12, x13;)), 0xaa2d_03ec);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, tst x14, x15;)), 0xea0f_01df);
    assert_eq!(
        word(|j| monoasm_arm64!(&mut *j, orr x0, x1, x2, lsl #4;)),
        0xaa02_1020
    );
}

#[test]
fn mul_div() {
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, mul x0, x1, x2;)), 0x9b02_7c20);
    assert_eq!(
        word(|j| monoasm_arm64!(&mut *j, madd x3, x4, x5, x6;)),
        0x9b05_1883
    );
    assert_eq!(
        word(|j| monoasm_arm64!(&mut *j, msub x7, x8, x9, x10;)),
        0x9b09_a907
    );
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, sdiv x11, x12, x13;)), 0x9acd_0d8b);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, udiv x14, x15, x16;)), 0x9ad0_09ee);
}

#[test]
fn shifts() {
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, lslv x0, x1, x2;)), 0x9ac2_2020);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, lsrv x3, x4, x5;)), 0x9ac5_2483);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, asrv x6, x7, x8;)), 0x9ac8_28e6);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, lsl x9, x10, #4;)), 0xd37c_ed49);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, lsr x11, x12, #8;)), 0xd348_fd8b);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, asr x13, x14, #2;)), 0x9342_fdcd);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, sxtw x15, x16;)), 0x9340_7e0f);
}

#[test]
fn conditional_select() {
    assert_eq!(
        word(|j| monoasm_arm64!(&mut *j, csel x0, x1, x2, eq;)),
        0x9a82_0020
    );
    assert_eq!(
        word(|j| monoasm_arm64!(&mut *j, csinc x3, x4, x5, ne;)),
        0x9a85_1483
    );
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, cset x6, gt;)), 0x9a9f_d7e6);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, csetm x7, lt;)), 0xda9f_a3e7);
}

#[test]
fn loads_stores() {
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, ldr x0, [x1, #16];)), 0xf940_0820);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, str x2, [x3, #4096];)), 0xf908_0062);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, ldr w4, [x5, #8];)), 0xb940_08a4);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, ldrb w8, [x9, #1];)), 0x3940_0528);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, ldrh w12, [x13, #2];)), 0x7940_05ac);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, ldrsw x16, [x17, #4];)), 0xb980_0630);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, ldr x0, [x1, #16]!;)), 0xf841_0c20);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, str x2, [x3, #-8]!;)), 0xf81f_8c62);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, ldr x4, [x5], #32;)), 0xf842_04a4);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, ldr x0, [x1, x2];)), 0xf862_6820);
    assert_eq!(
        word(|j| monoasm_arm64!(&mut *j, ldr x0, [x1, x2, lsl #3];)),
        0xf862_7820
    );
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, str x3, [x4, x5];)), 0xf825_6883);
}

#[test]
fn load_store_pair() {
    assert_eq!(
        word(|j| monoasm_arm64!(&mut *j, stp x0, x1, [sp, #-16]!;)),
        0xa9bf_07e0
    );
    assert_eq!(
        word(|j| monoasm_arm64!(&mut *j, ldp x2, x3, [sp], #16;)),
        0xa8c1_0fe2
    );
    assert_eq!(
        word(|j| monoasm_arm64!(&mut *j, stp x4, x5, [x6, #16];)),
        0xa901_14c4
    );
    assert_eq!(
        word(|j| monoasm_arm64!(&mut *j, ldp x7, x8, [x9, #-32];)),
        0xa97e_2127
    );
}

#[test]
fn floating_point() {
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, fmov d0, d1;)), 0x1e60_4020);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, fmov d2, x3;)), 0x9e67_0062);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, fmov x4, d5;)), 0x9e66_00a4);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, fadd d0, d1, d2;)), 0x1e62_2820);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, fsub d3, d4, d5;)), 0x1e65_3883);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, fmul d6, d7, d8;)), 0x1e68_08e6);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, fdiv d9, d10, d11;)), 0x1e6b_1949);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, fcmp d0, d1;)), 0x1e61_2000);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, fcmp d2, #0.0;)), 0x1e60_2048);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, scvtf d0, x1;)), 0x9e62_0020);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, fcvtzs x2, d3;)), 0x9e78_0062);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, ldr d0, [x1, #8];)), 0xfd40_0420);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, str d2, [x3, #16];)), 0xfd00_0862);
}

#[test]
fn system() {
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, nop;)), 0xd503_201f);
    assert_eq!(word(|j| monoasm_arm64!(&mut *j, brk #0;)), 0xd420_0000);
}

#[test]
fn branches_resolve() {
    // b .+8 (forward over a nop)
    let w = words(
        |j| {
            let l = j.label();
            monoasm_arm64!(&mut *j, b l; nop;);
            j.bind_label(l);
        },
        2,
    );
    assert_eq!(w[0], 0x1400_0002);

    // b .-4 (backward to a preceding nop)
    let w = words(
        |j| {
            let l = j.label();
            monoasm_arm64!(&mut *j, l: nop; b l;);
        },
        2,
    );
    assert_eq!(w[1], 0x17ff_ffff);

    // b.eq .+4
    let w = words(
        |j| {
            let l = j.label();
            monoasm_arm64!(&mut *j, b.eq l;);
            j.bind_label(l);
        },
        1,
    );
    assert_eq!(w[0], 0x5400_0020);

    // cbz x0, .+8
    let w = words(
        |j| {
            let l = j.label();
            monoasm_arm64!(&mut *j, cbz x0, l; nop;);
            j.bind_label(l);
        },
        2,
    );
    assert_eq!(w[0], 0xb400_0040);

    // cbnz x1, .+8
    let w = words(
        |j| {
            let l = j.label();
            monoasm_arm64!(&mut *j, cbnz x1, l; nop;);
            j.bind_label(l);
        },
        2,
    );
    assert_eq!(w[0], 0xb500_0041);

    // tbz x2, #3, .+8
    let w = words(
        |j| {
            let l = j.label();
            monoasm_arm64!(&mut *j, tbz x2, #3, l; nop;);
            j.bind_label(l);
        },
        2,
    );
    assert_eq!(w[0], 0x3618_0042);

    // adr x0, .+8
    let w = words(
        |j| {
            let l = j.label();
            monoasm_arm64!(&mut *j, adr x0, l; nop;);
            j.bind_label(l);
        },
        2,
    );
    assert_eq!(w[0], 0x1000_0040);
}
