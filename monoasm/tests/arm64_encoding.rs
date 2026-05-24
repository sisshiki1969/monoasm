//! Host-portable encoding tests for the AArch64 backend.
//!
//! These run on any host (they only *read* the emitted bytes from the
//! JIT page, they never execute them), so the encoders stay verified
//! even when the test suite runs natively on x86-64. Expected encodings
//! are taken from `llvm-mc --triple=aarch64` (instruction-level
//! `--show-encoding` plus object disassembly for resolved branches).

use monoasm::*;

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
    assert_eq!(assemble(|j| j.movz(X0, 0x1234, 0), 4), le(0xd282_4680));
    assert_eq!(assemble(|j| j.movz(X5, 0xabcd, 1), 4), le(0xd2b5_79a5));
    assert_eq!(assemble(|j| j.movz(X13, 0x1, 3), 4), le(0xd2e0_002d));
}

#[test]
fn movk_movn() {
    assert_eq!(assemble(|j| j.movk(X0, 0xffff, 2), 4), le(0xf2df_ffe0));
    assert_eq!(assemble(|j| j.movn(X7, 0, 0), 4), le(0x9280_0007));
    assert_eq!(assemble(|j| j.movn(X2, 0x10, 1), 4), le(0x92a0_0202));
}

#[test]
fn mov_reg_and_ret() {
    assert_eq!(word(|j| j.mov(X0, X1)), 0xaa01_03e0);
    assert_eq!(word(|j| j.mov(X9, X20)), 0xaa14_03e9);
    assert_eq!(word(|j| j.ret()), 0xd65f_03c0);
    assert_eq!(word(|j| j.ret_reg(X10)), 0xd65f_0140);
    assert_eq!(word(|j| j.br(X0)), 0xd61f_0000);
    assert_eq!(word(|j| j.blr(X5)), 0xd63f_00a0);
}

#[test]
fn mov_imm_sequences() {
    assert_eq!(assemble(|j| j.mov_imm(X0, 0x1234), 4), le(0xd282_4680));
    assert_eq!(assemble(|j| j.mov_imm(X0, 0), 4), le(0xd280_0000));
    let bytes = assemble(|j| j.mov_imm(X0, 0x1234_5678_9abc_def0), 16);
    let mut expected = Vec::new();
    expected.extend_from_slice(&le(0xd280_0000 | (0xdef0 << 5)));
    expected.extend_from_slice(&le(0xf2a0_0000 | (0x9abc << 5)));
    expected.extend_from_slice(&le(0xf2c0_0000 | (0x5678 << 5)));
    expected.extend_from_slice(&le(0xf2e0_0000 | (0x1234 << 5)));
    assert_eq!(bytes, expected);
}

#[test]
fn add_sub_immediate() {
    assert_eq!(word(|j| j.add_imm(X0, X1, 16, 0)), 0x9100_4020);
    assert_eq!(word(|j| j.add_imm(X0, X1, 1, 1)), 0x9140_0420);
    assert_eq!(word(|j| j.adds_imm(X2, X3, 1, 0)), 0xb100_0462);
    assert_eq!(word(|j| j.sub_imm(X4, X5, 256, 0)), 0xd104_00a4);
    assert_eq!(word(|j| j.subs_imm(X6, X7, 4095, 0)), 0xf13f_fce6);
    assert_eq!(word(|j| j.cmp_imm(X8, 10, 0)), 0xf100_291f);
    assert_eq!(word(|j| j.cmn_imm(X9, 3, 0)), 0xb100_0d3f);
    assert_eq!(word(|j| j.add_imm(SP, SP, 32, 0)), 0x9100_83ff);
    assert_eq!(word(|j| j.sub_imm(SP, SP, 16, 0)), 0xd100_43ff);
    assert_eq!(word(|j| j.mov_sp(SP, X5)), 0x9100_00bf);
    assert_eq!(word(|j| j.mov_sp(X3, SP)), 0x9100_03e3);
}

#[test]
fn add_sub_register() {
    assert_eq!(word(|j| j.add(X0, X1, X2)), 0x8b02_0020);
    assert_eq!(word(|j| j.add_lsl(X0, X1, X2, 3)), 0x8b02_0c20);
    assert_eq!(word(|j| j.sub(X10, X11, X12)), 0xcb0c_016a);
    assert_eq!(word(|j| j.subs(X13, X14, X15)), 0xeb0f_01cd);
    assert_eq!(word(|j| j.cmp(X16, X17)), 0xeb11_021f);
    assert_eq!(word(|j| j.neg(X18, X19)), 0xcb13_03f2);
}

#[test]
fn logical_register() {
    assert_eq!(word(|j| j.and_(X0, X1, X2)), 0x8a02_0020);
    assert_eq!(word(|j| j.orr(X3, X4, X5)), 0xaa05_0083);
    assert_eq!(word(|j| j.eor(X6, X7, X8)), 0xca08_00e6);
    assert_eq!(word(|j| j.ands(X9, X10, X11)), 0xea0b_0149);
    assert_eq!(word(|j| j.mvn(X12, X13)), 0xaa2d_03ec);
    assert_eq!(word(|j| j.tst(X14, X15)), 0xea0f_01df);
    assert_eq!(word(|j| j.orr_lsl(X0, X1, X2, 4)), 0xaa02_1020);
}

#[test]
fn mul_div() {
    assert_eq!(word(|j| j.mul(X0, X1, X2)), 0x9b02_7c20);
    assert_eq!(word(|j| j.madd(X3, X4, X5, X6)), 0x9b05_1883);
    assert_eq!(word(|j| j.msub(X7, X8, X9, X10)), 0x9b09_a907);
    assert_eq!(word(|j| j.sdiv(X11, X12, X13)), 0x9acd_0d8b);
    assert_eq!(word(|j| j.udiv(X14, X15, X16)), 0x9ad0_09ee);
}

#[test]
fn shifts() {
    assert_eq!(word(|j| j.lslv(X0, X1, X2)), 0x9ac2_2020);
    assert_eq!(word(|j| j.lsrv(X3, X4, X5)), 0x9ac5_2483);
    assert_eq!(word(|j| j.asrv(X6, X7, X8)), 0x9ac8_28e6);
    assert_eq!(word(|j| j.lsl_imm(X9, X10, 4)), 0xd37c_ed49);
    assert_eq!(word(|j| j.lsr_imm(X11, X12, 8)), 0xd348_fd8b);
    assert_eq!(word(|j| j.asr_imm(X13, X14, 2)), 0x9342_fdcd);
    assert_eq!(word(|j| j.sxtw(X15, X16)), 0x9340_7e0f);
}

#[test]
fn conditional_select() {
    assert_eq!(word(|j| j.csel(X0, X1, X2, Cond::Eq)), 0x9a82_0020);
    assert_eq!(word(|j| j.csinc(X3, X4, X5, Cond::Ne)), 0x9a85_1483);
    assert_eq!(word(|j| j.cset(X6, Cond::Gt)), 0x9a9f_d7e6);
    assert_eq!(word(|j| j.csetm(X7, Cond::Lt)), 0xda9f_a3e7);
}

#[test]
fn loads_stores() {
    assert_eq!(word(|j| j.ldr(X0, X1, 16)), 0xf940_0820);
    assert_eq!(word(|j| j.str(X2, X3, 4096)), 0xf908_0062);
    assert_eq!(word(|j| j.ldr32(X4, X5, 8)), 0xb940_08a4);
    assert_eq!(word(|j| j.ldrb(X8, X9, 1)), 0x3940_0528);
    assert_eq!(word(|j| j.ldrh(X12, X13, 2)), 0x7940_05ac);
    assert_eq!(word(|j| j.ldrsw(X16, X17, 4)), 0xb980_0630);
    assert_eq!(word(|j| j.ldr_pre(X0, X1, 16)), 0xf841_0c20);
    assert_eq!(word(|j| j.str_pre(X2, X3, -8)), 0xf81f_8c62);
    assert_eq!(word(|j| j.ldr_post(X4, X5, 32)), 0xf842_04a4);
    assert_eq!(word(|j| j.ldr_reg(X0, X1, X2, false)), 0xf862_6820);
    assert_eq!(word(|j| j.ldr_reg(X0, X1, X2, true)), 0xf862_7820);
    assert_eq!(word(|j| j.str_reg(X3, X4, X5, false)), 0xf825_6883);
}

#[test]
fn load_store_pair() {
    assert_eq!(word(|j| j.push_pair(X0, X1)), 0xa9bf_07e0);
    assert_eq!(word(|j| j.pop_pair(X2, X3)), 0xa8c1_0fe2);
    assert_eq!(word(|j| j.stp(X4, X5, X6, 16)), 0xa901_14c4);
    assert_eq!(word(|j| j.ldp(X7, X8, X9, -32)), 0xa97e_2127);
}

#[test]
fn floating_point() {
    assert_eq!(word(|j| j.fmov(D0, D1)), 0x1e60_4020);
    assert_eq!(word(|j| j.fmov_from_gpr(D2, X3)), 0x9e67_0062);
    assert_eq!(word(|j| j.fmov_to_gpr(X4, D5)), 0x9e66_00a4);
    assert_eq!(word(|j| j.fadd(D0, D1, D2)), 0x1e62_2820);
    assert_eq!(word(|j| j.fsub(D3, D4, D5)), 0x1e65_3883);
    assert_eq!(word(|j| j.fmul(D6, D7, D8)), 0x1e68_08e6);
    assert_eq!(word(|j| j.fdiv(D9, D10, D11)), 0x1e6b_1949);
    assert_eq!(word(|j| j.fcmp(D0, D1)), 0x1e61_2000);
    assert_eq!(word(|j| j.fcmp_zero(D2)), 0x1e60_2048);
    assert_eq!(word(|j| j.scvtf(D0, X1)), 0x9e62_0020);
    assert_eq!(word(|j| j.fcvtzs(X2, D3)), 0x9e78_0062);
    assert_eq!(word(|j| j.ldr_f(D0, X1, 8)), 0xfd40_0420);
    assert_eq!(word(|j| j.str_f(D2, X3, 16)), 0xfd00_0862);
}

#[test]
fn system() {
    assert_eq!(word(|j| j.nop()), 0xd503_201f);
    assert_eq!(word(|j| j.brk(0)), 0xd420_0000);
}

#[test]
fn branches_resolve() {
    // b .+8 (forward over a nop)
    let w = words(
        |j| {
            let l = j.label();
            j.b_label(&l);
            j.nop();
            j.bind_label(l);
        },
        2,
    );
    assert_eq!(w[0], 0x1400_0002);

    // b .-4 (backward to a preceding nop)
    let w = words(
        |j| {
            let l = j.label();
            j.bind_label(l.clone());
            j.nop();
            j.b_label(&l);
        },
        2,
    );
    assert_eq!(w[1], 0x17ff_ffff);

    // b.eq .+4
    let w = words(
        |j| {
            let l = j.label();
            j.bcond_label(Cond::Eq, &l);
            j.bind_label(l);
        },
        1,
    );
    assert_eq!(w[0], 0x5400_0020);

    // cbz x0, .+8
    let w = words(
        |j| {
            let l = j.label();
            j.cbz_label(X0, &l);
            j.nop();
            j.bind_label(l);
        },
        2,
    );
    assert_eq!(w[0], 0xb400_0040);

    // cbnz x1, .+8
    let w = words(
        |j| {
            let l = j.label();
            j.cbnz_label(X1, &l);
            j.nop();
            j.bind_label(l);
        },
        2,
    );
    assert_eq!(w[0], 0xb500_0041);

    // tbz x2, #3, .+8
    let w = words(
        |j| {
            let l = j.label();
            j.tbz_label(X2, 3, &l);
            j.nop();
            j.bind_label(l);
        },
        2,
    );
    assert_eq!(w[0], 0x3618_0042);

    // adr x0, .+8
    let w = words(
        |j| {
            let l = j.label();
            j.adr(X0, &l);
            j.nop();
            j.bind_label(l);
        },
        2,
    );
    assert_eq!(w[0], 0x1000_0040);
}
