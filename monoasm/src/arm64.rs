//! AArch64 (ARM64) instruction encoders for [`JitMemory`].
//!
//! monoasm was originally an x86-64 only assembler. This module adds an
//! AArch64 backend that reuses the architecture-neutral parts of
//! [`JitMemory`] (executable pages, labels, relocations, the `emit*`
//! helpers) and only contributes the A64 instruction encodings.
//!
//! A64 instructions are all 32 bits wide and little-endian, so each
//! encoder simply computes a `u32` and emits it via
//! [`JitMemory::emitl`]. Encodings follow the Arm Architecture Reference
//! Manual (A64 ISA) and are cross-checked against
//! `llvm-mc --triple=aarch64 --show-encoding`.

use crate::{DestLabel, JitMemory};

/// An AArch64 general-purpose register.
///
/// Index `0..=30` selects `X0`..`X30`. Index `31` denotes either the
/// zero register (`XZR`) or the stack pointer (`SP`) depending on the
/// instruction; use the [`XZR`] / [`SP`] constants to make the intent
/// explicit at the call site.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct GReg(pub u32);

macro_rules! gregs {
    ($($name:ident = $n:expr),* $(,)?) => {
        $(
            #[doc = concat!("General-purpose register `X", stringify!($n), "`.")]
            pub const $name: GReg = GReg($n);
        )*
    };
}

gregs! {
    X0 = 0, X1 = 1, X2 = 2, X3 = 3, X4 = 4, X5 = 5, X6 = 6, X7 = 7,
    X8 = 8, X9 = 9, X10 = 10, X11 = 11, X12 = 12, X13 = 13, X14 = 14, X15 = 15,
    X16 = 16, X17 = 17, X18 = 18, X19 = 19, X20 = 20, X21 = 21, X22 = 22, X23 = 23,
    X24 = 24, X25 = 25, X26 = 26, X27 = 27, X28 = 28, X29 = 29, X30 = 30,
}

/// Frame pointer (`X29`).
pub const FP: GReg = GReg(29);
/// Link register (`X30`).
pub const LR: GReg = GReg(30);
/// Zero register: reads as `0`, writes are discarded (register index 31).
pub const XZR: GReg = GReg(31);
/// Stack pointer (register index 31 in SP-interpreting instructions).
pub const SP: GReg = GReg(31);

impl GReg {
    #[inline]
    fn enc(self) -> u32 {
        debug_assert!(self.0 < 32, "invalid AArch64 register index {}", self.0);
        self.0
    }
}

/// An AArch64 SIMD&FP register used in scalar double-precision (`Dn`)
/// form.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct FReg(pub u32);

macro_rules! fregs {
    ($($name:ident = $n:expr),* $(,)?) => {
        $(
            #[doc = concat!("Scalar double-precision register `D", stringify!($n), "`.")]
            pub const $name: FReg = FReg($n);
        )*
    };
}

fregs! {
    D0 = 0, D1 = 1, D2 = 2, D3 = 3, D4 = 4, D5 = 5, D6 = 6, D7 = 7,
    D8 = 8, D9 = 9, D10 = 10, D11 = 11, D12 = 12, D13 = 13, D14 = 14, D15 = 15,
    D16 = 16, D17 = 17, D18 = 18, D19 = 19, D20 = 20, D21 = 21, D22 = 22, D23 = 23,
    D24 = 24, D25 = 25, D26 = 26, D27 = 27, D28 = 28, D29 = 29, D30 = 30, D31 = 31,
}

impl FReg {
    #[inline]
    fn enc(self) -> u32 {
        debug_assert!(self.0 < 32, "invalid AArch64 FP register index {}", self.0);
        self.0
    }
}

/// AArch64 condition codes (the 4-bit `cond` field).
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
#[repr(u32)]
pub enum Cond {
    /// Equal (`Z == 1`).
    Eq = 0,
    /// Not equal (`Z == 0`).
    Ne = 1,
    /// Unsigned higher or same / carry set (`C == 1`).
    Hs = 2,
    /// Unsigned lower / carry clear (`C == 0`).
    Lo = 3,
    /// Negative (`N == 1`).
    Mi = 4,
    /// Positive or zero (`N == 0`).
    Pl = 5,
    /// Overflow (`V == 1`).
    Vs = 6,
    /// No overflow (`V == 0`).
    Vc = 7,
    /// Unsigned higher.
    Hi = 8,
    /// Unsigned lower or same.
    Ls = 9,
    /// Signed greater than or equal.
    Ge = 10,
    /// Signed less than.
    Lt = 11,
    /// Signed greater than.
    Gt = 12,
    /// Signed less than or equal.
    Le = 13,
    /// Always.
    Al = 14,
}

impl Cond {
    /// Carry set (alias of [`Cond::Hs`]).
    pub const CS: Cond = Cond::Hs;
    /// Carry clear (alias of [`Cond::Lo`]).
    pub const CC: Cond = Cond::Lo;

    #[inline]
    fn enc(self) -> u32 {
        self as u32
    }

    /// The inverted condition (`EQ`↔`NE`, `LT`↔`GE`, …). Used to
    /// synthesize `CSET`/`CSETM`.
    #[inline]
    pub fn invert(self) -> Cond {
        // Inverting a condition flips the low bit of the encoding.
        match self {
            Cond::Eq => Cond::Ne,
            Cond::Ne => Cond::Eq,
            Cond::Hs => Cond::Lo,
            Cond::Lo => Cond::Hs,
            Cond::Mi => Cond::Pl,
            Cond::Pl => Cond::Mi,
            Cond::Vs => Cond::Vc,
            Cond::Vc => Cond::Vs,
            Cond::Hi => Cond::Ls,
            Cond::Ls => Cond::Hi,
            Cond::Ge => Cond::Lt,
            Cond::Lt => Cond::Ge,
            Cond::Gt => Cond::Le,
            Cond::Le => Cond::Gt,
            Cond::Al => Cond::Al,
        }
    }
}

/// Kind of AArch64 PC-relative relocation, distinguishing how a branch
/// displacement is scaled and packed into the instruction word. Resolved
/// by `JitMemory`'s relocation engine when the target [`DestLabel`] is
/// bound.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Arm64Reloc {
    /// `imm26` at bits `[25:0]`, scaled by 4 (`B`, `BL`).
    B26,
    /// `imm19` at bits `[23:5]`, scaled by 4 (`B.cond`, `CBZ`, `CBNZ`).
    Imm19,
    /// `imm14` at bits `[18:5]`, scaled by 4 (`TBZ`, `TBNZ`).
    Test14,
    /// `imm21` split across `[30:29]` (low 2) and `[23:5]` (high 19),
    /// byte-granular (`ADR`).
    Adr,
}

impl Arm64Reloc {
    /// Patch the PC-relative displacement `disp` (in bytes, target minus
    /// the address of this instruction) into the already-emitted
    /// instruction `word`.
    pub fn patch(self, word: u32, disp: i64) -> u32 {
        match self {
            Arm64Reloc::B26 => {
                assert_eq!(disp & 0b11, 0, "B/BL target not 4-byte aligned");
                let imm = disp >> 2;
                assert!(
                    (-(1 << 25)..(1 << 25)).contains(&imm),
                    "B/BL displacement out of range"
                );
                word | (imm as u32 & 0x03ff_ffff)
            }
            Arm64Reloc::Imm19 => {
                assert_eq!(
                    disp & 0b11,
                    0,
                    "conditional branch target not 4-byte aligned"
                );
                let imm = disp >> 2;
                assert!(
                    (-(1 << 18)..(1 << 18)).contains(&imm),
                    "conditional branch displacement out of range"
                );
                word | ((imm as u32 & 0x7_ffff) << 5)
            }
            Arm64Reloc::Test14 => {
                assert_eq!(disp & 0b11, 0, "test branch target not 4-byte aligned");
                let imm = disp >> 2;
                assert!(
                    (-(1 << 13)..(1 << 13)).contains(&imm),
                    "test branch displacement out of range"
                );
                word | ((imm as u32 & 0x3fff) << 5)
            }
            Arm64Reloc::Adr => {
                assert!(
                    (-(1 << 20)..(1 << 20)).contains(&disp),
                    "ADR displacement out of range"
                );
                let imm = disp as u32 & 0x1f_ffff;
                let immlo = imm & 0b11;
                let immhi = imm >> 2;
                word | (immlo << 29) | (immhi << 5)
            }
        }
    }
}

impl JitMemory {
    // ===================================================================
    // MOV (wide immediate) family
    // ===================================================================

    /// `MOVZ Xd, #imm16, LSL #(16 * hw)` — move zero-extended 16-bit
    /// immediate into a cleared register. `hw` selects the halfword
    /// position (`0..=3`).
    pub fn movz(&mut self, rd: GReg, imm16: u16, hw: u32) {
        debug_assert!(hw < 4, "movz: hw out of range");
        self.emitl(0xd280_0000 | (hw << 21) | ((imm16 as u32) << 5) | rd.enc());
    }

    /// `MOVN Xd, #imm16, LSL #(16 * hw)` — move bitwise-NOT of the
    /// zero-extended 16-bit immediate.
    pub fn movn(&mut self, rd: GReg, imm16: u16, hw: u32) {
        debug_assert!(hw < 4, "movn: hw out of range");
        self.emitl(0x9280_0000 | (hw << 21) | ((imm16 as u32) << 5) | rd.enc());
    }

    /// `MOVK Xd, #imm16, LSL #(16 * hw)` — keep the other bits and set
    /// one 16-bit halfword.
    pub fn movk(&mut self, rd: GReg, imm16: u16, hw: u32) {
        debug_assert!(hw < 4, "movk: hw out of range");
        self.emitl(0xf280_0000 | (hw << 21) | ((imm16 as u32) << 5) | rd.enc());
    }

    /// `MOV Xd, Xm` (register-to-register copy), encoded as the
    /// canonical alias `ORR Xd, XZR, Xm`.
    pub fn mov(&mut self, rd: GReg, rm: GReg) {
        self.emitl(0xaa00_0000 | (rm.enc() << 16) | (XZR.enc() << 5) | rd.enc());
    }

    /// `MOV Xd|SP, Xn|SP` involving the stack pointer, encoded as
    /// `ADD Xd, Xn, #0`.
    pub fn mov_sp(&mut self, rd: GReg, rn: GReg) {
        self.add_imm(rd, rn, 0, 0);
    }

    /// Materialize an arbitrary 64-bit immediate into `rd` using a
    /// `MOVZ` followed by `MOVK` for each remaining non-zero halfword
    /// (1–4 instructions).
    pub fn mov_imm(&mut self, rd: GReg, imm: u64) {
        let hw = [
            (imm & 0xffff) as u16,
            ((imm >> 16) & 0xffff) as u16,
            ((imm >> 32) & 0xffff) as u16,
            ((imm >> 48) & 0xffff) as u16,
        ];
        self.movz(rd, hw[0], 0);
        for (i, &h) in hw.iter().enumerate().skip(1) {
            if h != 0 {
                self.movk(rd, h, i as u32);
            }
        }
    }

    // ===================================================================
    // Add / subtract — immediate (12-bit, optional LSL #12)
    // ===================================================================

    fn addsub_imm(&mut self, base: u32, rd: GReg, rn: GReg, imm12: u32, shift12: u32) {
        debug_assert!(imm12 < (1 << 12), "addsub_imm: imm12 out of range");
        debug_assert!(shift12 < 2, "addsub_imm: shift must be 0 or 1");
        self.emitl(base | (shift12 << 22) | (imm12 << 10) | (rn.enc() << 5) | rd.enc());
    }

    /// `ADD Xd|SP, Xn|SP, #imm12 {, LSL #12}`.
    pub fn add_imm(&mut self, rd: GReg, rn: GReg, imm12: u32, shift12: u32) {
        self.addsub_imm(0x9100_0000, rd, rn, imm12, shift12);
    }

    /// `ADDS Xd, Xn|SP, #imm12 {, LSL #12}` (sets flags).
    pub fn adds_imm(&mut self, rd: GReg, rn: GReg, imm12: u32, shift12: u32) {
        self.addsub_imm(0xb100_0000, rd, rn, imm12, shift12);
    }

    /// `SUB Xd|SP, Xn|SP, #imm12 {, LSL #12}`.
    pub fn sub_imm(&mut self, rd: GReg, rn: GReg, imm12: u32, shift12: u32) {
        self.addsub_imm(0xd100_0000, rd, rn, imm12, shift12);
    }

    /// `SUBS Xd, Xn|SP, #imm12 {, LSL #12}` (sets flags).
    pub fn subs_imm(&mut self, rd: GReg, rn: GReg, imm12: u32, shift12: u32) {
        self.addsub_imm(0xf100_0000, rd, rn, imm12, shift12);
    }

    /// `CMP Xn|SP, #imm12 {, LSL #12}` — alias of `SUBS XZR, Xn, #imm`.
    pub fn cmp_imm(&mut self, rn: GReg, imm12: u32, shift12: u32) {
        self.subs_imm(XZR, rn, imm12, shift12);
    }

    /// `CMN Xn|SP, #imm12 {, LSL #12}` — alias of `ADDS XZR, Xn, #imm`.
    pub fn cmn_imm(&mut self, rn: GReg, imm12: u32, shift12: u32) {
        self.adds_imm(XZR, rn, imm12, shift12);
    }

    // ===================================================================
    // Add / subtract — shifted register (LSL amount in `shift`)
    // ===================================================================

    fn addsub_reg(&mut self, base: u32, rd: GReg, rn: GReg, rm: GReg, shift: u32) {
        debug_assert!(shift < 64, "addsub_reg: LSL amount out of range");
        self.emitl(base | (rm.enc() << 16) | (shift << 10) | (rn.enc() << 5) | rd.enc());
    }

    /// `ADD Xd, Xn, Xm` (LSL #0).
    pub fn add(&mut self, rd: GReg, rn: GReg, rm: GReg) {
        self.addsub_reg(0x8b00_0000, rd, rn, rm, 0);
    }

    /// `ADD Xd, Xn, Xm, LSL #shift`.
    pub fn add_lsl(&mut self, rd: GReg, rn: GReg, rm: GReg, shift: u32) {
        self.addsub_reg(0x8b00_0000, rd, rn, rm, shift);
    }

    /// `ADDS Xd, Xn, Xm` (LSL #0, sets flags).
    pub fn adds(&mut self, rd: GReg, rn: GReg, rm: GReg) {
        self.addsub_reg(0xab00_0000, rd, rn, rm, 0);
    }

    /// `SUB Xd, Xn, Xm` (LSL #0).
    pub fn sub(&mut self, rd: GReg, rn: GReg, rm: GReg) {
        self.addsub_reg(0xcb00_0000, rd, rn, rm, 0);
    }

    /// `SUBS Xd, Xn, Xm` (LSL #0, sets flags).
    pub fn subs(&mut self, rd: GReg, rn: GReg, rm: GReg) {
        self.addsub_reg(0xeb00_0000, rd, rn, rm, 0);
    }

    /// `CMP Xn, Xm` — alias of `SUBS XZR, Xn, Xm`.
    pub fn cmp(&mut self, rn: GReg, rm: GReg) {
        self.subs(XZR, rn, rm);
    }

    /// `CMN Xn, Xm` — alias of `ADDS XZR, Xn, Xm`.
    pub fn cmn(&mut self, rn: GReg, rm: GReg) {
        self.adds(XZR, rn, rm);
    }

    /// `NEG Xd, Xm` — alias of `SUB Xd, XZR, Xm`.
    pub fn neg(&mut self, rd: GReg, rm: GReg) {
        self.sub(rd, XZR, rm);
    }

    // ===================================================================
    // Logical — shifted register
    // ===================================================================

    fn logical_reg(&mut self, base: u32, rd: GReg, rn: GReg, rm: GReg, shift: u32) {
        debug_assert!(shift < 64, "logical_reg: LSL amount out of range");
        self.emitl(base | (rm.enc() << 16) | (shift << 10) | (rn.enc() << 5) | rd.enc());
    }

    /// `AND Xd, Xn, Xm`.
    pub fn and_(&mut self, rd: GReg, rn: GReg, rm: GReg) {
        self.logical_reg(0x8a00_0000, rd, rn, rm, 0);
    }

    /// `ORR Xd, Xn, Xm`.
    pub fn orr(&mut self, rd: GReg, rn: GReg, rm: GReg) {
        self.logical_reg(0xaa00_0000, rd, rn, rm, 0);
    }

    /// `ORR Xd, Xn, Xm, LSL #shift`.
    pub fn orr_lsl(&mut self, rd: GReg, rn: GReg, rm: GReg, shift: u32) {
        self.logical_reg(0xaa00_0000, rd, rn, rm, shift);
    }

    /// `EOR Xd, Xn, Xm`.
    pub fn eor(&mut self, rd: GReg, rn: GReg, rm: GReg) {
        self.logical_reg(0xca00_0000, rd, rn, rm, 0);
    }

    /// `ANDS Xd, Xn, Xm` (sets flags).
    pub fn ands(&mut self, rd: GReg, rn: GReg, rm: GReg) {
        self.logical_reg(0xea00_0000, rd, rn, rm, 0);
    }

    /// `MVN Xd, Xm` — alias of `ORN Xd, XZR, Xm`.
    pub fn mvn(&mut self, rd: GReg, rm: GReg) {
        // ORN (shifted register): ORR base with the N bit (bit 21) set.
        self.logical_reg(0xaa20_0000, rd, XZR, rm, 0);
    }

    /// `TST Xn, Xm` — alias of `ANDS XZR, Xn, Xm`.
    pub fn tst(&mut self, rn: GReg, rm: GReg) {
        self.ands(XZR, rn, rm);
    }

    // ===================================================================
    // Multiply / divide
    // ===================================================================

    /// `MADD Xd, Xn, Xm, Xa` (`Xd = Xa + Xn * Xm`).
    pub fn madd(&mut self, rd: GReg, rn: GReg, rm: GReg, ra: GReg) {
        self.emitl(0x9b00_0000 | (rm.enc() << 16) | (ra.enc() << 10) | (rn.enc() << 5) | rd.enc());
    }

    /// `MSUB Xd, Xn, Xm, Xa` (`Xd = Xa - Xn * Xm`).
    pub fn msub(&mut self, rd: GReg, rn: GReg, rm: GReg, ra: GReg) {
        self.emitl(0x9b00_8000 | (rm.enc() << 16) | (ra.enc() << 10) | (rn.enc() << 5) | rd.enc());
    }

    /// `MUL Xd, Xn, Xm` — alias of `MADD Xd, Xn, Xm, XZR`.
    pub fn mul(&mut self, rd: GReg, rn: GReg, rm: GReg) {
        self.madd(rd, rn, rm, XZR);
    }

    /// `SDIV Xd, Xn, Xm` (signed division).
    pub fn sdiv(&mut self, rd: GReg, rn: GReg, rm: GReg) {
        self.emitl(0x9ac0_0c00 | (rm.enc() << 16) | (rn.enc() << 5) | rd.enc());
    }

    /// `UDIV Xd, Xn, Xm` (unsigned division).
    pub fn udiv(&mut self, rd: GReg, rn: GReg, rm: GReg) {
        self.emitl(0x9ac0_0800 | (rm.enc() << 16) | (rn.enc() << 5) | rd.enc());
    }

    // ===================================================================
    // Shifts
    // ===================================================================

    /// `LSLV Xd, Xn, Xm` (variable logical shift left).
    pub fn lslv(&mut self, rd: GReg, rn: GReg, rm: GReg) {
        self.emitl(0x9ac0_2000 | (rm.enc() << 16) | (rn.enc() << 5) | rd.enc());
    }

    /// `LSRV Xd, Xn, Xm` (variable logical shift right).
    pub fn lsrv(&mut self, rd: GReg, rn: GReg, rm: GReg) {
        self.emitl(0x9ac0_2400 | (rm.enc() << 16) | (rn.enc() << 5) | rd.enc());
    }

    /// `ASRV Xd, Xn, Xm` (variable arithmetic shift right).
    pub fn asrv(&mut self, rd: GReg, rn: GReg, rm: GReg) {
        self.emitl(0x9ac0_2800 | (rm.enc() << 16) | (rn.enc() << 5) | rd.enc());
    }

    fn bfm(&mut self, base: u32, rd: GReg, rn: GReg, immr: u32, imms: u32) {
        // 64-bit bitfield ops set the N bit (bit 22), already folded into
        // the supplied `base`.
        self.emitl(base | (immr << 16) | (imms << 10) | (rn.enc() << 5) | rd.enc());
    }

    /// `LSL Xd, Xn, #shift` — alias of `UBFM`.
    pub fn lsl_imm(&mut self, rd: GReg, rn: GReg, shift: u32) {
        debug_assert!(shift < 64, "lsl_imm: shift out of range");
        self.bfm(0xd340_0000, rd, rn, (64 - shift) & 63, 63 - shift);
    }

    /// `LSR Xd, Xn, #shift` — alias of `UBFM`.
    pub fn lsr_imm(&mut self, rd: GReg, rn: GReg, shift: u32) {
        debug_assert!(shift < 64, "lsr_imm: shift out of range");
        self.bfm(0xd340_0000, rd, rn, shift, 63);
    }

    /// `ASR Xd, Xn, #shift` — alias of `SBFM`.
    pub fn asr_imm(&mut self, rd: GReg, rn: GReg, shift: u32) {
        debug_assert!(shift < 64, "asr_imm: shift out of range");
        self.bfm(0x9340_0000, rd, rn, shift, 63);
    }

    /// `SXTW Xd, Wn` — sign-extend a 32-bit value (`SBFM Xd, Xn, #0, #31`).
    pub fn sxtw(&mut self, rd: GReg, rn: GReg) {
        self.bfm(0x9340_0000, rd, rn, 0, 31);
    }

    // ===================================================================
    // Conditional select
    // ===================================================================

    /// `CSEL Xd, Xn, Xm, cond`.
    pub fn csel(&mut self, rd: GReg, rn: GReg, rm: GReg, cond: Cond) {
        self.emitl(
            0x9a80_0000 | (rm.enc() << 16) | (cond.enc() << 12) | (rn.enc() << 5) | rd.enc(),
        );
    }

    /// `CSINC Xd, Xn, Xm, cond`.
    pub fn csinc(&mut self, rd: GReg, rn: GReg, rm: GReg, cond: Cond) {
        self.emitl(
            0x9a80_0400 | (rm.enc() << 16) | (cond.enc() << 12) | (rn.enc() << 5) | rd.enc(),
        );
    }

    /// `CSET Xd, cond` — set `Xd` to 1 if `cond` holds, else 0
    /// (`CSINC Xd, XZR, XZR, invert(cond)`).
    pub fn cset(&mut self, rd: GReg, cond: Cond) {
        self.csinc(rd, XZR, XZR, cond.invert());
    }

    /// `CSETM Xd, cond` — set `Xd` to all-ones if `cond` holds, else 0
    /// (`CSINV Xd, XZR, XZR, invert(cond)`).
    pub fn csetm(&mut self, rd: GReg, cond: Cond) {
        let c = cond.invert();
        // CSINV base.
        self.emitl(0xda80_0000 | (XZR.enc() << 16) | (c.enc() << 12) | (XZR.enc() << 5) | rd.enc());
    }

    // ===================================================================
    // Load / store — immediate (unsigned scaled offset)
    // ===================================================================

    fn ldst_uimm(&mut self, base: u32, scale: u32, rt: u32, rn: GReg, byte_off: u32) {
        debug_assert!(
            byte_off & ((1 << scale) - 1) == 0,
            "load/store offset misaligned"
        );
        let imm12 = byte_off >> scale;
        debug_assert!(imm12 < (1 << 12), "load/store offset out of range");
        self.emitl(base | (imm12 << 10) | (rn.enc() << 5) | rt);
    }

    /// `LDR Xt, [Xn|SP, #off]` (off scaled by 8).
    pub fn ldr(&mut self, rt: GReg, rn: GReg, off: u32) {
        self.ldst_uimm(0xf940_0000, 3, rt.enc(), rn, off);
    }

    /// `STR Xt, [Xn|SP, #off]` (off scaled by 8).
    pub fn str(&mut self, rt: GReg, rn: GReg, off: u32) {
        self.ldst_uimm(0xf900_0000, 3, rt.enc(), rn, off);
    }

    /// `LDR Wt, [Xn|SP, #off]` (32-bit, off scaled by 4).
    pub fn ldr32(&mut self, rt: GReg, rn: GReg, off: u32) {
        self.ldst_uimm(0xb940_0000, 2, rt.enc(), rn, off);
    }

    /// `STR Wt, [Xn|SP, #off]` (32-bit, off scaled by 4).
    pub fn str32(&mut self, rt: GReg, rn: GReg, off: u32) {
        self.ldst_uimm(0xb900_0000, 2, rt.enc(), rn, off);
    }

    /// `LDRB Wt, [Xn|SP, #off]` (byte, unscaled).
    pub fn ldrb(&mut self, rt: GReg, rn: GReg, off: u32) {
        self.ldst_uimm(0x3940_0000, 0, rt.enc(), rn, off);
    }

    /// `STRB Wt, [Xn|SP, #off]` (byte, unscaled).
    pub fn strb(&mut self, rt: GReg, rn: GReg, off: u32) {
        self.ldst_uimm(0x3900_0000, 0, rt.enc(), rn, off);
    }

    /// `LDRH Wt, [Xn|SP, #off]` (halfword, off scaled by 2).
    pub fn ldrh(&mut self, rt: GReg, rn: GReg, off: u32) {
        self.ldst_uimm(0x7940_0000, 1, rt.enc(), rn, off);
    }

    /// `STRH Wt, [Xn|SP, #off]` (halfword, off scaled by 2).
    pub fn strh(&mut self, rt: GReg, rn: GReg, off: u32) {
        self.ldst_uimm(0x7900_0000, 1, rt.enc(), rn, off);
    }

    /// `LDRSW Xt, [Xn|SP, #off]` (load 32-bit, sign-extend; off scaled by 4).
    pub fn ldrsw(&mut self, rt: GReg, rn: GReg, off: u32) {
        self.ldst_uimm(0xb980_0000, 2, rt.enc(), rn, off);
    }

    // ---- pre/post-indexed (9-bit signed, unscaled) ----

    fn ldst_idx(&mut self, base: u32, rt: u32, rn: GReg, imm9: i32) {
        debug_assert!(
            (-256..256).contains(&imm9),
            "pre/post-index imm out of range"
        );
        self.emitl(base | (((imm9 as u32) & 0x1ff) << 12) | (rn.enc() << 5) | rt);
    }

    /// `LDR Xt, [Xn|SP, #imm]!` (pre-indexed).
    pub fn ldr_pre(&mut self, rt: GReg, rn: GReg, imm9: i32) {
        self.ldst_idx(0xf840_0c00, rt.enc(), rn, imm9);
    }

    /// `LDR Xt, [Xn|SP], #imm` (post-indexed).
    pub fn ldr_post(&mut self, rt: GReg, rn: GReg, imm9: i32) {
        self.ldst_idx(0xf840_0400, rt.enc(), rn, imm9);
    }

    /// `STR Xt, [Xn|SP, #imm]!` (pre-indexed).
    pub fn str_pre(&mut self, rt: GReg, rn: GReg, imm9: i32) {
        self.ldst_idx(0xf800_0c00, rt.enc(), rn, imm9);
    }

    /// `STR Xt, [Xn|SP], #imm` (post-indexed).
    pub fn str_post(&mut self, rt: GReg, rn: GReg, imm9: i32) {
        self.ldst_idx(0xf800_0400, rt.enc(), rn, imm9);
    }

    // ---- register offset ----

    /// `LDR Xt, [Xn|SP, Xm {, LSL #3}]`. `scaled` selects `LSL #3`.
    pub fn ldr_reg(&mut self, rt: GReg, rn: GReg, rm: GReg, scaled: bool) {
        let s = if scaled { 1 } else { 0 };
        // option = 011 (LSL/UXTX).
        self.emitl(0xf860_6800 | (rm.enc() << 16) | (s << 12) | (rn.enc() << 5) | rt.enc());
    }

    /// `STR Xt, [Xn|SP, Xm {, LSL #3}]`. `scaled` selects `LSL #3`.
    pub fn str_reg(&mut self, rt: GReg, rn: GReg, rm: GReg, scaled: bool) {
        let s = if scaled { 1 } else { 0 };
        self.emitl(0xf820_6800 | (rm.enc() << 16) | (s << 12) | (rn.enc() << 5) | rt.enc());
    }

    // ===================================================================
    // Load / store pair (64-bit, signed 7-bit offset scaled by 8)
    // ===================================================================

    fn ldstp(&mut self, base: u32, rt: GReg, rt2: GReg, rn: GReg, byte_off: i32) {
        debug_assert!(byte_off % 8 == 0, "ldp/stp offset not a multiple of 8");
        let imm7 = byte_off / 8;
        debug_assert!((-64..64).contains(&imm7), "ldp/stp offset out of range");
        self.emitl(
            base | (((imm7 as u32) & 0x7f) << 15) | (rt2.enc() << 10) | (rn.enc() << 5) | rt.enc(),
        );
    }

    /// `STP Xt, Xt2, [Xn|SP, #off]`.
    pub fn stp(&mut self, rt: GReg, rt2: GReg, rn: GReg, off: i32) {
        self.ldstp(0xa900_0000, rt, rt2, rn, off);
    }

    /// `LDP Xt, Xt2, [Xn|SP, #off]`.
    pub fn ldp(&mut self, rt: GReg, rt2: GReg, rn: GReg, off: i32) {
        self.ldstp(0xa940_0000, rt, rt2, rn, off);
    }

    /// `STP Xt, Xt2, [Xn|SP, #off]!` (pre-indexed).
    pub fn stp_pre(&mut self, rt: GReg, rt2: GReg, rn: GReg, off: i32) {
        self.ldstp(0xa980_0000, rt, rt2, rn, off);
    }

    /// `LDP Xt, Xt2, [Xn|SP], #off` (post-indexed).
    pub fn ldp_post(&mut self, rt: GReg, rt2: GReg, rn: GReg, off: i32) {
        self.ldstp(0xa8c0_0000, rt, rt2, rn, off);
    }

    /// Push a register pair: `STP Xa, Xb, [SP, #-16]!`.
    pub fn push_pair(&mut self, ra: GReg, rb: GReg) {
        self.stp_pre(ra, rb, SP, -16);
    }

    /// Pop a register pair: `LDP Xa, Xb, [SP], #16`.
    pub fn pop_pair(&mut self, ra: GReg, rb: GReg) {
        self.ldp_post(ra, rb, SP, 16);
    }

    // ===================================================================
    // Floating-point (scalar double precision)
    // ===================================================================

    /// `FMOV Dd, Dn` (register copy).
    pub fn fmov(&mut self, rd: FReg, rn: FReg) {
        self.emitl(0x1e60_4000 | (rn.enc() << 5) | rd.enc());
    }

    /// `FMOV Dd, Xn` (move 64-bit GPR bits into a double).
    pub fn fmov_from_gpr(&mut self, rd: FReg, rn: GReg) {
        self.emitl(0x9e67_0000 | (rn.enc() << 5) | rd.enc());
    }

    /// `FMOV Xd, Dn` (move double bits into a 64-bit GPR).
    pub fn fmov_to_gpr(&mut self, rd: GReg, rn: FReg) {
        self.emitl(0x9e66_0000 | (rn.enc() << 5) | rd.enc());
    }

    /// `FADD Dd, Dn, Dm`.
    pub fn fadd(&mut self, rd: FReg, rn: FReg, rm: FReg) {
        self.emitl(0x1e60_2800 | (rm.enc() << 16) | (rn.enc() << 5) | rd.enc());
    }

    /// `FSUB Dd, Dn, Dm`.
    pub fn fsub(&mut self, rd: FReg, rn: FReg, rm: FReg) {
        self.emitl(0x1e60_3800 | (rm.enc() << 16) | (rn.enc() << 5) | rd.enc());
    }

    /// `FMUL Dd, Dn, Dm`.
    pub fn fmul(&mut self, rd: FReg, rn: FReg, rm: FReg) {
        self.emitl(0x1e60_0800 | (rm.enc() << 16) | (rn.enc() << 5) | rd.enc());
    }

    /// `FDIV Dd, Dn, Dm`.
    pub fn fdiv(&mut self, rd: FReg, rn: FReg, rm: FReg) {
        self.emitl(0x1e60_1800 | (rm.enc() << 16) | (rn.enc() << 5) | rd.enc());
    }

    /// `FCMP Dn, Dm`.
    pub fn fcmp(&mut self, rn: FReg, rm: FReg) {
        self.emitl(0x1e60_2000 | (rm.enc() << 16) | (rn.enc() << 5));
    }

    /// `FCMP Dn, #0.0`.
    pub fn fcmp_zero(&mut self, rn: FReg) {
        self.emitl(0x1e60_2008 | (rn.enc() << 5));
    }

    /// `SCVTF Dd, Xn` (signed 64-bit integer → double).
    pub fn scvtf(&mut self, rd: FReg, rn: GReg) {
        self.emitl(0x9e62_0000 | (rn.enc() << 5) | rd.enc());
    }

    /// `FCVTZS Xd, Dn` (double → signed 64-bit integer, round toward zero).
    pub fn fcvtzs(&mut self, rd: GReg, rn: FReg) {
        self.emitl(0x9e78_0000 | (rn.enc() << 5) | rd.enc());
    }

    /// `LDR Dt, [Xn|SP, #off]` (off scaled by 8).
    pub fn ldr_f(&mut self, rt: FReg, rn: GReg, off: u32) {
        self.ldst_uimm(0xfd40_0000, 3, rt.enc(), rn, off);
    }

    /// `STR Dt, [Xn|SP, #off]` (off scaled by 8).
    pub fn str_f(&mut self, rt: FReg, rn: GReg, off: u32) {
        self.ldst_uimm(0xfd00_0000, 3, rt.enc(), rn, off);
    }

    // ===================================================================
    // Branches
    // ===================================================================

    /// `RET Xn` — return to the address held in `rn`.
    pub fn ret_reg(&mut self, rn: GReg) {
        self.emitl(0xd65f_0000 | (rn.enc() << 5));
    }

    /// `RET` — return to the address held in the link register (`X30`).
    pub fn ret(&mut self) {
        self.ret_reg(LR);
    }

    /// `BR Xn` — branch to the address in `rn`.
    pub fn br(&mut self, rn: GReg) {
        self.emitl(0xd61f_0000 | (rn.enc() << 5));
    }

    /// `BLR Xn` — branch with link to the address in `rn`.
    pub fn blr(&mut self, rn: GReg) {
        self.emitl(0xd63f_0000 | (rn.enc() << 5));
    }

    /// `B label` — unconditional branch to a label.
    pub fn b_label(&mut self, label: &DestLabel) {
        self.emit_arm64_branch(0x1400_0000, Arm64Reloc::B26, label.clone());
    }

    /// `BL label` — branch with link to a label.
    pub fn bl_label(&mut self, label: &DestLabel) {
        self.emit_arm64_branch(0x9400_0000, Arm64Reloc::B26, label.clone());
    }

    /// `B.cond label` — conditional branch to a label.
    pub fn bcond_label(&mut self, cond: Cond, label: &DestLabel) {
        self.emit_arm64_branch(0x5400_0000 | cond.enc(), Arm64Reloc::Imm19, label.clone());
    }

    /// `CBZ Xt, label` — branch if `rt == 0`.
    pub fn cbz_label(&mut self, rt: GReg, label: &DestLabel) {
        self.emit_arm64_branch(0xb400_0000 | rt.enc(), Arm64Reloc::Imm19, label.clone());
    }

    /// `CBNZ Xt, label` — branch if `rt != 0`.
    pub fn cbnz_label(&mut self, rt: GReg, label: &DestLabel) {
        self.emit_arm64_branch(0xb500_0000 | rt.enc(), Arm64Reloc::Imm19, label.clone());
    }

    /// `TBZ Xt, #bit, label` — branch if bit `bit` of `rt` is zero.
    pub fn tbz_label(&mut self, rt: GReg, bit: u32, label: &DestLabel) {
        debug_assert!(bit < 64, "tbz: bit out of range");
        let b5 = (bit >> 5) & 1;
        let b40 = bit & 0x1f;
        self.emit_arm64_branch(
            0x3600_0000 | (b5 << 31) | (b40 << 19) | rt.enc(),
            Arm64Reloc::Test14,
            label.clone(),
        );
    }

    /// `TBNZ Xt, #bit, label` — branch if bit `bit` of `rt` is one.
    pub fn tbnz_label(&mut self, rt: GReg, bit: u32, label: &DestLabel) {
        debug_assert!(bit < 64, "tbnz: bit out of range");
        let b5 = (bit >> 5) & 1;
        let b40 = bit & 0x1f;
        self.emit_arm64_branch(
            0x3700_0000 | (b5 << 31) | (b40 << 19) | rt.enc(),
            Arm64Reloc::Test14,
            label.clone(),
        );
    }

    /// `ADR Xd, label` — compute the PC-relative address of a label.
    pub fn adr(&mut self, rd: GReg, label: &DestLabel) {
        self.emit_arm64_branch(0x1000_0000 | rd.enc(), Arm64Reloc::Adr, label.clone());
    }

    // ===================================================================
    // System / misc
    // ===================================================================

    /// `NOP`.
    pub fn nop(&mut self) {
        self.emitl(0xd503_201f);
    }

    /// `BRK #imm16` — software breakpoint.
    pub fn brk(&mut self, imm16: u16) {
        self.emitl(0xd420_0000 | ((imm16 as u32) << 5));
    }
}
