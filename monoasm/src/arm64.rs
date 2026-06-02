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

use crate::{DestLabel, JitMemory, Page, Pos, PAGE_SIZE};
use std::alloc::{alloc, Layout};

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
    /// The 5-bit register field encoding (`0..=31`).
    #[inline]
    pub fn enc(self) -> u32 {
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
    /// The 5-bit register field encoding (`0..=31`).
    #[inline]
    pub fn enc(self) -> u32 {
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
    // Generic A64 instruction-word encoders (the encoding "machinery").
    //
    // Each takes a `base` word — the opcode together with all fixed bits —
    // and ORs in the operand fields at their canonical positions. The
    // per-instruction base words live in the `monoasm_arm64!` macro, the
    // same way the x86-64 `monoasm!` macro feeds opcodes to the generic
    // `enc_*` encoders. For programmatic emission that needs runtime
    // `Cond` / `DestLabel` values or expands to several instructions, the
    // convenience methods further down wrap these encoders.
    // ===================================================================

    /// MOV-wide family (`MOVZ`/`MOVN`/`MOVK`):
    /// `base | hw<<21 | imm16<<5 | Rd`.
    pub fn movewide(&mut self, base: u32, rd: GReg, imm16: u32, hw: u32) {
        debug_assert!(hw < 4, "movewide: hw out of range");
        self.emitl(base | (hw << 21) | ((imm16 & 0xffff) << 5) | rd.enc());
    }

    /// Add/subtract (immediate):
    /// `base | shift12<<22 | imm12<<10 | Rn<<5 | Rd`.
    pub fn addsub_imm(&mut self, base: u32, rd: GReg, rn: GReg, imm12: u32, shift12: u32) {
        debug_assert!(imm12 < (1 << 12), "addsub_imm: imm12 out of range");
        debug_assert!(shift12 < 2, "addsub_imm: shift must be 0 or 1");
        self.emitl(base | (shift12 << 22) | (imm12 << 10) | (rn.enc() << 5) | rd.enc());
    }

    /// Add/subtract (shifted register):
    /// `base | Rm<<16 | shift<<10 | Rn<<5 | Rd`.
    pub fn addsub_reg(&mut self, base: u32, rd: GReg, rn: GReg, rm: GReg, shift: u32) {
        debug_assert!(shift < 64, "addsub_reg: LSL amount out of range");
        self.emitl(base | (rm.enc() << 16) | (shift << 10) | (rn.enc() << 5) | rd.enc());
    }

    /// Logical (shifted register):
    /// `base | Rm<<16 | shift<<10 | Rn<<5 | Rd`.
    pub fn logical_reg(&mut self, base: u32, rd: GReg, rn: GReg, rm: GReg, shift: u32) {
        debug_assert!(shift < 64, "logical_reg: LSL amount out of range");
        self.emitl(base | (rm.enc() << 16) | (shift << 10) | (rn.enc() << 5) | rd.enc());
    }

    /// Bitfield move (`UBFM`/`SBFM`):
    /// `base | immr<<16 | imms<<10 | Rn<<5 | Rd`.
    pub fn bfm(&mut self, base: u32, rd: GReg, rn: GReg, immr: u32, imms: u32) {
        self.emitl(base | (immr << 16) | (imms << 10) | (rn.enc() << 5) | rd.enc());
    }

    /// Two-source data processing: `base | Rm<<16 | Rn<<5 | Rd`
    /// (`SDIV`/`UDIV`/`LSLV`/`LSRV`/`ASRV`/`RORV`).
    pub fn dp_2src(&mut self, base: u32, rd: GReg, rn: GReg, rm: GReg) {
        self.emitl(base | (rm.enc() << 16) | (rn.enc() << 5) | rd.enc());
    }

    /// Extract register (`EXTR`): `base | Rm<<16 | imms<<10 | Rn<<5 | Rd`.
    /// `ROR Xd, Xn, #shift` is the alias `EXTR Xd, Xn, Xn, #shift`.
    pub fn extr(&mut self, base: u32, rd: GReg, rn: GReg, rm: GReg, imms: u32) {
        debug_assert!(imms < 64, "extr: shift amount out of range");
        self.emitl(base | (rm.enc() << 16) | (imms << 10) | (rn.enc() << 5) | rd.enc());
    }

    /// Three-source data processing: `base | Rm<<16 | Ra<<10 | Rn<<5 | Rd`
    /// (`MADD`/`MSUB`).
    pub fn dp_3src(&mut self, base: u32, rd: GReg, rn: GReg, rm: GReg, ra: GReg) {
        self.emitl(base | (rm.enc() << 16) | (ra.enc() << 10) | (rn.enc() << 5) | rd.enc());
    }

    /// Conditional select: `base | Rm<<16 | cond<<12 | Rn<<5 | Rd`.
    pub fn condsel(&mut self, base: u32, rd: GReg, rn: GReg, rm: GReg, cond: Cond) {
        self.emitl(base | (rm.enc() << 16) | (cond.enc() << 12) | (rn.enc() << 5) | rd.enc());
    }

    /// Load/store (unsigned scaled immediate):
    /// `base | imm12<<10 | Rn<<5 | Rt`. `byte_off` is scaled down by
    /// `scale`; `rt` is a pre-encoded register field so the one encoder
    /// serves both general-purpose and SIMD&FP forms.
    pub fn ldst_uimm(&mut self, base: u32, scale: u32, rt: u32, rn: GReg, byte_off: u32) {
        debug_assert!(
            byte_off & ((1 << scale) - 1) == 0,
            "load/store offset misaligned"
        );
        let imm12 = byte_off >> scale;
        debug_assert!(imm12 < (1 << 12), "load/store offset out of range");
        self.emitl(base | (imm12 << 10) | (rn.enc() << 5) | rt);
    }

    /// Load/store (9-bit signed pre/post-indexed):
    /// `base | imm9<<12 | Rn<<5 | Rt`.
    pub fn ldst_idx(&mut self, base: u32, rt: u32, rn: GReg, imm9: i32) {
        debug_assert!(
            (-256..256).contains(&imm9),
            "pre/post-index imm out of range"
        );
        self.emitl(base | (((imm9 as u32) & 0x1ff) << 12) | (rn.enc() << 5) | rt);
    }

    /// Load/store (register offset): `base | Rm<<16 | S<<12 | Rn<<5 | Rt`.
    /// `scaled` selects the scaled-index (`LSL`) form.
    pub fn ldst_reg(&mut self, base: u32, rt: GReg, rn: GReg, rm: GReg, scaled: bool) {
        let s = if scaled { 1 } else { 0 };
        self.emitl(base | (rm.enc() << 16) | (s << 12) | (rn.enc() << 5) | rt.enc());
    }

    /// Load/store pair (7-bit signed offset scaled by 8):
    /// `base | imm7<<15 | Rt2<<10 | Rn<<5 | Rt`.
    pub fn ldstp(&mut self, base: u32, rt: GReg, rt2: GReg, rn: GReg, byte_off: i32) {
        debug_assert!(byte_off % 8 == 0, "ldp/stp offset not a multiple of 8");
        let imm7 = byte_off / 8;
        debug_assert!((-64..64).contains(&imm7), "ldp/stp offset out of range");
        self.emitl(
            base | (((imm7 as u32) & 0x7f) << 15) | (rt2.enc() << 10) | (rn.enc() << 5) | rt.enc(),
        );
    }

    /// Three-register scalar SIMD&FP op: `base | Rm<<16 | Rn<<5 | Rd`
    /// (`FADD`/`FSUB`/`FMUL`/`FDIV`).
    pub fn fp_3op(&mut self, base: u32, rd: FReg, rn: FReg, rm: FReg) {
        self.emitl(base | (rm.enc() << 16) | (rn.enc() << 5) | rd.enc());
    }

    /// Scalar SIMD&FP compare: `base | Rm<<16 | Rn<<5` (`FCMP Dn, Dm`).
    pub fn fp_cmp(&mut self, base: u32, rn: FReg, rm: FReg) {
        self.emitl(base | (rm.enc() << 16) | (rn.enc() << 5));
    }

    /// Two-field word `base | Rn<<5 | Rd`. Used for cross-class register
    /// moves (`FMOV` variants, `SCVTF`, `FCVTZS`), `FCMP Dn, #0.0`,
    /// single-register branches (`RET`/`BR`/`BLR`), and `BRK`. `rd`/`rn`
    /// are pre-encoded register (or immediate) fields.
    pub fn emit_rr(&mut self, base: u32, rd: u32, rn: u32) {
        self.emitl(base | (rn << 5) | rd);
    }

    // ===================================================================
    // Convenience methods kept as a programmatic API.
    //
    // These take runtime `Cond` / `DestLabel` values or expand to several
    // instructions, so the compile-time `monoasm_arm64!` macro cannot
    // inline them: it emits them by calling these methods, and they are
    // also used directly for programmatic code generation.
    // ===================================================================

    /// Materialize an arbitrary 64-bit immediate into `rd` using a `MOVZ`
    /// followed by a `MOVK` for each remaining non-zero halfword
    /// (1–4 instructions).
    pub fn mov_imm(&mut self, rd: GReg, imm: u64) {
        let hw = [
            (imm & 0xffff) as u32,
            ((imm >> 16) & 0xffff) as u32,
            ((imm >> 32) & 0xffff) as u32,
            ((imm >> 48) & 0xffff) as u32,
        ];
        // MOVZ Rd, #hw0, LSL #0.
        self.movewide(0xd280_0000, rd, hw[0], 0);
        for (i, &h) in hw.iter().enumerate().skip(1) {
            if h != 0 {
                // MOVK Rd, #h, LSL #(16 * i).
                self.movewide(0xf280_0000, rd, h, i as u32);
            }
        }
    }

    /// `CSEL Xd, Xn, Xm, cond`.
    pub fn csel(&mut self, rd: GReg, rn: GReg, rm: GReg, cond: Cond) {
        self.condsel(0x9a80_0000, rd, rn, rm, cond);
    }

    /// `CSINC Xd, Xn, Xm, cond`.
    pub fn csinc(&mut self, rd: GReg, rn: GReg, rm: GReg, cond: Cond) {
        self.condsel(0x9a80_0400, rd, rn, rm, cond);
    }

    /// `CSET Xd, cond` — set `Xd` to 1 if `cond` holds, else 0
    /// (`CSINC Xd, XZR, XZR, invert(cond)`).
    pub fn cset(&mut self, rd: GReg, cond: Cond) {
        self.condsel(0x9a80_0400, rd, XZR, XZR, cond.invert());
    }

    /// `CSETM Xd, cond` — set `Xd` to all-ones if `cond` holds, else 0
    /// (`CSINV Xd, XZR, XZR, invert(cond)`).
    pub fn csetm(&mut self, rd: GReg, cond: Cond) {
        self.condsel(0xda80_0000, rd, XZR, XZR, cond.invert());
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
}

// ===========================================================================
// Relocations
// ===========================================================================

/// Relocation target descriptor for the AArch64 backend.
///
/// Queued by [`JitMemory::handle_reloc`] and resolved by
/// [`JitMemory::write_reloc`] once the target [`DestLabel`] is bound.
#[derive(Clone, PartialEq, Debug)]
pub(crate) enum TargetType {
    /// An absolute 64-bit address slot at `pos` (constant / data pools).
    Abs { page: Page, pos: Pos },
    /// A PC-relative branch / `ADR`: the scaled immediate is patched into
    /// the bitfields of the instruction word already emitted at `pos`,
    /// rather than into a separate displacement slot. `kind` selects the
    /// immediate layout.
    Rel { page: Page, pos: Pos, kind: Arm64Reloc },
}

impl JitMemory {
    /// Emit an AArch64 PC-relative branch / `ADR` instruction whose target
    /// is `dest`. `base_word` is the fully-encoded instruction with a
    /// zeroed immediate field; `kind` describes how the displacement is
    /// later patched in.
    pub fn emit_arm64_branch(&mut self, base_word: u32, kind: Arm64Reloc, dest: DestLabel) {
        let page = self.cur_page();
        let pos = self.cur_pos();
        self.emitl(base_word);
        let target = TargetType::Rel { page, pos, kind };
        self.handle_reloc(dest, target);
    }

    /// Dump the generated machine code as an objdump-style disassembly
    /// listing (the AArch64 counterpart of the x86-64 `dump_code`).
    ///
    /// The disassembler binary defaults to `objdump`, which is the native
    /// tool on an aarch64 host. When running the emulated tests on a
    /// non-aarch64 host, set the `OBJDUMP` environment variable to a
    /// cross-capable binutils (e.g. `aarch64-linux-gnu-objdump`) so the
    /// A64 stream is decoded correctly.
    pub fn dump_code(&self) -> Result<String, std::io::Error> {
        use std::io::Write;
        use std::process::Command;
        let asm = self.as_slice();
        let mut file = tempfile::NamedTempFile::new()?;
        let (start_pos, code_end, _end_pos) = self.code_block.last().unwrap();
        file.write_all(&asm[start_pos.0..code_end.0]).unwrap();

        let objdump = std::env::var("OBJDUMP").unwrap_or_else(|_| "objdump".to_string());
        Command::new(objdump)
            .args([
                "-D",
                "-b",
                "binary",
                "-m",
                "aarch64",
                file.path().to_str().unwrap(),
            ])
            .output()
            .map(|o| {
                std::str::from_utf8(&o.stdout)
                    .unwrap()
                    .to_string()
                    .split_inclusive('\n')
                    .filter(|s| {
                        s.len() > 1
                            && !s.contains("file format binary")
                            && !s.contains("Disassembly of section")
                            && !s.contains("<.data>")
                    })
                    .collect()
            })
    }

    /// Patch a single relocation `target` now that its label resolves to
    /// `(src_page, src_pos)`.
    pub(crate) fn write_reloc(&mut self, src_page: Page, src_pos: Pos, target: TargetType) {
        let src_ptr = self[src_page].contents() as usize + src_pos.0;
        match target {
            TargetType::Abs { page, pos } => {
                self[page].write64(pos, src_ptr as _);
            }
            TargetType::Rel { page, pos, kind } => {
                // AArch64 branches are relative to the address of the
                // branch instruction itself, and the displacement is
                // packed into the bitfields of the existing instruction
                // word (rather than a separate displacement slot).
                let branch_ptr = self[page].contents() as usize + pos.0;
                let disp = (src_ptr as i128) - (branch_ptr as i128);
                let disp = i64::try_from(disp).expect("AArch64 relocation displacement overflow");
                let word = u32::from_le_bytes([
                    self[page][pos],
                    self[page][pos + 1],
                    self[page][pos + 2],
                    self[page][pos + 3],
                ]);
                self[page].write32(pos, kind.patch(word, disp) as i32);
            }
        }
    }
}

// ===========================================================================
// JIT page protection (W^X) and I-cache maintenance
// ===========================================================================

/// Apple Silicon (aarch64-apple-darwin) rejects RWX heap pages: JIT
/// memory must be mapped with `mmap(..., MAP_JIT, ...)` and the per-thread
/// write permission toggled via `pthread_jit_write_protect_np`.
#[cfg(target_os = "macos")]
mod apple_jit {
    use libc::{
        c_void, mmap, MAP_ANON, MAP_FAILED, MAP_JIT, MAP_PRIVATE, PROT_EXEC, PROT_READ, PROT_WRITE,
    };

    extern "C" {
        pub fn pthread_jit_write_protect_np(enabled: i32);
        pub fn sys_icache_invalidate(addr: *mut c_void, len: usize);
    }

    /// Map `size` bytes of MAP_JIT memory. As far as the MMU is concerned
    /// the mapping is RWX, but the *per-thread* writability is gated by
    /// `pthread_jit_write_protect_np`.
    pub unsafe fn alloc(size: usize) -> *mut u8 {
        let p = mmap(
            std::ptr::null_mut(),
            size,
            PROT_READ | PROT_WRITE | PROT_EXEC,
            MAP_PRIVATE | MAP_ANON | MAP_JIT,
            -1,
            0,
        );
        assert!(
            p != MAP_FAILED,
            "monoasm: mmap MAP_JIT failed ({}). On macOS, JIT processes \
             typically need the `com.apple.security.cs.allow-jit` entitlement.",
            std::io::Error::last_os_error()
        );
        p as *mut u8
    }
}

/// AArch64 JIT page protection and I-cache maintenance.
///
/// On macOS the code pages are `MAP_JIT` and their per-thread writability
/// is toggled with `pthread_jit_write_protect_np`; `writable` tracks the
/// current state so emit paths can lazily flip back. On Linux/AArch64 the
/// toggle is a no-op (the pages are RWX) and `writable` stays `true`. In
/// both cases the I-cache must be synchronized after writing code.
#[derive(Debug)]
pub(crate) struct JitProtect {
    writable: bool,
}

impl JitProtect {
    /// Create the protection state. The pages start out writable for the
    /// initial code generation (on macOS this flips the MAP_JIT pages
    /// writable for the current thread).
    pub(crate) fn new() -> Self {
        let mut protect = JitProtect { writable: false };
        protect.set_writable();
        protect
    }

    /// Allocate the two contiguous code pages plus a separate data page,
    /// returning `(code_pages_base, data_page_base)`.
    pub(crate) fn allocate_pages() -> (*mut u8, *mut u8) {
        #[cfg(target_os = "macos")]
        {
            let code = unsafe { apple_jit::alloc(PAGE_SIZE * 2) };
            let data_layout = Layout::from_size_align(PAGE_SIZE, PAGE_SIZE).expect("Bad Layout.");
            let data = unsafe { alloc(data_layout) };
            (code, data)
        }
        #[cfg(not(target_os = "macos"))]
        {
            use region::{protect, Protection};
            let layout = Layout::from_size_align(PAGE_SIZE * 3, PAGE_SIZE).expect("Bad Layout.");
            let contents = unsafe { alloc(layout) };
            unsafe {
                protect(contents, PAGE_SIZE * 2, Protection::READ_WRITE_EXECUTE)
                    .expect("Mprotect failed.");
                protect(contents.add(PAGE_SIZE * 2), PAGE_SIZE, Protection::READ_WRITE)
                    .expect("Mprotect failed.");
            }
            (contents, unsafe { contents.add(PAGE_SIZE * 2) })
        }
    }

    /// Lazily flip the pages back to writable for the current thread if a
    /// previous `set_executable` left them write-protected.
    #[inline]
    pub(crate) fn ensure_writable(&mut self) {
        if !self.writable {
            self.set_writable();
        }
    }

    /// Switch the pages to writable for the current thread.
    #[inline]
    pub(crate) fn set_writable(&mut self) {
        #[cfg(target_os = "macos")]
        unsafe {
            apple_jit::pthread_jit_write_protect_np(0)
        };
        self.writable = true;
    }

    /// Switch the pages to executable for the current thread.
    #[inline]
    pub(crate) fn set_executable(&mut self) {
        #[cfg(target_os = "macos")]
        unsafe {
            apple_jit::pthread_jit_write_protect_np(1)
        };
        self.writable = false;
    }

    /// Synchronize the I-cache over `[ptr, ptr+len)` so the CPU sees the
    /// freshly written instructions.
    #[inline]
    pub(crate) unsafe fn invalidate_icache(ptr: *const u8, len: usize) {
        if len == 0 {
            return;
        }
        #[cfg(target_os = "macos")]
        {
            apple_jit::sys_icache_invalidate(ptr as *mut _, len);
        }
        #[cfg(not(target_os = "macos"))]
        {
            // Conservative 64-byte cache line: every current AArch64 core
            // has D/I-cache lines that are a multiple of 64, and walking
            // smaller lines than the actual size is always safe.
            let line: usize = 64;
            let start = (ptr as usize) & !(line - 1);
            let end = ((ptr as usize) + len + line - 1) & !(line - 1);
            let mut p = start;
            while p < end {
                core::arch::asm!("dc cvau, {x}", x = in(reg) p, options(nostack, preserves_flags));
                p += line;
            }
            core::arch::asm!("dsb ish", options(nostack, preserves_flags));
            let mut p = start;
            while p < end {
                core::arch::asm!("ic ivau, {x}", x = in(reg) p, options(nostack, preserves_flags));
                p += line;
            }
            core::arch::asm!("dsb ish", options(nostack, preserves_flags));
            core::arch::asm!("isb", options(nostack, preserves_flags));
        }
    }
}
