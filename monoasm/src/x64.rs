//! x86-64 instruction encoders and operand types for [`JitMemory`].
//!
//! monoasm was originally an x86-64 only assembler; this module holds the
//! x86-64 backend: the register / operand types, the REX/ModRM/SIB
//! encoding machinery, the instruction encoders, and code dumping. The
//! architecture-neutral JIT engine (executable pages, labels,
//! relocations, the `emit*` helpers) lives in [`crate::jit_memory`], and
//! the AArch64 backend lives in [`crate::arm64`].

use crate::*;
use std::alloc::{alloc, Layout};
use std::io::Write;

/// Register.
#[derive(Copy, Clone, PartialEq, Debug)]
pub struct Reg(u8);

impl Reg {
    pub fn from(num: u64) -> Self {
        Reg(num as u8)
    }

    pub fn is_rip(&self) -> bool {
        self == &Self::rip()
    }

    pub fn is_cl(&self) -> bool {
        self == &Self::rcx()
    }

    pub fn is_rax(&self) -> bool {
        self == &Self::rax()
    }
}

impl Reg {
    fn rax() -> Self {
        Self::from(0)
    }
    fn rcx() -> Self {
        Self::from(1)
    }

    fn rbp() -> Self {
        Self::from(5)
    }

    fn rip() -> Self {
        Self::from(16)
    }
}

impl std::fmt::Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "R({})", self.0)
    }
}

/// Displacement for indirect addressing.
#[derive(Clone, PartialEq, Debug)]
pub enum Disp {
    None,
    D8(i8),
    D32(i32),
    Label(DestLabel),
}

impl Disp {
    pub fn from_disp(disp: i32) -> Self {
        match disp {
            0 => Disp::None,
            disp => {
                if let Ok(disp) = i8::try_from(disp) {
                    Disp::D8(disp)
                } else {
                    Disp::D32(disp)
                }
            }
        }
    }

    pub fn from_label(label: DestLabel) -> Self {
        Disp::Label(label)
    }
}

/// Scale index for indirect addressing.
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Scale {
    None,
    S1(u8, Reg),
}

impl Scale {
    fn index(&self) -> Reg {
        match self {
            Self::None => Reg(0),
            Self::S1(_, r) => *r,
        }
    }
}

pub enum Imm {
    None,
    B(i8),
    W(i16),
    L(i32),
    Q(i64),
}

impl Imm {
    pub fn offset(&self) -> u8 {
        match self {
            Self::None => 0,
            Self::B(_) => 1,
            Self::W(_) => 2,
            Self::L(_) => 4,
            Self::Q(_) => 8,
        }
    }
}

/// Destination for jump and call instructions.
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Dest {
    /// Register
    Reg(Reg),
    /// Relative
    Rel(usize),
}

/// Adressing modes.
#[derive(Clone, PartialEq, Debug)]
pub enum Mode {
    Reg,
    Ind(Scale, Disp), // [reg + disp]
}

impl Mode {
    fn encode(&self) -> u8 {
        match self {
            Mode::Reg => 3,
            Mode::Ind(_, Disp::None) => 0,
            Mode::Ind(_, Disp::D8(_)) => 1,
            Mode::Ind(_, Disp::D32(_)) => 2,
            Mode::Ind(_, Disp::Label(_)) => 2,
        }
    }

    fn scale(&self) -> Scale {
        match self {
            Mode::Reg => Scale::None,
            Mode::Ind(scale, _) => *scale,
        }
    }

    fn disp(&self) -> Disp {
        match self {
            Mode::Reg => Disp::None,
            Mode::Ind(_, disp) => disp.clone(),
        }
    }

    fn is_indirect_no_disp(&self) -> bool {
        matches!(self, Mode::Ind(_, Disp::None))
    }
}

/// Register / Memory reference Operands.
#[derive(Clone, PartialEq, Debug)]
pub struct Rm {
    base: Reg,
    mode: Mode,
}

impl std::convert::From<Reg> for Rm {
    fn from(value: Reg) -> Self {
        Rm::reg(value)
    }
}

impl Rm {
    pub fn reg(base: Reg) -> Self {
        Self {
            base,
            mode: Mode::Reg,
        }
    }

    pub fn is_reg(&self) -> bool {
        self.mode == Mode::Reg
    }

    pub fn is_rax(&self) -> bool {
        self.mode == Mode::Reg && self.base.is_rax()
    }

    pub fn ind(base: Reg, disp: Disp, scale: Scale) -> Self {
        let mode = Mode::Ind(scale, disp);
        Self { base, mode }
    }

    pub fn rip_ind_from(rm: Rm) -> Self {
        let disp = match rm.mode {
            Mode::Reg => unimplemented!("register direct addressing is not allowed for RIP."),
            Mode::Ind(Scale::None, Disp::D8(d)) => d as i32,
            Mode::Ind(Scale::None, Disp::D32(d)) => d,
            Mode::Ind(Scale::None, Disp::None) => 0,
            Mode::Ind(Scale::None, Disp::Label(label)) => {
                return Self {
                    base: Reg::rbp(),
                    mode: Mode::Ind(Scale::None, Disp::Label(label)),
                }
            }
            _ => unimplemented!("scale index is not allowed for RIP."),
        };
        Self {
            base: Reg::rbp(),
            mode: Mode::Ind(Scale::None, Disp::D32(disp)),
        }
    }
}

enum ModRM {
    Reg(Reg),
    Digit(u8),
}

enum Rex {
    REXW,
    None,
    Byte,
}

#[allow(dead_code)]
impl JitMemory {
    /// Encoding: Opcode +rd
    /// Op+ rd
    pub fn enc_o(&mut self, op: u8, reg: Reg) {
        assert!(!reg.is_rip());
        self.rex_none(Reg(0), reg, Reg(0), Mode::Reg);
        self.op_with_rd(op, reg);
    }

    /// Encoding: Opcode +rd
    /// Op+ rd
    pub fn enc_oi(&mut self, op: u8, reg: Reg) {
        assert!(!reg.is_rip());
        self.rex_none(Reg(0), reg, Reg(0), Mode::Reg);
        self.op_with_rd(op, reg);
    }

    pub fn enc_oi_byte(&mut self, op: u8, reg: Reg) {
        assert!(!reg.is_rip());
        self.rex_none_byte(Reg(0), reg, Reg(0), Mode::Reg);
        self.op_with_rd(op, reg);
    }

    /// Encoding: Opcode +rd
    /// REX.W Op+ rd
    pub fn enc_rexw_o(&mut self, op: u8, reg: Reg) {
        assert!(!reg.is_rip());
        self.rexw(Reg(0), reg, Reg(0), Mode::Reg);
        self.op_with_rd(op, reg);
    }

    /// Encoding: MI
    /// Op ModRM:r/m
    pub fn enc_mi(&mut self, op: u8, rm_op: Rm, imm: Imm) {
        self.encode(&[op], Rex::None, ModRM::Reg(Reg(0)), rm_op, imm);
    }

    /// Encoding: MI
    /// REX.W Op ModRM:r/m
    pub fn enc_rexw_mi(&mut self, op: u8, rm_op: Rm, imm: Imm) {
        self.encode(&[op], Rex::REXW, ModRM::Reg(Reg(0)), rm_op, imm);
    }

    pub fn enc_mi_byte(&mut self, op: u8, rm_op: Rm, imm: Imm) {
        self.encode(&[op], Rex::Byte, ModRM::Reg(Reg(0)), rm_op, imm);
    }

    /// REX Op ModRM
    /// MR-> ModRM:r/m(w) ModRM:reg(r)
    /// RM-> ModRM:reg(r) ModRM:r/m(w)
    pub fn enc_mr(&mut self, op: &[u8], reg: Reg, rm_op: Rm) {
        self.encode(op, Rex::None, ModRM::Reg(reg), rm_op, Imm::None);
    }

    pub fn enc_mr_byte(&mut self, op: &[u8], reg: Reg, rm_op: Rm) {
        self.encode(op, Rex::Byte, ModRM::Reg(reg), rm_op, Imm::None);
    }

    /// REX.W Op ModRM
    /// MR-> ModRM:r/m(w) ModRM:reg(r)
    /// RM-> ModRM:reg(r) ModRM:r/m(w)
    pub fn enc_rexw_mr(&mut self, op: &[u8], reg: Reg, rm_op: Rm) {
        self.encode(op, Rex::REXW, ModRM::Reg(reg), rm_op, Imm::None);
    }

    /// This is used in "setcc r/m8".
    pub fn enc_m_byte(&mut self, op: &[u8], rm: Rm) {
        self.encode(op, Rex::Byte, ModRM::Reg(Reg(0)), rm, Imm::None);
    }

    /// Encoding: D
    /// Op cd
    pub fn enc_d(&mut self, op: &[u8], dest: DestLabel) {
        self.emit(op);
        self.emit_reloc(dest, 4);
    }

    /// Encoding: /n
    /// Op /n
    pub fn enc_digit(&mut self, op: &[u8], rm: impl Into<Rm>, digit: u8) {
        let rm = rm.into();
        self.encode(op, Rex::None, ModRM::Digit(digit), rm, Imm::None);
    }

    pub fn enc_digit_imm(&mut self, op: &[u8], rm: Rm, digit: u8, imm: Imm) {
        self.encode(op, Rex::None, ModRM::Digit(digit), rm, imm);
    }

    pub fn enc_digit_imm_byte(&mut self, op: &[u8], op_al: u8, rm: Rm, digit: u8, imm: Imm) {
        if rm.is_rax() {
            self.emitb(op_al);
            self.emit_disp_imm(Disp::None, imm);
        } else if rm.is_reg() {
            self.encode(op, Rex::Byte, ModRM::Digit(digit), rm, imm);
        } else {
            self.encode(op, Rex::None, ModRM::Digit(digit), rm, imm);
        }
    }

    /// Encoding: /n
    /// REX.W Op /n
    pub fn enc_rexw_digit(&mut self, op: &[u8], rm: Rm, digit: u8, imm: Imm) {
        self.encode(op, Rex::REXW, ModRM::Digit(digit), rm, imm);
    }

    fn encode(&mut self, op: &[u8], rex: Rex, modrm_mode: ModRM, rm: Rm, imm: Imm) {
        let reg = match modrm_mode {
            ModRM::Digit(_) => Reg(0),
            ModRM::Reg(r) => r,
        };
        let rex_fn = match rex {
            Rex::REXW => JitMemory::rexw,
            Rex::None => JitMemory::rex_none,
            Rex::Byte => JitMemory::rex_none_byte,
        };
        assert!(!reg.is_rip());
        if rm.base.is_rip() {
            // For rip, only indirect addressing with disp32 ([rip + disp32]) is allowed.
            // [rip] and [rip + disp8] are to be converted to [rip + disp32].
            let rm = Rm::rip_ind_from(rm);
            rex_fn(self, reg, rm.base, Reg(0), rm.mode.clone());
            self.emit(op);
            self.modrm(modrm_mode, Mode::Ind(Scale::None, Disp::None), rm.base);
            self.emit_disp_imm(rm.mode.disp(), imm);
        } else if rm.mode != Mode::Reg && (rm.base.0 & 0b111) == 4 {
            // If mode != Reg and r/m == 4/12 (rsp/r12), use SIB.
            match rm.mode.clone() {
                Mode::Ind(scale, disp) => {
                    let (scale, index) = match scale {
                        Scale::None => (0, Reg(4)), // magic number
                        Scale::S1(scale, index) => (scale, index),
                    };
                    let base = rm.base;
                    rex_fn(self, reg, base, index, rm.mode.clone());
                    self.emit(op);
                    self.modrm(modrm_mode, rm.mode.clone(), base);
                    self.sib(scale, index, base);
                    self.emit_disp_imm(disp.clone(), imm);
                }
                _ => unreachable!(),
            }
        } else if rm.mode.is_indirect_no_disp() && (rm.base.0 & 0b111) == 5 {
            // If mode == Ind and r/m == 5/13 (rbp/r13), use [rbp/r13 + 0(disp8)].
            let scale = rm.mode.scale();
            rex_fn(self, reg, rm.base, scale.index(), rm.mode);
            let mode = Mode::Ind(scale, Disp::D8(0));
            self.emit(op);
            self.modrm(modrm_mode, mode.clone(), rm.base);
            match scale {
                Scale::None => {}
                Scale::S1(scale, index) => self.sib(scale, index, rm.base),
            }
            self.emit_disp_imm(mode.disp(), imm);
        } else {
            rex_fn(
                self,
                reg,
                rm.base,
                match rm.mode.clone() {
                    Mode::Reg => Reg(0),
                    Mode::Ind(scale, _) => match scale {
                        Scale::None => Reg(0),
                        Scale::S1(_, index) => index,
                    },
                },
                rm.mode.clone(),
            );
            // index != Reg::RIP
            self.emit(op);
            self.modrm(modrm_mode, rm.mode.clone(), rm.base);
            match rm.mode {
                Mode::Reg => {}
                Mode::Ind(scale, _) => match scale {
                    Scale::None => {}
                    Scale::S1(scale, index) => self.sib(scale, index, rm.base),
                },
            };
            self.emit_disp_imm(rm.mode.disp(), imm);
        }
    }
}

impl JitMemory {
    /// ModRM
    ///
    /// ~~~~text
    /// +-------+---+---+---+---+---+---+---+---+
    /// |  bit  | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
    /// +-------+---+---+---+---+---+---+---+---+
    /// | field |  mod  |    reg    |    r/m    |
    /// +-------+-------+-----------+-----------+
    /// |  rex  |       |     r     |     b     |
    /// +-------+-------+-----------+-----------+
    /// ~~~~
    ///
    fn modrm(&mut self, modrm_mode: ModRM, mode: Mode, base: Reg) {
        let base = match mode {
            Mode::Reg | Mode::Ind(Scale::None, _) => base,
            Mode::Ind(_, _) => Reg(4),
        };
        let mode = mode.encode();
        let modrm = mode << 6
            | (match modrm_mode {
                ModRM::Digit(d) => d,
                ModRM::Reg(r) => r.0,
            } & 0b111)
                << 3
            | (base.0 & 0b111);
        self.emitb(modrm);
    }

    /// REX.W
    ///
    /// ~~~~text
    ///      bit
    /// +---+---+------------------------------------------------+
    /// | W | 3 | 1 = 64 bit operand size                        |
    /// +---+---+------------------------------------------------+
    /// | R | 2 | rex_r = ext of reg field of ModRM              |
    /// +---+---+------------------------------------------------+
    /// | X | 1 | rex_i = ext of index field of SIB              |
    /// +---+---+------------------------------------------------+
    /// | B | 0 | rex_b = ext of r/m(ModRM) or base(SIB)         |
    /// |   |   |           or reg field of Op.                  |
    /// +---+---+------------------------------------------------+
    /// ~~~~
    ///
    fn rexw(&mut self, reg: Reg, base: Reg, index: Reg, _mode: Mode) {
        let rexw = 0x48 | (reg.0 & 0b1000) >> 1 | (index.0 & 0b1000) >> 2 | (base.0 & 0b1000) >> 3;
        self.emitb(rexw);
    }

    fn rex_none(&mut self, reg: Reg, base: Reg, index: Reg, mode: Mode) {
        if reg.0 > 7 || base.0 > 7 || index.0 > 7 {
            self.rex(reg, base, index, mode);
        };
    }

    fn rex_none_byte(&mut self, reg: Reg, base: Reg, index: Reg, mode: Mode) {
        if reg.0 > 7 || base.0 > 7 || index.0 > 7 {
            self.rex(reg, base, index, mode);
        } else if reg.0 > 3 || (base.0 > 3 && mode == Mode::Reg) {
            self.rex(Reg(0), Reg(0), Reg(0), mode);
        };
    }

    fn rex(&mut self, reg: Reg, base: Reg, index: Reg, _mode: Mode) {
        let rex_prefix =
            0x40 | (reg.0 & 0b1000) >> 1 | (index.0 & 0b1000) >> 2 | (base.0 & 0b1000) >> 3;
        self.emitb(rex_prefix);
    }

    fn op_with_rd(&mut self, op: u8, reg: Reg) {
        let op = op | (reg.0 & 0b0111);
        self.emitb(op);
    }

    /// SIB
    ///
    /// ~~~~text
    /// +-------+---+---+---+---+---+---+---+---+
    /// |  bit  | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
    /// +-------+---+---+---+---+---+---+---+---+
    /// | field | scale |   index   |    base   |
    /// +-------+-------+-----------+-----------+
    /// |  rex  |       |     x     |     b     |
    /// +-------+-------+-----------+-----------+
    ///
    /// scale: 00|01|10|11
    ///  mul : na| 2| 4| 8
    ///
    /// index: register number (with rex.x)
    ///
    /// base: register number (with rex.b)
    ///     rex.b:0 base:101 => use RBP  mode:00/disp32 01/RBP+disp8 10/RBP+disp32
    ///     rex.b:1 base:101 => use R13  mode:00/disp32 01/R13+disp8 10/R13+disp32
    /// ~~~~
    ///
    fn sib(&mut self, scale: u8, index: Reg, base: Reg) {
        assert!(scale < 4);
        assert!(index.0 < 16);
        let sib = (scale << 6) | ((index.0 & 0b111) << 3) | (base.0 & 0b111);
        self.emitb(sib);
    }

    fn emit_disp_imm(&mut self, disp: Disp, imm: Imm) {
        match disp {
            Disp::D8(d) => self.emitb(d as u8),
            Disp::D32(d) => self.emitl(d as u32),
            Disp::Label(label) => {
                self.emit_reloc(label, 4 + imm.offset());
            }
            Disp::None => {}
        }
        match imm {
            Imm::None => {}
            Imm::B(b) => self.emitb(b as u8),
            Imm::W(w) => self.emitw(w as u16),
            Imm::L(l) => self.emitl(l as u32),
            Imm::Q(q) => self.emitq(q as u64),
        }
    }
}

impl JitMemory {
    ///
    /// Apply patch for the displacement of the jmp instruction in *patch_point*.
    ///
    pub fn apply_jmp_patch_address(&mut self, patch_point: CodePtr, jmp_dest: &DestLabel) {
        let jmp_dest = self.get_label_address(jmp_dest);
        let offset = jmp_dest - patch_point - 5;
        unsafe { *(patch_point.as_ptr().add(1) as *mut [u8; 4]) = (offset as i32).to_ne_bytes() };
    }

    /// Dump generated code.
    pub fn dump_code(&self) -> Result<String, std::io::Error> {
        use std::process::Command;
        let asm = self.as_slice();
        let mut file = tempfile::NamedTempFile::new()?;
        let (start_pos, code_end, _end_pos) = self.code_block.last().unwrap();
        file.write_all(&asm[start_pos.0..code_end.0]).unwrap();

        Command::new("objdump")
            .args([
                "-D",
                "-Mintel,x86-64",
                "-b",
                "binary",
                "-m",
                "i386",
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
}

// ===========================================================================
// Relocations
// ===========================================================================

/// Relocation target descriptor for the x86-64 backend.
///
/// Queued by [`JitMemory::handle_reloc`] and resolved by
/// [`JitMemory::write_reloc`] once the target [`DestLabel`] is bound.
#[derive(Clone, PartialEq, Debug)]
pub(crate) enum TargetType {
    /// A 32-bit PC-relative displacement slot at `pos`. The displacement
    /// is measured from `pos + offset` (the end of the instruction), as
    /// used by `jmp`/`call`/`jcc` and RIP-relative addressing.
    Rel { page: Page, offset: u8, pos: Pos },
    /// An absolute 64-bit address slot at `pos` (constant / data pools).
    Abs { page: Page, pos: Pos },
}

impl JitMemory {
    /// Save a 32-bit PC-relative relocation slot for `dest`. `offset` is
    /// the number of bytes from the slot to the end of the instruction.
    pub fn emit_reloc(&mut self, dest: DestLabel, offset: u8) {
        let page = self.cur_page();
        let pos = self.cur_pos();
        let target = TargetType::Rel { page, offset, pos };
        self.emitl(0);
        self.handle_reloc(dest, target);
    }

    /// Patch a single relocation `target` now that its label resolves to
    /// `(src_page, src_pos)`.
    pub(crate) fn write_reloc(&mut self, src_page: Page, src_pos: Pos, target: TargetType) {
        let src_ptr = self[src_page].contents() as usize + src_pos.0;
        match target {
            TargetType::Rel { page, offset, pos } => {
                let target_ptr = self[page].contents() as usize + pos.0 + (offset as usize);
                let disp = (src_ptr as i128) - (target_ptr as i128);
                match i32::try_from(disp) {
                    Ok(disp) => self[page].write32(pos, disp),
                    Err(_) => panic!(
                        "Relocation overflow. src:{:016x} dest:{:016x}",
                        src_ptr, target_ptr
                    ),
                }
            }
            TargetType::Abs { page, pos } => {
                self[page].write64(pos, src_ptr as _);
            }
        }
    }
}

// ===========================================================================
// JIT page protection (W^X)
// ===========================================================================

/// x86-64 JIT page protection: the code pages are plain RWX and the
/// I-cache is coherent with the D-cache, so every W^X operation is a
/// no-op and this carries no state.
#[derive(Debug)]
pub(crate) struct JitProtect;

impl JitProtect {
    #[inline]
    pub(crate) fn new() -> Self {
        JitProtect
    }

    /// Allocate the two contiguous RWX code pages plus a separate RW data
    /// page, returning `(code_pages_base, data_page_base)`.
    pub(crate) fn allocate_pages() -> (*mut u8, *mut u8) {
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

    #[inline]
    pub(crate) fn ensure_writable(&mut self) {}

    #[inline]
    pub(crate) fn set_writable(&mut self) {}

    #[inline]
    pub(crate) fn set_executable(&mut self) {}

    /// No-op: x86-64 I-caches are kept coherent by hardware.
    #[inline]
    pub(crate) unsafe fn invalidate_icache(_ptr: *const u8, _len: usize) {}
}
