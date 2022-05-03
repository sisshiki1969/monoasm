//-----------------------------------------------------
//
// JIT module runtime
//
//-----------------------------------------------------

use crate::*;
//use monoasm_inst::Reg;
use region::{protect, Protection};
use std::{
    alloc::{alloc, Layout},
    io::Write,
};

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

enum ModRM {
    Reg(Reg),
    Digit(u8),
}

enum Rex {
    REXW,
    REX,
    None,
}

/// Memory manager.
#[derive(Debug)]
pub struct JitMemory {
    /// Pointer to the heap.
    contents: *mut u8,
    /// Current position
    counter: Pos,
    /// Current label id.
    label_count: usize,
    /// Relocation information.
    reloc: Relocations,
    /// Constants section.
    constants: Vec<(u64, DestLabel)>,
    /// Machine code length
    code_len: usize,
}

impl Index<Pos> for JitMemory {
    type Output = u8;

    fn index(&self, index: Pos) -> &u8 {
        if index.0 >= PAGE_SIZE {
            panic!("Page size overflow")
        }
        unsafe { &*self.contents.offset(index.0 as isize) }
    }
}

impl IndexMut<Pos> for JitMemory {
    fn index_mut(&mut self, index: Pos) -> &mut u8 {
        if index.0 >= PAGE_SIZE {
            panic!("Page size overflow")
        }
        unsafe { &mut *self.contents.offset(index.0 as isize) }
    }
}

impl std::default::Default for JitMemory {
    fn default() -> Self {
        Self::new()
    }
}

impl JitMemory {
    /// Create new JitMemory.
    ///
    /// This function try to allocate heap memory of 4KB for JIT assemble.
    ///
    /// ### panic
    /// Panic if Layout::from_size_align() or region::protect() returned Err.
    pub fn new() -> JitMemory {
        let size = 4096;
        let layout = Layout::from_size_align(size, PAGE_SIZE).expect("Bad Layout.");
        let contents = unsafe { alloc(layout) };

        unsafe {
            protect(contents, size, Protection::READ_WRITE_EXECUTE).expect("Mprotect failed.");
        }
        let mut res = JitMemory {
            contents,
            counter: Pos(0),
            label_count: 0,
            reloc: Relocations::new(),
            constants: vec![],
            code_len: 0usize,
        };
        res.emitb(0xc3);
        res.counter = Pos(0);
        res
    }

    /// Resolve all relocations and return the top addresss of generated machine code as a function pointer.
    pub fn finalize(&mut self) {
        self.code_len = self.counter.0;
        self.resolve_constants();
        self.fill_relocs();

        #[cfg(debug_assertions)]
        self.dump_code();
    }

    pub fn to_vec(&self) -> Vec<u8> {
        //let len = self.counter.0;
        let mut v = vec![];
        let slice = unsafe { std::slice::from_raw_parts(self.contents, self.code_len) };
        v.extend_from_slice(slice);
        v
    }

    pub fn get_current(&self) -> usize {
        self.counter.0
    }

    fn _p(&self) {
        for i in 0..self.counter.0 {
            print!("{:>02x} ", self[Pos(i)]);
        }
        println!();
    }

    /// Create a new label and returns `DestLabel`.
    pub fn label(&mut self) -> DestLabel {
        let label = self.label_count;
        self.label_count += 1;
        self.reloc.push(Reloc::new());
        DestLabel(label)
    }

    pub fn const_f64(&mut self, val: f64) -> DestLabel {
        let label = self.label();
        let val = u64::from_ne_bytes(val.to_ne_bytes());
        self.constants.push((val, label));
        label
    }

    pub fn const_i64(&mut self, val: i64) -> DestLabel {
        let label = self.label();
        let val = val as u64;
        self.constants.push((val, label));
        label
    }

    /// Bind the current location to `label`.
    pub fn bind_label(&mut self, label: DestLabel) {
        self.reloc[label].loc = Some(self.counter);
    }

    /// Bind the current location to `label`.
    pub fn bind_label_to_pos(&mut self, label: DestLabel, pos: usize) {
        self.reloc[label].loc = Some(Pos::from(pos));
    }

    /// Bind the current location to `label`.
    pub fn get_label_pos(&self, label: DestLabel) -> usize {
        self.reloc[label]
            .loc
            .expect("The DestLabel has no position binding.")
            .0
    }

    pub fn get_label_absolute_address(&self, label: DestLabel) -> *const u8 {
        unsafe { self.contents.add(self.get_label_pos(label)) }
    }

    /// Save relocaton slot for `DestLabel`.
    pub fn save_reloc(&mut self, dest: DestLabel, offset: u8) {
        self.reloc[dest].disp.push((offset, self.counter));
    }

    /// Resolve and fill all relocations.
    pub fn fill_relocs(&mut self) {
        let mut reloc = std::mem::take(&mut self.reloc);
        for rel in reloc.iter() {
            if let Some(pos) = rel.loc {
                for (size, dest) in &rel.disp {
                    let disp = pos.0 as i64 - dest.0 as i64 - *size as i64;
                    if i32::min_value() as i64 > disp || disp > i32::max_value() as i64 {
                        panic!("Relocation overflow");
                    }
                    self.write32(*dest, disp as i32);
                }
            }
        }
        reloc.iter_mut().for_each(|reloc| reloc.disp = vec![]);
        self.reloc = reloc;
    }

    /// Resolve labels for constant data, and emit them to `contents`.
    fn resolve_constants(&mut self) {
        let constants = std::mem::take(&mut self.constants);
        for (val, label) in constants {
            self.bind_label(label);
            self.emitq(val);
        }
    }

    pub fn get_label_addr<T, U>(&mut self, label: DestLabel) -> extern "C" fn(T) -> U {
        let counter = self.reloc[label]
            .loc
            .expect("The DestLabel has no position binding.")
            .0;
        let adr = self.contents;
        unsafe { mem::transmute(adr.add(counter)) }
    }

    pub fn get_label_u64(&mut self, label: DestLabel) -> u64 {
        let counter = self.reloc[label]
            .loc
            .expect("The DestLabel has no position binding.")
            .0;
        let adr = self.contents;
        unsafe { mem::transmute(adr.add(counter)) }
    }

    /// Emit bytes.
    pub fn emit(&mut self, slice: &[u8]) {
        slice.iter().for_each(|b| self.emitb(*b));
    }

    /// Emit a byte.
    pub fn emitb(&mut self, val: u8) {
        let c = self.counter;
        self[c] = val;
        self.counter = c + 1;
    }

    /// Emit a word.
    pub fn emitw(&mut self, val: u16) {
        let c = self.counter;
        self[c] = val as u8;
        self[c + 1] = (val >> 8) as u8;
        self.counter = c + 2;
    }

    /// Emit a long word.
    pub fn emitl(&mut self, val: u32) {
        let c = self.counter;
        self[c] = val as u8;
        self[c + 1] = (val >> 8) as u8;
        self[c + 2] = (val >> 16) as u8;
        self[c + 3] = (val >> 24) as u8;
        self.counter = c + 4;
    }

    /// Write 32bit data `val` on `loc`.
    fn write32(&mut self, loc: Pos, val: i32) {
        let val = val as u32;
        self[loc] = val as u8;
        self[loc + 1] = (val >> 8) as u8;
        self[loc + 2] = (val >> 16) as u8;
        self[loc + 3] = (val >> 24) as u8;
    }

    /// Emit a quad word.
    pub fn emitq(&mut self, val: u64) {
        self.emitl(val as u32);
        self.emitl((val >> 32) as u32);
    }
}

#[allow(dead_code)]
impl JitMemory {
    /// Encoding: Opcode +rd  
    /// Op+ rd
    pub fn enc_o(&mut self, op: u8, reg: Reg) {
        assert!(!reg.is_rip());
        self.rex_none(Reg(0), reg, Reg(0));
        self.op_with_rd(op, reg);
    }

    /// Encoding: Opcode +rd  
    /// Op+ rd
    pub fn enc_oi(&mut self, op: u8, reg: Reg) {
        assert!(!reg.is_rip());
        self.rex_none(Reg(0), reg, Reg(0));
        self.op_with_rd(op, reg);
    }

    /// Encoding: Opcode +rd  
    /// REX.W Op+ rd
    pub fn enc_rexw_o(&mut self, op: u8, reg: Reg) {
        assert!(!reg.is_rip());
        self.rexw(Reg(0), reg, Reg(0));
        self.op_with_rd(op, reg);
    }

    /// Encoding: MI  
    /// REX.W Op ModRM:r/m
    pub fn enc_rexw_mi(&mut self, op: u8, rm_op: Or, imm: Imm) {
        self.encode(&[op], Rex::REXW, ModRM::Reg(Reg(0)), rm_op, imm);
    }

    /// Encoding: MI  
    /// Op ModRM:r/m
    pub fn enc_rex_mi(&mut self, op: u8, rm_op: Or, imm: Imm) {
        self.encode(&[op], Rex::None, ModRM::Reg(Reg(0)), rm_op, imm);
    }

    /// REX.W Op ModRM
    /// MR-> ModRM:r/m(w) ModRM:reg(r)
    /// RM-> ModRM:reg(r) ModRM:r/m(w)
    pub fn enc_rexw_mr(&mut self, op: &[u8], reg: Reg, rm_op: Or) {
        self.encode(op, Rex::REXW, ModRM::Reg(reg), rm_op, Imm::None);
    }

    /// REX Op ModRM
    /// MR-> ModRM:r/m(w) ModRM:reg(r)
    /// RM-> ModRM:reg(r) ModRM:r/m(w)
    pub fn enc_rex_mr(&mut self, op: &[u8], reg: Reg, rm_op: Or) {
        self.encode(op, Rex::None, ModRM::Reg(reg), rm_op, Imm::None);
    }

    /// This is used in "setcc r/m8".
    pub fn enc_rex_m(&mut self, op: &[u8], rm: Or) {
        self.encode(op, Rex::REX, ModRM::Reg(Reg(0)), rm, Imm::None);
    }

    /// Encoding: D  
    /// Op cd
    pub fn enc_d(&mut self, op: &[u8], dest: DestLabel) {
        self.emit(op);
        self.save_reloc(dest, 4);
        self.emitl(0);
    }

    /// Encoding: /n  
    /// Op /n
    pub fn enc_digit(&mut self, op: &[u8], reg: Reg, digit: u8) {
        self.rex_none(Reg(0), reg, Reg(0));
        self.emit(op);
        self.modrm(ModRM::Digit(digit), Mode::Reg.encode(), reg);
    }

    /// Encoding: /n  
    /// REX.W Op /n
    pub fn enc_rexw_digit(&mut self, op: &[u8], rm: Or, digit: u8, imm: Imm) {
        self.encode(op, Rex::REXW, ModRM::Digit(digit), rm, imm);
    }

    pub fn enc_rex_digit(&mut self, op: &[u8], rm: Or, digit: u8, imm: Imm) {
        self.encode(op, Rex::None, ModRM::Digit(digit), rm, imm);
    }

    fn encode(&mut self, op: &[u8], rex: Rex, modrm_mode: ModRM, rm: Or, imm: Imm) {
        let reg = match modrm_mode {
            ModRM::Digit(_) => Reg(0),
            ModRM::Reg(r) => r,
        };
        let rex_fn = match rex {
            Rex::REXW => JitMemory::rexw,
            Rex::REX => JitMemory::rex,
            Rex::None => JitMemory::rex_none,
        };
        assert!(!reg.is_rip());
        if rm.base.is_rip() {
            // For rip, only indirect addressing with disp32 ([rip + disp32]) is allowed.
            // [rip] and [rip + disp8] are to be converted to [rip + disp32].
            let rm = Or::rip_ind_from(rm);
            rex_fn(self, reg, rm.base, Reg(0));
            self.emit(op);
            self.modrm(modrm_mode, 0, rm.base);
            self.emit_disp_imm(rm.mode, imm);
        } else if rm.mode != Mode::Reg && (rm.base.0 & 0b111) == 4 {
            // If mode != Reg and r/m == 4/12 (rsp/r12), use SIB.
            match rm.mode {
                Mode::Ind | Mode::InD8(_) | Mode::InD32(_) => {
                    let index = Reg(4); // magic number
                    let scale = 0;
                    let base = rm.base;
                    rex_fn(self, reg, base, index);
                    self.emit(op);
                    self.modrm(modrm_mode, rm.mode.encode(), base);
                    self.sib(scale, index, base);
                    self.emit_disp_imm(rm.mode, imm);
                }
                _ => unimplemented!(),
            }
        } else if rm.mode == Mode::Ind && (rm.base.0 & 0b111) == 5 {
            // If mode == Ind and r/m == 5/13 (rbp/r13), use [rbp/r13 + 0(disp8)].
            rex_fn(self, reg, rm.base, Reg(0));
            let mode = Mode::InD8(0);
            self.emit(op);
            self.modrm(modrm_mode, mode.encode(), rm.base);
            self.emit_disp_imm(mode, imm);
        } else {
            rex_fn(self, reg, rm.base, Reg(0));
            self.emit(op);
            self.modrm(modrm_mode, rm.mode.encode(), rm.base);
            self.emit_disp_imm(rm.mode, imm);
        }
    }
}

impl JitMemory {
    /// ModRM
    /// +-------+---+---+---+---+---+---+---+---+
    /// |  bit  | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
    /// +-------+---+---+---+---+---+---+---+---+
    /// | field |  mod  |    reg    |    r/m    |
    /// +-------+-------+-----------+-----------+
    /// |  rex  |       |     r     |     b     |
    /// +-------+-------+-----------+-----------+
    fn modrm(&mut self, modrm_mode: ModRM, mode: u8, base: Reg) {
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
    fn rexw(&mut self, reg: Reg, base: Reg, index: Reg) {
        let rexw = 0x48 | (reg.0 & 0b1000) >> 1 | (index.0 & 0b1000) >> 2 | (base.0 & 0b1000) >> 3;
        self.emitb(rexw);
    }

    fn rex_none(&mut self, reg: Reg, base: Reg, index: Reg) {
        if reg.0 > 7 || base.0 > 7 || index.0 > 7 {
            self.rex(reg, base, index);
        };
    }

    fn rex(&mut self, reg: Reg, base: Reg, index: Reg) {
        let rex_prefix =
            0x40 | (reg.0 & 0b1000) >> 1 | (index.0 & 0b1000) >> 2 | (base.0 & 0b1000) >> 3;
        self.emitb(rex_prefix);
    }

    fn op_with_rd(&mut self, op: u8, reg: Reg) {
        let op = op | (reg.0 & 0b0111);
        self.emitb(op);
    }

    /// SIB
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
    ///
    fn sib(&mut self, scale: u8, index: Reg, base: Reg) {
        assert!(scale < 4);
        assert!(index.0 < 16);
        let sib = (scale << 6) | ((index.0 & 0b111) << 3) | (base.0 & 0b111);
        self.emitb(sib);
    }

    fn emit_disp_imm(&mut self, mode: Mode, imm: Imm) {
        match mode {
            Mode::InD8(d) => self.emitb(d as u8),
            Mode::InD32(d) => self.emitl(d as u32),
            Mode::IndLabel(label) => {
                self.save_reloc(label, 4 + imm.offset());
                self.emitl(0);
            }
            _ => {}
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
    /// Dump generated code.
    fn dump_code(&self) {
        use std::fs::File;
        use std::process::Command;
        let asm = self.to_vec();
        let mut file = File::create("tmp.bin").unwrap();
        file.write_all(&asm).unwrap();

        let output = Command::new("objdump")
            .args(&[
                "-D",
                "-Mintel,x86-64",
                "-b",
                "binary",
                "-m",
                "i386",
                "tmp.bin",
            ])
            .output();
        let asm = match &output {
            Ok(output) => std::str::from_utf8(&output.stdout).unwrap().to_string(),
            Err(err) => err.to_string(),
        };
        eprintln!("{}", asm);
    }
}
