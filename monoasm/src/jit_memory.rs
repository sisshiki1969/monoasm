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
    ptr::NonNull,
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

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(transparent)]
pub struct CodePtr(NonNull<u8>);

impl CodePtr {
    pub fn from(ptr: *mut u8) -> Self {
        Self(NonNull::new(ptr).unwrap())
    }

    pub fn as_ptr(&self) -> *mut u8 {
        self.0.as_ptr()
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Page(pub usize);

/// Memory manager.
#[derive(Debug)]
pub struct JitMemory {
    page: Page,
    pages: Vec<MemPage>,
    /// Relocation information.
    reloc: Relocations,
}

/// Memory manager.
#[derive(Debug)]
pub struct MemPage {
    /// Pointer to the heap.
    contents: *mut u8,
    /// Current position
    counter: Pos,
    /// Constants section.
    constants: Vec<(u64, DestLabel)>,
    /// Machine code length
    code_len: usize,
    /// The top pos of the current code block.
    code_block_top: Pos,
    /// Code blocks. (start_pos, code_end, end_pos)
    pub code_block: Vec<(Pos, Pos, Pos)>,
}

impl MemPage {
    fn new() -> Self {
        let layout = Layout::from_size_align(PAGE_SIZE, 4096).expect("Bad Layout.");
        let contents = unsafe { alloc(layout) };
        unsafe {
            protect(contents, PAGE_SIZE, Protection::READ_WRITE_EXECUTE).expect("Mprotect failed.");
        }
        MemPage {
            contents,
            counter: Pos(0),
            constants: vec![],
            code_len: 0usize,
            code_block_top: Pos(0),
            code_block: vec![],
        }
    }
}

impl std::ops::Deref for JitMemory {
    type Target = MemPage;
    fn deref(&self) -> &Self::Target {
        &self.pages[self.page.0]
    }
}

impl std::ops::DerefMut for JitMemory {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.pages[self.page.0]
    }
}

impl Index<Page> for JitMemory {
    type Output = MemPage;

    fn index(&self, index: Page) -> &MemPage {
        &self.pages[index.0]
    }
}

impl IndexMut<Page> for JitMemory {
    fn index_mut(&mut self, index: Page) -> &mut MemPage {
        &mut self.pages[index.0]
    }
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
    /// This function try to allocate heap memory of 64KB for JIT assemble.
    ///
    /// ### panic
    /// Panic if Layout::from_size_align() or region::protect() returned Err.
    pub fn new() -> JitMemory {
        let mut res = JitMemory {
            page: Page(0),
            pages: vec![MemPage::new()],
            reloc: Relocations::new(),
        };
        res.emitb(0xc3);
        res.counter = Pos(0);
        res
    }

    pub fn add_page(&mut self) {
        self.pages.push(MemPage::new());
    }

    pub fn select(&mut self, page: usize) {
        assert!(page < self.pages.len());
        self.page = Page(page);
    }

    /// Resolve all relocations and return the top addresss of generated machine code as a function pointer.
    pub fn finalize(&mut self) {
        let mut info = vec![];
        for p in 0..self.pages.len() {
            self.select(p);
            let start_pos = self.code_block_top;
            let code_end = self.counter;
            self.code_len = self.counter.0;
            info.push((start_pos, code_end, Pos(0)));
        }
        self.resolve_constants();
        for p in 0..self.pages.len() {
            self.select(p);
            info[p].2 = self.counter;
        }
        self.fill_relocs();
        for p in 0..self.pages.len() {
            self.select(p);
            self.code_block_top = self.counter;
            self.code_block.push(info[p].clone());
        }
    }

    pub fn as_slice(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.contents, self.code_len) }
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
        let label = self.reloc.len();
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
        let p = self.page;
        self.reloc[label].loc = Some((p, self.counter));
    }

    /*/// Bind the current location to `label`.
    pub fn bind_label_to_pos(&mut self, label: DestLabel, pos: usize) {
        self.reloc[label].loc = Some(Pos::from(pos));
    }*/

    /// Bind the current location to `label`.
    fn get_label_pos(&self, label: DestLabel) -> usize {
        self.reloc[label]
            .loc
            .expect("The DestLabel has no position binding.")
            .1
             .0
    }

    pub fn get_current_address(&self) -> CodePtr {
        let ptr = unsafe { self.contents.add(self.counter.0) };
        CodePtr::from(ptr)
    }

    pub fn get_label_address(&self, label: DestLabel) -> CodePtr {
        let ptr = unsafe { self.contents.add(self.get_label_pos(label)) };
        CodePtr::from(ptr)
    }

    /// Save relocaton slot for `DestLabel`.
    pub fn save_reloc(&mut self, dest: DestLabel, offset: u8) {
        let p = self.page;
        let pos = self.counter;
        self.reloc[dest].disp.push((p, offset, pos));
    }

    /// Resolve and fill all relocations.
    pub fn fill_relocs(&mut self) {
        let mut reloc = std::mem::take(&mut self.reloc);
        for rel in reloc.iter() {
            if let Some((src_page, pos)) = rel.loc {
                let src_ptr = self[src_page].contents as usize + pos.0;
                for (dest_page, size, dest) in &rel.disp {
                    let dest_ptr = self[*dest_page].contents as usize + dest.0 + (*size as usize);
                    let disp = (src_ptr as i64) - (dest_ptr as i64);
                    match i32::try_from(disp) {
                        Ok(disp) => {
                            self.select(dest_page.0);
                            self.write32(*dest, disp as i32)
                        }
                        Err(_) => panic!("Relocation overflow"),
                    }
                }
            }
        }
        reloc.iter_mut().for_each(|reloc| reloc.disp = vec![]);
        self.reloc = reloc;
    }

    /// Resolve labels for constant data, and emit them to `contents`.
    fn resolve_constants(&mut self) {
        for p in 0..self.pages.len() {
            self.select(p);
            let constants = std::mem::take(&mut self.constants);
            for (val, label) in constants {
                self.bind_label(label);
                self.emitq(val);
            }
        }
    }

    pub fn get_label_addr<T, U>(&mut self, label: DestLabel) -> extern "C" fn(T) -> U {
        let (page, counter) = self.reloc[label]
            .loc
            .expect("The DestLabel has no position binding.");
        let adr = self[page].contents;
        unsafe { mem::transmute(adr.add(counter.0)) }
    }

    pub fn get_label_addr2<S, T, U>(&mut self, label: DestLabel) -> extern "C" fn(S, T) -> U {
        let (page, counter) = self.reloc[label]
            .loc
            .expect("The DestLabel has no position binding.");
        let adr = self[page].contents;
        unsafe { mem::transmute(adr.add(counter.0)) }
    }

    pub fn get_label_u64(&mut self, label: DestLabel) -> u64 {
        let (page, counter) = self.reloc[label]
            .loc
            .expect("The DestLabel has no position binding.");
        let adr = self[page].contents;
        unsafe { mem::transmute(adr.add(counter.0)) }
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
    pub fn enc_rexw_mi(&mut self, op: u8, rm_op: Rm, imm: Imm) {
        self.encode(&[op], Rex::REXW, ModRM::Reg(Reg(0)), rm_op, imm);
    }

    /// Encoding: MI  
    /// Op ModRM:r/m
    pub fn enc_rex_mi(&mut self, op: u8, rm_op: Rm, imm: Imm) {
        self.encode(&[op], Rex::None, ModRM::Reg(Reg(0)), rm_op, imm);
    }

    /// REX.W Op ModRM
    /// MR-> ModRM:r/m(w) ModRM:reg(r)
    /// RM-> ModRM:reg(r) ModRM:r/m(w)
    pub fn enc_rexw_mr(&mut self, op: &[u8], reg: Reg, rm_op: Rm) {
        self.encode(op, Rex::REXW, ModRM::Reg(reg), rm_op, Imm::None);
    }

    /// REX Op ModRM
    /// MR-> ModRM:r/m(w) ModRM:reg(r)
    /// RM-> ModRM:reg(r) ModRM:r/m(w)
    pub fn enc_rex_mr(&mut self, op: &[u8], reg: Reg, rm_op: Rm) {
        self.encode(op, Rex::None, ModRM::Reg(reg), rm_op, Imm::None);
    }

    /// This is used in "setcc r/m8".
    pub fn enc_rex_m(&mut self, op: &[u8], rm: Rm) {
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
        self.modrm(ModRM::Digit(digit), Mode::Reg, reg);
    }

    /// Encoding: M  
    /// Op /n
    pub fn enc_m_digit(&mut self, op: &[u8], rm: Rm, digit: u8) {
        self.encode(op, Rex::None, ModRM::Digit(digit), rm, Imm::None);
    }

    pub fn enc_m_digit_imm(&mut self, op: &[u8], rm: Rm, digit: u8, imm: Imm) {
        self.encode(op, Rex::None, ModRM::Digit(digit), rm, imm);
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
            Rex::REX => JitMemory::rex,
            Rex::None => JitMemory::rex_none,
        };
        assert!(!reg.is_rip());
        if rm.base.is_rip() {
            // For rip, only indirect addressing with disp32 ([rip + disp32]) is allowed.
            // [rip] and [rip + disp8] are to be converted to [rip + disp32].
            let rm = Rm::rip_ind_from(rm);
            rex_fn(self, reg, rm.base, Reg(0));
            self.emit(op);
            self.modrm(modrm_mode, Mode::Ind(Scale::None, Disp::None), rm.base);
            self.emit_disp_imm(rm.mode.disp(), imm);
        } else if rm.mode != Mode::Reg && (rm.base.0 & 0b111) == 4 {
            // If mode != Reg and r/m == 4/12 (rsp/r12), use SIB.
            match rm.mode {
                Mode::Ind(scale, disp) => {
                    let (scale, index) = match scale {
                        Scale::None => (0, Reg(4)), // magic number
                        Scale::S1(scale, index) => (scale, index),
                    };
                    let base = rm.base;
                    rex_fn(self, reg, base, index);
                    self.emit(op);
                    self.modrm(modrm_mode, rm.mode, base);
                    self.sib(scale, index, base);
                    self.emit_disp_imm(disp, imm);
                }
                _ => unreachable!(),
            }
        } else if rm.mode.is_indirect_no_disp() && (rm.base.0 & 0b111) == 5 {
            // If mode == Ind and r/m == 5/13 (rbp/r13), use [rbp/r13 + 0(disp8)].
            let scale = rm.mode.scale();
            rex_fn(self, reg, rm.base, scale.index());
            let mode = Mode::Ind(scale, Disp::D8(0));
            self.emit(op);
            self.modrm(modrm_mode, mode, rm.base);
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
                match rm.mode {
                    Mode::Reg => Reg(0),
                    Mode::Ind(scale, _) => match scale {
                        Scale::None => Reg(0),
                        Scale::S1(_, index) => index,
                    },
                },
            );
            // index != Reg::RIP
            self.emit(op);
            self.modrm(modrm_mode, rm.mode, rm.base);
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
                self.save_reloc(label, 4 + imm.offset());
                self.emitl(0);
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
    /// Dump generated code.
    pub fn dump_code(&self) -> Result<String, std::io::Error> {
        use std::fs::File;
        use std::process::Command;
        let asm = self.as_slice();
        let mut file = File::create("tmp.bin").unwrap();
        let (start_pos, code_end, _end_pos) = self.code_block.last().unwrap();
        file.write_all(&asm[start_pos.0..code_end.0]).unwrap();

        Command::new("objdump")
            .args(&[
                "-D",
                "-Mintel,x86-64",
                "-b",
                "binary",
                "-m",
                "i386",
                "tmp.bin",
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
