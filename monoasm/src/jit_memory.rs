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

/// Memory manager.
#[derive(Debug)]
pub struct JitMemory {
    /// Current memory page.
    page: Page,
    /// Information of momory pages.
    pages: [MemPage; 3],
    /// Relocation information.
    labels: Labels,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Page(pub usize);

const DATA_PAGE: Page = Page(2);

///
/// Memory manager.
///
#[derive(Debug)]
pub struct MemPage {
    /// Pointer to the heap.
    contents: usize,
    /// Current position
    counter: Pos,
    /// Constants section.
    constants: Vec<(DataType, DestLabel)>,
    /// Data section.
    data: Vec<(DataType, DestLabel)>,
    /// Machine code length
    code_len: usize,
    /// The top pos of the current code block.
    code_block_top: Pos,
    /// Code blocks. (start_pos, code_end, end_pos)
    pub code_block: Vec<(Pos, Pos, Pos)>,
}

impl Index<Pos> for MemPage {
    type Output = u8;

    fn index(&self, index: Pos) -> &u8 {
        if index.0 >= PAGE_SIZE {
            panic!("Page size overflow")
        }
        unsafe { &*self.contents().add(index.0) }
    }
}

impl IndexMut<Pos> for MemPage {
    fn index_mut(&mut self, index: Pos) -> &mut u8 {
        if index.0 >= PAGE_SIZE {
            panic!("Page size overflow")
        }
        unsafe { &mut *self.contents().add(index.0) }
    }
}

impl MemPage {
    fn new(contents: *mut u8) -> Self {
        MemPage {
            contents: contents as usize,
            counter: Pos(0),
            constants: vec![],
            data: vec![],
            code_len: 0usize,
            code_block_top: Pos(0),
            code_block: vec![],
        }
    }

    fn contents(&self) -> *mut u8 {
        self.contents as *mut u8
    }

    /// Adjust cursor with 4KB alignment.
    pub fn align_page(&mut self) {
        self.counter = Pos((self.counter.0 + 4095) & !0b1111_1111_1111);
    }

    /// Adjust cursor with 16 byte alignment.
    pub fn align16(&mut self) {
        self.counter = Pos((self.counter.0 + 15) & !0b1111);
    }

    /// Adjust cursor with 8 byte alignment.
    pub fn align8(&mut self) {
        self.counter = Pos((self.counter.0 + 7) & !0b111);
    }

    /// Adjust cursor with 4 byte alignment.
    pub fn align4(&mut self) {
        self.counter = Pos((self.counter.0 + 3) & !0b11);
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

    /// Emit a quad word.
    pub fn emitq(&mut self, val: u64) {
        self.emitl(val as u32);
        self.emitl((val >> 32) as u32);
    }

    /// Write 32bit data `val` on `loc`.
    fn write32(&mut self, loc: Pos, val: i32) {
        let val = val as u32;
        self[loc] = val as u8;
        self[loc + 1] = (val >> 8) as u8;
        self[loc + 2] = (val >> 16) as u8;
        self[loc + 3] = (val >> 24) as u8;
    }

    /// Write 64bit data `val` on `loc`.
    fn write64(&mut self, loc: Pos, val: u64) {
        self[loc] = val as u8;
        self[loc + 1] = (val >> 8) as u8;
        self[loc + 2] = (val >> 16) as u8;
        self[loc + 3] = (val >> 24) as u8;
        self[loc + 4] = (val >> 32) as u8;
        self[loc + 5] = (val >> 40) as u8;
        self[loc + 6] = (val >> 48) as u8;
        self[loc + 7] = (val >> 56) as u8;
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
    Byte,
}

#[derive(Debug, Clone, Copy)]
enum DataType {
    U64(u64),
    U32(u32),
    Bytes(usize),
    AbsAddress(DestLabel),
    Align8,
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
        unsafe { &*self.contents().add(index.0) }
    }
}

impl IndexMut<Pos> for JitMemory {
    fn index_mut(&mut self, index: Pos) -> &mut u8 {
        if index.0 >= PAGE_SIZE {
            panic!("Page size overflow")
        }
        unsafe { &mut *self.contents().add(index.0) }
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
        let layout = Layout::from_size_align(PAGE_SIZE * 3, PAGE_SIZE).expect("Bad Layout.");
        let contents = unsafe { alloc(layout) };
        unsafe {
            protect(contents, PAGE_SIZE * 2, Protection::READ_WRITE_EXECUTE)
                .expect("Mprotect failed.");
            protect(
                contents.add(PAGE_SIZE * 2),
                PAGE_SIZE,
                Protection::READ_WRITE,
            )
            .expect("Mprotect failed.");
        }
        let initial_page = MemPage::new(contents);
        let second_page = MemPage::new(unsafe { contents.add(PAGE_SIZE) });
        let data_page = MemPage::new(unsafe { contents.add(PAGE_SIZE * 2) });
        JitMemory {
            page: Page(0),
            pages: [initial_page, second_page, data_page],
            labels: Labels::new(),
        }
    }

    pub fn select_page(&mut self, page: usize) {
        assert!(page < 2);
        self.page = Page(page);
    }

    pub fn get_page(&self) -> usize {
        self.page.0
    }

    pub fn include(&self, ptr: *mut u8) -> bool {
        self.contents() <= ptr && ptr < unsafe { self.contents().add(PAGE_SIZE * 2) }
    }

    /// Resolve all relocations and return the top addresss of generated machine code as a function pointer.
    pub fn finalize(&mut self) {
        for page in &mut self.pages {
            let start_pos = page.code_block_top;
            let code_end = page.counter;
            page.code_len = page.counter.0;
            page.code_block.push((start_pos, code_end, Pos(0)));
        }
        self.resolve_constants();
        self.resolve_data();
        for page in &mut self.pages {
            page.code_block.last_mut().unwrap().2 = page.counter;
        }
        self.fill_relocs();
        for page in &mut self.pages {
            page.code_block_top = page.counter;
        }
    }

    pub fn as_slice(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.contents(), self.code_len) }
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
        self.labels.new_label()
    }

    pub fn const_f64(&mut self, val: f64) -> DestLabel {
        let label = self.label();
        let val = u64::from_ne_bytes(val.to_ne_bytes());
        self.constants.push((DataType::U64(val), label));
        label
    }

    pub fn const_i64(&mut self, val: i64) -> DestLabel {
        let label = self.label();
        let val = val as u64;
        self.constants.push((DataType::U64(val), label));
        label
    }

    pub fn data_i64(&mut self, val: i64) -> DestLabel {
        let label = self.label();
        let val = val as u64;
        self.data.push((DataType::U64(val), label));
        label
    }

    pub fn const_i32(&mut self, val: i32) -> DestLabel {
        let label = self.label();
        let val = val as u32;
        self.constants.push((DataType::U32(val), label));
        label
    }

    pub fn data_i32(&mut self, val: i32) -> DestLabel {
        let label = self.label();
        let val = val as u32;
        self.data.push((DataType::U32(val), label));
        label
    }

    pub fn constant(&mut self, size: usize) -> DestLabel {
        let label = self.label();
        self.constants.push((DataType::Bytes(size), label));
        label
    }

    pub fn data(&mut self, size: usize) -> DestLabel {
        let label = self.label();
        self.data.push((DataType::Bytes(size), label));
        label
    }

    pub fn abs_address(&mut self, addr_label: DestLabel) -> DestLabel {
        let label = self.label();
        self.constants
            .push((DataType::AbsAddress(addr_label), label));
        label
    }

    pub fn const_align8(&mut self) -> DestLabel {
        let label = self.label();
        self.constants.push((DataType::Align8, label));
        label
    }

    /// Bind the current location to `label`.
    pub fn bind_label(&mut self, label: DestLabel) {
        let page = self.page;
        self.labels[label].loc = Some((page, self.counter));
    }

    pub fn bind_label_with_page(&mut self, page: Page, label: DestLabel) {
        self.labels[label].loc = Some((page, self[page].counter));
    }

    /*/// Bind the current location to `label`.
    pub fn bind_label_to_pos(&mut self, label: DestLabel, pos: usize) {
        self.reloc[label].loc = Some(Pos::from(pos));
    }*/

    /// Bind the current location to `label`.
    fn get_label_pos(&self, label: DestLabel) -> (Page, Pos) {
        self.labels[label]
            .loc
            .expect("The DestLabel has no position binding.")
    }

    pub fn get_current_address(&self) -> CodePtr {
        let ptr = unsafe { self.contents().add(self.counter.0) };
        CodePtr::from(ptr)
    }

    pub fn get_label_address(&self, label: DestLabel) -> CodePtr {
        let (page, pos) = self.get_label_pos(label);
        let ptr = unsafe { self[page].contents().add(pos.0) };
        CodePtr::from(ptr)
    }

    /// Save relocaton slot for `DestLabel`.
    pub fn save_reloc(&mut self, dest: DestLabel, offset: u8) {
        let page = self.page;
        let pos = self.counter;
        self.labels[dest]
            .target
            .push(TargetType::Rel { page, offset, pos });
    }

    /// Save relocaton slot for `DestLabel`.
    fn save_absolute_reloc(&mut self, page: Page, dest: DestLabel) {
        let pos = self[page].counter;
        self.labels[dest].target.push(TargetType::Abs { page, pos });
    }

    /// Resolve and fill all relocations.
    fn fill_relocs(&mut self) {
        let mut reloc = std::mem::take(&mut self.labels);
        for rel in reloc.iter_mut() {
            if let Some((src_page, src_pos)) = rel.loc {
                let src_ptr = self[src_page].contents + src_pos.0;
                for target in std::mem::take(&mut rel.target) {
                    match target {
                        TargetType::Rel { page, offset, pos } => {
                            let target_ptr = self[page].contents + pos.0 + (offset as usize);
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
        }
        self.labels = reloc;
    }

    /// Resolve labels for constant data, and emit them to `contents`.
    fn resolve_constants(&mut self) {
        for id in 0..2 {
            let constants = std::mem::take(&mut self[Page(id)].constants);
            for (c, const_label) in constants {
                match c {
                    DataType::U64(val) => {
                        self[Page(id)].align16();
                        self.bind_label_with_page(Page(id), const_label);
                        self[Page(id)].emitq(val);
                    }
                    DataType::U32(val) => {
                        self[Page(id)].align4();
                        self.bind_label_with_page(Page(id), const_label);
                        self[Page(id)].emitl(val);
                    }
                    DataType::Bytes(size) => {
                        self[Page(id)].align16();
                        self.bind_label_with_page(Page(id), const_label);
                        for _ in 0..size {
                            self[Page(id)].emitb(0);
                        }
                    }
                    DataType::AbsAddress(label) => {
                        self[Page(id)].align8();
                        self.bind_label_with_page(Page(id), const_label);
                        self.save_absolute_reloc(Page(id), label);
                        self[Page(id)].emitq(0);
                    }
                    DataType::Align8 => {
                        self[Page(id)].align8();
                        self.bind_label_with_page(Page(id), const_label);
                    }
                }
            }
        }
    }

    /// Resolve labels for data, and emit them to the data page.
    fn resolve_data(&mut self) {
        for id in 0..2 {
            let data = std::mem::take(&mut self[Page(id)].data);
            for (c, data_label) in data {
                match c {
                    DataType::U64(val) => {
                        self[DATA_PAGE].align16();
                        self.bind_label_with_page(DATA_PAGE, data_label);
                        self[DATA_PAGE].emitq(val);
                    }
                    DataType::U32(val) => {
                        self[DATA_PAGE].align4();
                        self.bind_label_with_page(DATA_PAGE, data_label);
                        self[DATA_PAGE].emitl(val);
                    }
                    DataType::Bytes(size) => {
                        self[DATA_PAGE].align16();
                        self.bind_label_with_page(DATA_PAGE, data_label);
                        for _ in 0..size {
                            self[DATA_PAGE].emitb(0);
                        }
                    }
                    DataType::AbsAddress(label) => {
                        self[DATA_PAGE].align8();
                        self.bind_label_with_page(DATA_PAGE, data_label);
                        self.save_absolute_reloc(DATA_PAGE, label);
                        self[DATA_PAGE].emitq(0);
                    }
                    DataType::Align8 => {
                        self[DATA_PAGE].align8();
                        self.bind_label_with_page(DATA_PAGE, data_label);
                    }
                }
            }
        }
    }

    pub fn get_label_addr<T, U>(&mut self, label: DestLabel) -> extern "C" fn(T) -> U {
        let (page, counter) = self.labels[label]
            .loc
            .expect("The DestLabel has no position binding.");
        let adr = self[page].contents();
        unsafe { mem::transmute(adr.add(counter.0)) }
    }

    pub fn get_label_addr2<S, T, U>(&mut self, label: DestLabel) -> extern "C" fn(S, T) -> U {
        let (page, counter) = self.labels[label]
            .loc
            .expect("The DestLabel has no position binding.");
        let adr = self[page].contents();
        unsafe { mem::transmute(adr.add(counter.0)) }
    }

    pub fn get_label_u64(&mut self, label: DestLabel) -> u64 {
        let (page, counter) = self.labels[label]
            .loc
            .expect("The DestLabel has no position binding.");
        let adr = self[page].contents();
        unsafe { adr.add(counter.0) as u64 }
    }

    /// Emit bytes.
    pub fn emit(&mut self, slice: &[u8]) {
        slice.iter().for_each(|b| self.emitb(*b));
    }

    ///
    /// Apply patch for the displacement of the jmp instruction in *patch_point*.
    ///
    pub fn apply_jmp_patch(&mut self, patch_point: DestLabel, jmp_dest: DestLabel) {
        let patch_point = self.get_label_address(patch_point);
        self.apply_jmp_patch_address(patch_point, jmp_dest);
    }

    ///
    /// Apply patch for the displacement of the jmp instruction in *patch_point*.
    ///
    pub fn apply_jmp_patch_address(&mut self, patch_point: CodePtr, jmp_dest: DestLabel) {
        let jmp_dest = self.get_label_address(jmp_dest);
        let offset = jmp_dest - patch_point - 5;
        unsafe { *(patch_point.as_ptr().add(1) as *mut [u8; 4]) = (offset as i32).to_ne_bytes() };
    }
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
    /// REX.W Op ModRM:r/m
    pub fn enc_rexw_mi(&mut self, op: u8, rm_op: Rm, imm: Imm) {
        self.encode(&[op], Rex::REXW, ModRM::Reg(Reg(0)), rm_op, imm);
    }

    /// Encoding: MI  
    /// Op ModRM:r/m
    pub fn enc_rex_mi(&mut self, op: u8, rm_op: Rm, imm: Imm) {
        self.encode(&[op], Rex::None, ModRM::Reg(Reg(0)), rm_op, imm);
    }

    pub fn enc_rex_mi_byte(&mut self, op: u8, rm_op: Rm, imm: Imm) {
        self.encode(&[op], Rex::Byte, ModRM::Reg(Reg(0)), rm_op, imm);
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

    pub fn enc_rex_mr_byte(&mut self, op: &[u8], reg: Reg, rm_op: Rm) {
        self.encode(op, Rex::Byte, ModRM::Reg(reg), rm_op, Imm::None);
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

    pub fn enc_rex_digit(&mut self, op: &[u8], rm: Rm, digit: u8, imm: Imm) {
        self.encode(op, Rex::REX, ModRM::Digit(digit), rm, imm);
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
            Rex::Byte => JitMemory::rex_none_byte,
        };
        assert!(!reg.is_rip());
        if rm.base.is_rip() {
            // For rip, only indirect addressing with disp32 ([rip + disp32]) is allowed.
            // [rip] and [rip + disp8] are to be converted to [rip + disp32].
            let rm = Rm::rip_ind_from(rm);
            rex_fn(self, reg, rm.base, Reg(0), rm.mode);
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
                    rex_fn(self, reg, base, index, rm.mode);
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
            rex_fn(self, reg, rm.base, scale.index(), rm.mode);
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
                rm.mode,
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
