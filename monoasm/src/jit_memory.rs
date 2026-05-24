//-----------------------------------------------------
//
// JIT module runtime
//
//-----------------------------------------------------

use crate::*;
//use monoasm_inst::Reg;
use region::{protect, Protection};
use std::alloc::{alloc, Layout};

/// Memory manager.
#[derive(Debug)]
pub struct JitMemory {
    /// Current memory page.
    page: Page,
    /// Information of momory pages.
    pages: [MemPage; 3],
    ///
    labels: Vec<DestLabel>,
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

#[derive(Debug, Clone)]
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
            labels: vec![],
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
        let label = DestLabel::new();
        self.labels.push(label.clone());
        label
    }

    pub fn const_f64(&mut self, val: f64) -> DestLabel {
        let label = self.label();
        let val = u64::from_ne_bytes(val.to_ne_bytes());
        self.constants.push((DataType::U64(val), label.clone()));
        label
    }

    pub fn const_i64(&mut self, val: i64) -> DestLabel {
        let label = self.label();
        let val = val as u64;
        self.constants.push((DataType::U64(val), label.clone()));
        label
    }

    pub fn data_i64(&mut self, val: i64) -> DestLabel {
        let label = self.label();
        let val = val as u64;
        self.data.push((DataType::U64(val), label.clone()));
        label
    }

    pub fn const_i32(&mut self, val: i32) -> DestLabel {
        let label = self.label();
        let val = val as u32;
        self.constants.push((DataType::U32(val), label.clone()));
        label
    }

    pub fn data_i32(&mut self, val: i32) -> DestLabel {
        let label = self.label();
        let val = val as u32;
        self.data.push((DataType::U32(val), label.clone()));
        label
    }

    pub fn constant(&mut self, size: usize) -> DestLabel {
        let label = self.label();
        self.constants.push((DataType::Bytes(size), label.clone()));
        label
    }

    pub fn data(&mut self, size: usize) -> DestLabel {
        let label = self.label();
        self.data.push((DataType::Bytes(size), label.clone()));
        label
    }

    pub fn abs_address(&mut self, addr_label: DestLabel) -> DestLabel {
        let label = self.label();
        self.constants
            .push((DataType::AbsAddress(addr_label), label.clone()));
        label
    }

    pub fn const_align8(&mut self) -> DestLabel {
        let label = self.label();
        self.constants.push((DataType::Align8, label.clone()));
        label
    }

    /// Bind the current location to `label`.
    pub fn bind_label(&mut self, label: DestLabel) {
        let src_page = self.page;
        self.bind_label_with_page(src_page, label);
    }

    pub fn bind_label_with_page(&mut self, src_page: Page, mut label: DestLabel) {
        let src_pos = self[src_page].counter;
        match label.bind(src_page, src_pos) {
            LabelInfo::Resolved(_) => panic!("The DestLabel has already been resolved."),
            LabelInfo::NotResolved(targets) => {
                for target in targets {
                    self.write_reloc(src_page, src_pos, target);
                }
            }
        }
    }

    pub fn get_current_address(&self) -> CodePtr {
        let ptr = unsafe { self.contents().add(self.counter.0) };
        CodePtr::from(ptr)
    }

    pub fn get_label_address(&self, label: &DestLabel) -> CodePtr {
        let (page, pos) = label.loc();
        let ptr = unsafe { self[page].contents().add(pos.0) };
        CodePtr::from(ptr)
    }

    fn handle_reloc(&mut self, label: DestLabel, target: TargetType) {
        match *label.0.borrow_mut() {
            LabelInfo::Resolved((src_page, src_pos)) => {
                self.write_reloc(src_page, src_pos, target);
            }
            LabelInfo::NotResolved(ref mut targets) => {
                targets.push(target);
            }
        };
    }

    /// Save relocaton slot for `DestLabel`.
    #[cfg(target_arch = "x86_64")]
    pub fn emit_reloc(&mut self, dest: DestLabel, offset: u8) {
        let page = self.page;
        let pos = self.counter;
        let target = TargetType::Rel { page, offset, pos };
        self.emitl(0);
        self.handle_reloc(dest, target);
    }

    /// Save relocaton slot for `DestLabel`.
    fn emit_absolute_reloc(&mut self, page: Page, dest: DestLabel) {
        let pos = self[page].counter;
        let target = TargetType::Abs { page, pos };
        self[page].emitq(0);
        self.handle_reloc(dest, target);
    }

    fn write_reloc(&mut self, src_page: Page, src_pos: Pos, target: TargetType) {
        let src_ptr = self[src_page].contents + src_pos.0;
        match target {
            #[cfg(target_arch = "x86_64")]
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
            #[cfg(target_arch = "aarch64")]
            TargetType::Arm64 { page, pos, kind } => {
                // AArch64 branches are relative to the address of the
                // branch instruction itself, and the displacement is
                // packed into the bitfields of the existing instruction
                // word (rather than a separate displacement slot).
                let branch_ptr = self[page].contents + pos.0;
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

    /// Emit an AArch64 PC-relative branch/ADR instruction whose target
    /// is a [`DestLabel`]. `base_word` is the fully-encoded instruction
    /// with a zeroed immediate field; `kind` describes how the
    /// displacement is later patched in.
    #[cfg(target_arch = "aarch64")]
    pub fn emit_arm64_branch(&mut self, base_word: u32, kind: crate::Arm64Reloc, dest: DestLabel) {
        let page = self.page;
        let pos = self.counter;
        self.emitl(base_word);
        let target = TargetType::Arm64 { page, pos, kind };
        self.handle_reloc(dest, target);
    }

    /// Resolve and fill all relocations.
    fn fill_relocs(&mut self) {
        self.labels
            .retain(|label| matches!(&*label.0.borrow(), LabelInfo::NotResolved(_)));
        //for label in std::mem::take(&mut self.labels) {
        //    match &*label.0.borrow() {
        //        LabelInfo::Resolved(_) => {}
        //        LabelInfo::NotResolved(targets) => {
        //            assert!(targets.is_empty());
        //        }
        //    }
        //}
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
                        self.emit_absolute_reloc(Page(id), label);
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
                        self.emit_absolute_reloc(DATA_PAGE, label);
                    }
                    DataType::Align8 => {
                        self[DATA_PAGE].align8();
                        self.bind_label_with_page(DATA_PAGE, data_label);
                    }
                }
            }
        }
    }

    fn addr(&self, label: &DestLabel) -> *mut u8 {
        let (page, counter) = label.loc();
        let adr = self[page].contents();
        unsafe { adr.add(counter.0) }
    }

    pub fn get_label_addr<T, U>(&mut self, label: &DestLabel) -> extern "C" fn(T) -> U {
        unsafe { mem::transmute(self.addr(label)) }
    }

    pub fn get_label_addr2<S, T, U>(&mut self, label: &DestLabel) -> extern "C" fn(S, T) -> U {
        unsafe { mem::transmute(self.addr(label)) }
    }

    pub fn get_label_u64(&mut self, label: &DestLabel) -> u64 {
        self.addr(label) as u64
    }

    /// Emit bytes.
    pub fn emit(&mut self, slice: &[u8]) {
        slice.iter().for_each(|b| self.emitb(*b));
    }
}
