//-----------------------------------------------------
//
// JIT module runtime
//
//-----------------------------------------------------

use crate::*;
//use monoasm_inst::Reg;

// ---------------------------------------------------------------------------
// JIT memory allocation and W^X handling are architecture-specific and
// live in the backend modules (`x64`, `arm64`) as the `JitProtect` type:
//
//   * `JitProtect::allocate_pages()` reserves the code/data pages,
//   * `ensure_writable` / `set_writable` / `set_executable` toggle the
//     per-thread write permission of MAP_JIT pages, and
//   * `invalidate_icache` synchronizes the I-cache after code is written.
//
// On x86-64 these are no-ops over plain RWX pages; on AArch64 (notably
// Apple Silicon) they drive `pthread_jit_write_protect_np` and the cache
// maintenance instructions. The engine below stays architecture-neutral
// and only calls into `JitProtect`.
// ---------------------------------------------------------------------------

/// Memory manager.
#[derive(Debug)]
pub struct JitMemory {
    /// Current memory page.
    page: Page,
    /// Information of momory pages.
    pages: [MemPage; 3],
    ///
    labels: Vec<DestLabel>,
    /// Architecture-specific JIT page protection / W^X state. Carries the
    /// per-thread writability tracking on macOS/aarch64 and is a
    /// zero-sized no-op on x86-64.
    protect: JitProtect,
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

    pub(crate) fn contents(&self) -> *mut u8 {
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
    pub(crate) fn write32(&mut self, loc: Pos, val: i32) {
        let val = val as u32;
        self[loc] = val as u8;
        self[loc + 1] = (val >> 8) as u8;
        self[loc + 2] = (val >> 16) as u8;
        self[loc + 3] = (val >> 24) as u8;
    }

    /// Write 64bit data `val` on `loc`.
    pub(crate) fn write64(&mut self, loc: Pos, val: u64) {
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
        // All `JitMemory.<emit*>()` / `MemPage` mutation routes go
        // through this deref, so lazily flipping back to writable here
        // covers method-style writes (`jit.emitb`, `jit.label`, …).
        self.ensure_writable();
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
        // `self[page]` is the explicit-page write path used by
        // `write_reloc` / `fill_relocs`; same lazy flip rationale as
        // `DerefMut`.
        self.ensure_writable();
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
        // Byte-level write entry point; ensure W^X is in writable
        // state so callers (incl. MemPage's emit helpers via deref)
        // don't fault on Apple Silicon.
        self.ensure_writable();
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
        let (code_contents, data_contents) = JitProtect::allocate_pages();
        // The pages start out writable for the initial code generation
        // (`JitProtect::new` flips MAP_JIT pages writable on macOS).
        let protect = JitProtect::new();
        let initial_page = MemPage::new(code_contents);
        let second_page = MemPage::new(unsafe { code_contents.add(PAGE_SIZE) });
        let data_page = MemPage::new(data_contents);
        JitMemory {
            page: Page(0),
            pages: [initial_page, second_page, data_page],
            labels: vec![],
            protect,
        }
    }

    /// Ensure the JIT region is writable for the current thread before an
    /// emit / patch operation. On macOS/aarch64, calling
    /// `set_executable()` (or `finalize()`, which delegates to it) leaves
    /// the pages write-protected; the next emit would SIGBUS without this
    /// lazy flip. Outside macOS/aarch64 it is an inlined no-op so the hot
    /// path stays branch-free.
    #[inline]
    fn ensure_writable(&mut self) {
        self.protect.ensure_writable();
    }

    /// Switch JIT memory back to writable mode for the current thread.
    /// On macOS/AArch64 this calls `pthread_jit_write_protect_np(0)`;
    /// elsewhere it is a no-op. Callers don't normally need this — every
    /// emit path lazily flips back to writable via `ensure_writable`.
    pub fn set_writable(&mut self) {
        self.protect.set_writable();
    }

    /// Switch JIT memory to executable mode for the current thread and
    /// synchronize the I-cache so the CPU sees the freshly written
    /// instructions. Called automatically at the end of
    /// [`finalize`](Self::finalize).
    pub fn set_executable(&mut self) {
        self.protect.set_executable();
        // Synchronize the I-cache over the freshly written code so the
        // CPU sees the new instructions (a no-op on x86-64).
        for page in &self.pages[..2] {
            let len = page.code_len.max(page.counter.0);
            if len > 0 {
                unsafe { JitProtect::invalidate_icache(page.contents(), len) }
            }
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
        // Publish the freshly written code: flip MAP_JIT pages to
        // executable on macOS/AArch64 and synchronize the I-cache on
        // AArch64. No-op on x86-64.
        self.set_executable();
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

    /// Resolve `label`'s relocation `target`: patch it immediately if the
    /// label is already bound, otherwise queue it for `finalize`. The
    /// `target` variants and the patching itself
    /// ([`write_reloc`](Self::write_reloc)) are architecture-specific and
    /// defined in the backend modules.
    pub(crate) fn handle_reloc(&mut self, label: DestLabel, target: TargetType) {
        match *label.0.borrow_mut() {
            LabelInfo::Resolved((src_page, src_pos)) => {
                self.write_reloc(src_page, src_pos, target);
            }
            LabelInfo::NotResolved(ref mut targets) => {
                targets.push(target);
            }
        };
    }

    /// The current code page being emitted into.
    pub(crate) fn cur_page(&self) -> Page {
        self.page
    }

    /// The current write cursor within the current page.
    pub(crate) fn cur_pos(&self) -> Pos {
        self.counter
    }

    /// Save an absolute-address relocation slot for `DestLabel`.
    fn emit_absolute_reloc(&mut self, page: Page, dest: DestLabel) {
        let pos = self[page].counter;
        let target = TargetType::Abs { page, pos };
        self[page].emitq(0);
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
