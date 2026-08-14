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
    /// Low watermark of the byte range written since the last
    /// `set_executable()`. See [`MemPage::mark_dirty_pos`].
    dirty_lo: usize,
    /// High watermark (exclusive) of the byte range written since the last
    /// `set_executable()`. The range is empty iff `dirty_lo >= dirty_hi`.
    dirty_hi: usize,
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
        // Single byte-level chokepoint for every write into this page
        // (`emit*`, `write32`/`write64`, and direct `page[pos] = b`), so
        // recording the watermark here is enough to make `set_executable`
        // flush exactly what was touched.
        self.mark_dirty_pos(index.0, 1);
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
            dirty_lo: usize::MAX,
            dirty_hi: 0,
        }
    }

    pub(crate) fn contents(&self) -> *mut u8 {
        self.contents as *mut u8
    }

    /// Widen the dirty watermark to cover `[pos, pos + len)`.
    ///
    /// The watermark bounds everything written into this page since the
    /// last [`JitMemory::set_executable`], and is what gets handed to
    /// `invalidate_icache`. Keep this branch-free: it runs once per byte
    /// written on the emit hot path.
    #[inline]
    pub(crate) fn mark_dirty_pos(&mut self, pos: usize, len: usize) {
        self.dirty_lo = self.dirty_lo.min(pos);
        self.dirty_hi = self.dirty_hi.max(pos + len);
    }

    /// The byte range written since the last `set_executable()`, or `None`
    /// if nothing was written.
    #[inline]
    fn take_dirty(&mut self) -> Option<(usize, usize)> {
        let (lo, hi) = (self.dirty_lo, self.dirty_hi);
        self.dirty_lo = usize::MAX;
        self.dirty_hi = 0;
        if lo < hi {
            Some((lo, hi))
        } else {
            None
        }
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
        let page = self.page;
        self.pages[page.0].mark_dirty_pos(index.0, 1);
        unsafe { &mut *self.pages[page.0].contents().add(index.0) }
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
        // Synchronize the I-cache over the bytes actually written since the
        // previous `set_executable()` (a no-op on x86-64).
        //
        // Flushing each page in full instead would cost O(total code
        // emitted so far) per call, i.e. O(N^2) over a process that
        // finalizes N times — which is exactly what made repeated JIT
        // compilation crawl on Apple Silicon, where `sys_icache_invalidate`
        // costs time proportional to the range.
        for (id, page) in self.pages.iter_mut().enumerate() {
            // Every page's watermark is reset, but only the code pages get
            // cache maintenance — the data page is never executed.
            let dirty = page.take_dirty();
            if id < DATA_PAGE.0 {
                if let Some((lo, hi)) = dirty {
                    unsafe { JitProtect::invalidate_icache(page.contents().add(lo), hi - lo) }
                }
            }
        }
    }

    /// Record that `len` bytes at `ptr` were written into the JIT region
    /// through a raw pointer, bypassing the [`MemPage`] write helpers.
    ///
    /// Writes that go through `emit*`, `write32`/`write64` or `page[pos]`
    /// mark themselves; this is the escape hatch for code that patches
    /// already-emitted instructions via an address obtained from
    /// [`get_label_address`](Self::get_label_address) or
    /// [`get_current_address`](Self::get_current_address). Without it the
    /// patch would not be part of the range handed to `invalidate_icache`
    /// by the next [`set_executable`](Self::set_executable), and an
    /// AArch64 core could keep executing the stale instruction.
    ///
    /// A `ptr` outside the JIT pages is ignored.
    pub fn mark_dirty(&mut self, ptr: *const u8, len: usize) {
        let p = ptr as usize;
        for page in &mut self.pages {
            let base = page.contents() as usize;
            if p >= base && p - base < PAGE_SIZE {
                page.mark_dirty_pos(p - base, len);
                return;
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

#[cfg(test)]
mod tests {
    use super::*;

    fn dirty(jit: &JitMemory, page: Page) -> Option<(usize, usize)> {
        let page = &jit.pages[page.0];
        if page.dirty_lo < page.dirty_hi {
            Some((page.dirty_lo, page.dirty_hi))
        } else {
            None
        }
    }

    /// The whole point of the watermark: consecutive publishes must flush
    /// only what each one wrote, not the page from its start.
    #[test]
    fn dirty_range_covers_only_the_new_writes() {
        let mut jit = JitMemory::new();
        assert_eq!(None, dirty(&jit, Page(0)));

        jit.emitl(0xdead_beef);
        assert_eq!(Some((0, 4)), dirty(&jit, Page(0)));

        jit.set_executable();
        assert_eq!(None, dirty(&jit, Page(0)));

        jit.emitl(0x1234_5678);
        assert_eq!(Some((4, 8)), dirty(&jit, Page(0)));

        jit.set_executable();
        // A publish with nothing written in between flushes nothing.
        jit.set_executable();
        assert_eq!(None, dirty(&jit, Page(0)));
    }

    /// Patching already-published code (late label binds, stub / inline
    /// cache updates) must re-dirty the patched bytes — this is the case a
    /// naive "flush the current code block" scheme would miss.
    #[test]
    fn patching_published_code_re_dirties_it() {
        let mut jit = JitMemory::new();
        for _ in 0..64 {
            jit.emitb(0);
        }
        jit.set_executable();
        assert_eq!(None, dirty(&jit, Page(0)));

        jit[Page(0)].write32(Pos(16), 0x0011_2233);
        assert_eq!(Some((16, 20)), dirty(&jit, Page(0)));

        jit.set_executable();
        jit[Page(0)].write64(Pos(40), 0);
        assert_eq!(Some((40, 48)), dirty(&jit, Page(0)));
    }

    /// Each page keeps its own range, so touching one doesn't make the
    /// other pay for it.
    #[test]
    fn dirty_ranges_are_per_page() {
        let mut jit = JitMemory::new();
        jit.select_page(1);
        jit.emitl(0);
        assert_eq!(None, dirty(&jit, Page(0)));
        assert_eq!(Some((0, 4)), dirty(&jit, Page(1)));

        jit.set_executable();
        assert_eq!(None, dirty(&jit, Page(1)));
    }

    /// `mark_dirty` is the escape hatch for raw-pointer patches; it has to
    /// resolve the address to the right page, and ignore foreign pointers.
    #[test]
    fn mark_dirty_resolves_addresses_to_pages() {
        let mut jit = JitMemory::new();
        jit.set_executable();

        let p0 = unsafe { jit[Page(0)].contents().add(8) };
        let p1 = unsafe { jit[Page(1)].contents().add(32) };
        jit.mark_dirty(p0, 4);
        jit.mark_dirty(p1, 4);
        assert_eq!(Some((8, 12)), dirty(&jit, Page(0)));
        assert_eq!(Some((32, 36)), dirty(&jit, Page(1)));

        jit.set_executable();
        let outside = &0u8 as *const u8;
        jit.mark_dirty(outside, 1);
        assert_eq!(None, dirty(&jit, Page(0)));
        assert_eq!(None, dirty(&jit, Page(1)));
    }

    /// The data page is never executed, but its watermark must still be
    /// reset so it doesn't grow unboundedly across finalizes.
    #[test]
    fn data_page_watermark_is_reset() {
        let mut jit = JitMemory::new();
        jit[DATA_PAGE].emitq(0);
        assert_eq!(Some((0, 8)), dirty(&jit, DATA_PAGE));
        jit.set_executable();
        assert_eq!(None, dirty(&jit, DATA_PAGE));
    }
}
