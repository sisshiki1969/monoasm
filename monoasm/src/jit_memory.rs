//-----------------------------------------------------
//
// JIT module runtime
//
//-----------------------------------------------------

use crate::*;
//use monoasm_inst::Reg;
#[cfg(not(all(target_arch = "aarch64", target_os = "macos")))]
use region::{protect, Protection};
use std::alloc::{alloc, Layout};

// ---------------------------------------------------------------------------
// Platform-specific JIT memory allocation and W^X handling.
//
// Apple Silicon (aarch64-apple-darwin) rejects RWX heap pages: JIT memory
// must be allocated with `mmap(..., MAP_JIT, ...)` and the per-thread write
// permission is toggled via `pthread_jit_write_protect_np`. After writing
// code, `sys_icache_invalidate` (or equivalent cache maintenance) must be
// run so the CPU sees the freshly written instructions. On Linux/AArch64
// the toggle is a no-op but the cache maintenance is still required; on
// x86-64 both are no-ops.
// ---------------------------------------------------------------------------

#[cfg(all(target_arch = "aarch64", target_os = "macos"))]
mod apple_jit {
    use libc::{
        c_void, mmap, MAP_ANON, MAP_FAILED, MAP_JIT, MAP_PRIVATE, PROT_EXEC, PROT_READ, PROT_WRITE,
    };

    extern "C" {
        fn pthread_jit_write_protect_np(enabled: i32);
        pub fn sys_icache_invalidate(addr: *mut c_void, len: usize);
    }

    /// Map `size` bytes of MAP_JIT memory. As far as the MMU is
    /// concerned the mapping is RWX, but the *per-thread* writability is
    /// gated by [`set_writable`] / [`set_executable`].
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

    #[inline]
    pub fn set_writable() {
        unsafe { pthread_jit_write_protect_np(0) }
    }

    #[inline]
    pub fn set_executable() {
        unsafe { pthread_jit_write_protect_np(1) }
    }
}

/// Toggle MAP_JIT pages to writable for the current thread (macOS/AArch64);
/// no-op elsewhere.
#[inline]
fn flip_writable() {
    #[cfg(all(target_arch = "aarch64", target_os = "macos"))]
    apple_jit::set_writable();
}

/// Toggle MAP_JIT pages to executable for the current thread
/// (macOS/AArch64); no-op elsewhere.
#[inline]
fn flip_executable() {
    #[cfg(all(target_arch = "aarch64", target_os = "macos"))]
    apple_jit::set_executable();
}

/// Allocate the two contiguous code pages plus a separate data page,
/// returning `(code_pages_base, data_page_base)`.
#[cfg(all(target_arch = "aarch64", target_os = "macos"))]
fn allocate_pages() -> (*mut u8, *mut u8) {
    let code = unsafe { apple_jit::alloc(PAGE_SIZE * 2) };
    let data_layout = Layout::from_size_align(PAGE_SIZE, PAGE_SIZE).expect("Bad Layout.");
    let data = unsafe { alloc(data_layout) };
    (code, data)
}

#[cfg(not(all(target_arch = "aarch64", target_os = "macos")))]
fn allocate_pages() -> (*mut u8, *mut u8) {
    let layout = Layout::from_size_align(PAGE_SIZE * 3, PAGE_SIZE).expect("Bad Layout.");
    let contents = unsafe { alloc(layout) };
    unsafe {
        protect(contents, PAGE_SIZE * 2, Protection::READ_WRITE_EXECUTE).expect("Mprotect failed.");
        protect(
            contents.add(PAGE_SIZE * 2),
            PAGE_SIZE,
            Protection::READ_WRITE,
        )
        .expect("Mprotect failed.");
    }
    (contents, unsafe { contents.add(PAGE_SIZE * 2) })
}

/// Synchronize the instruction and data caches over `[ptr, ptr+len)`.
/// Required on AArch64 after writing generated code so the CPU sees the
/// new instructions; x86-64 has coherent I-caches so this is a no-op.
#[inline]
#[allow(unused_variables)]
unsafe fn invalidate_icache(ptr: *const u8, len: usize) {
    #[cfg(all(target_arch = "aarch64", target_os = "macos"))]
    {
        apple_jit::sys_icache_invalidate(ptr as *mut _, len);
    }
    #[cfg(all(target_arch = "aarch64", not(target_os = "macos")))]
    {
        if len == 0 {
            return;
        }
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

/// Memory manager.
#[derive(Debug)]
pub struct JitMemory {
    /// Current memory page.
    page: Page,
    /// Information of momory pages.
    pages: [MemPage; 3],
    ///
    labels: Vec<DestLabel>,
    /// MAP_JIT pages are write-protected per-thread on macOS/aarch64.
    /// Track the current writability so every emit path can lazily flip
    /// the region back to writable when a previous `finalize()` (or
    /// explicit `set_executable()`) left it executable. Outside macOS/
    /// aarch64 the W^X toggle is a no-op, so the field is only compiled
    /// in there to keep the struct size identical for other targets.
    #[cfg(all(target_arch = "aarch64", target_os = "macos"))]
    writable: bool,
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
        let (code_contents, data_contents) = allocate_pages();
        // MAP_JIT pages start out write-protected on macOS; switch them
        // to writable for the initial code generation. No-op elsewhere.
        flip_writable();
        let initial_page = MemPage::new(code_contents);
        let second_page = MemPage::new(unsafe { code_contents.add(PAGE_SIZE) });
        let data_page = MemPage::new(data_contents);
        JitMemory {
            page: Page(0),
            pages: [initial_page, second_page, data_page],
            labels: vec![],
            #[cfg(all(target_arch = "aarch64", target_os = "macos"))]
            writable: true,
        }
    }

    /// Ensure the MAP_JIT region is writable for the current thread
    /// before an emit / patch operation. On macOS/aarch64, calling
    /// `set_executable()` (or `finalize()`, which delegates to it)
    /// leaves the pages write-protected; the next emit would SIGBUS
    /// without this lazy flip. Outside macOS/aarch64 this is an
    /// inlined no-op so the hot path stays branch-free.
    #[inline]
    fn ensure_writable(&mut self) {
        #[cfg(all(target_arch = "aarch64", target_os = "macos"))]
        {
            if !self.writable {
                flip_writable();
                self.writable = true;
            }
        }
    }

    /// Switch JIT memory back to writable mode for the current thread.
    /// On macOS/AArch64 this calls `pthread_jit_write_protect_np(0)`;
    /// elsewhere it is a no-op. Callers don't normally need this — every
    /// emit path lazily flips back to writable via `ensure_writable`.
    pub fn set_writable(&mut self) {
        flip_writable();
        #[cfg(all(target_arch = "aarch64", target_os = "macos"))]
        {
            self.writable = true;
        }
    }

    /// Switch JIT memory to executable mode for the current thread and
    /// synchronize the I-cache so the CPU sees the freshly written
    /// instructions. Called automatically at the end of
    /// [`finalize`](Self::finalize).
    pub fn set_executable(&mut self) {
        flip_executable();
        #[cfg(all(target_arch = "aarch64", target_os = "macos"))]
        {
            self.writable = false;
        }
        #[cfg(target_arch = "aarch64")]
        {
            for page in &self.pages[..2] {
                let len = page.code_len.max(page.counter.0);
                if len > 0 {
                    unsafe { invalidate_icache(page.contents(), len) }
                }
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
