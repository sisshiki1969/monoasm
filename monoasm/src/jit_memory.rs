use crate::*;
/// Memory manager.
pub struct JitMemory {
    /// Pointer to the heap.
    pub contents: *mut u8,
    /// Current position
    pub counter: Pos,
    /// Current label id.
    pub label_count: usize,
    /// Relocation information.
    pub reloc: Relocations,
}

impl JitMemory {
    pub fn new() -> JitMemory {
        let contents: *mut u8;
        let size = 4096;
        unsafe {
            let mut page = mem::MaybeUninit::uninit().assume_init();
            libc::posix_memalign(&mut page, PAGE_SIZE, size);
            libc::mprotect(
                page,
                size,
                libc::PROT_EXEC | libc::PROT_READ | libc::PROT_WRITE,
            );
            contents = page as *mut u8;
        }
        JitMemory {
            contents,
            counter: Pos(0),
            label_count: 0,
            reloc: Relocations::new(),
        }
    }

    fn p(&self) {
        for i in 0..self.counter.0 {
            print!("{:>02x} ", self[Pos(i)]);
        }
        print!("\n");
    }

    pub fn get_mem_addr(&self) -> u64 {
        self.contents as u64
    }

    pub fn label(&mut self) -> DestLabel {
        let label = self.label_count;
        self.label_count += 1;
        self.reloc.push(Reloc::new());
        DestLabel(label)
    }

    pub fn bind_label(&mut self, label: DestLabel) {
        self.reloc[label].loc = Some(self.counter);
    }

    pub fn save_reloc(&mut self, dest: DestLabel, size: u8) {
        self.reloc[dest].disp.push((size, self.counter));
    }

    pub fn finalize(&mut self) -> fn() -> i64 {
        let mut relocs: Vec<(Pos, i32)> = vec![];
        for rel in &mut self.reloc.iter_mut() {
            let pos = rel.loc.unwrap();
            for (size, dest) in &mut rel.disp {
                let disp = pos.0 as i64 - dest.0 as i64 - *size as i64;
                if i32::min_value() as i64 > disp || disp > i32::max_value() as i64 {
                    panic!("Relocation overflow");
                }
                relocs.push((*dest, disp as i32));
            }
        }
        for (dest, disp) in relocs {
            self.write32(dest, disp);
        }
        unsafe { mem::transmute(self.contents) }
    }

    pub fn emitb(&mut self, val: u8) {
        let c = self.counter;
        self[c] = val;
        self.counter = c + 1;
    }

    fn emitb_with_rd(&mut self, val: u8, r: Reg) {
        let c = self.counter;
        self[c] = val | ((r as u8) & 0b0111);
        self.counter = c + 1;
    }

    pub fn emitw(&mut self, val: u16) {
        let c = self.counter;
        self[c] = val as u8;
        self[c + 1] = (val >> 8) as u8;
        self.counter = c + 2;
    }

    pub fn emitl(&mut self, val: u32) {
        let c = self.counter;
        self[c] = val as u8;
        self[c + 1] = (val >> 8) as u8;
        self[c + 2] = (val >> 16) as u8;
        self[c + 3] = (val >> 24) as u8;
        self.counter = c + 4;
    }

    fn write32(&mut self, loc: Pos, val: i32) {
        let val = val as u32;
        self[loc] = val as u8;
        self[loc + 1] = (val >> 8) as u8;
        self[loc + 2] = (val >> 16) as u8;
        self[loc + 3] = (val >> 24) as u8;
    }

    pub fn emitq(&mut self, val: u64) {
        self.emitl(val as u32);
        self.emitl((val >> 32) as u32);
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
