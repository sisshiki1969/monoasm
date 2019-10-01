#![allow(dead_code)]
extern crate libc;
use std::mem;
use std::ops::{Index, IndexMut};

const PAGE_SIZE: usize = 4096;

#[derive(Copy, Clone, PartialEq)]
pub enum Or {
    Imm(u64),
    Reg(Reg),
    Ind(Reg),
    IndD8(Reg, i8),
    IndD32(Reg, i32),
}

#[derive(Copy, Clone, PartialEq)]
pub enum Dest {
    Reg(Reg),
    Rel(usize),
}

#[derive(Copy, Clone, PartialEq)]
pub enum Reg {
    Rax = 0,
    Rcx = 1,
    Rdx = 2,
    Rbx = 3,
    Rsp = 4,
    Rbp = 5,
    Rsi = 6,
    Rdi = 7,
    R8 = 8,
    R9 = 9,
    R10 = 10,
    R11 = 11,
    R12 = 12,
    R13 = 13,
    R14 = 14,
    R15 = 15,
}

#[derive(Copy, Clone, PartialEq)]
pub enum Mode {
    Ind = 0,   // (rax)
    InD8 = 1,  // (rax + disp8)
    InD32 = 2, // (rax + disp32)
    Reg = 3,   // rax
}

pub struct Reloc {
    pub loc: Option<usize>,
    pub disp: Vec<(u8, usize)>, //(size_in_bytes, pos_in_contents)
}

impl Reloc {
    fn new() -> Reloc {
        Reloc {
            loc: None,
            disp: vec![],
        }
    }
}

/// http://www.jonathanturner.org/2015/12/building-a-simple-jit-in-rust.html
pub struct JitMemory {
    pub contents: *mut u8,
    pub counter: usize,
    pub label_count: usize,
    pub reloc: Vec<Reloc>,
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
            contents = mem::transmute(page);
        }
        JitMemory {
            contents,
            counter: 0,
            label_count: 0,
            reloc: vec![],
        }
    }

    fn p(&self) {
        for i in 0..self.counter {
            print!("{:>02x} ", self[i]);
        }
        print!("\n");
    }

    pub fn get_mem_addr(&self) -> u64 {
        self.contents as u64
    }

    pub fn label(&mut self) -> usize {
        let label = self.label_count;
        self.label_count += 1;
        self.reloc.push(Reloc::new());
        label
    }

    pub fn bind_label(&mut self, label: usize) {
        self.reloc[label].loc = Some(self.counter);
    }

    pub fn save_reloc(&mut self, dest: usize, size: u8) {
        self.reloc[dest].disp.push((size, self.counter));
    }

    pub fn finalize(&mut self) -> (fn() -> i64) {
        let mut relocs: Vec<(usize, i32)> = vec![];
        for rel in &mut self.reloc {
            let pos = rel.loc.unwrap();
            for (size, dest) in &mut rel.disp {
                let disp = pos as i32 - *dest as i32 - *size as i32;
                relocs.push((*dest, disp));
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

    fn write32(&mut self, loc: usize, val: i32) {
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

impl Index<usize> for JitMemory {
    type Output = u8;

    fn index(&self, index: usize) -> &u8 {
        unsafe { &*self.contents.offset(index as isize) }
    }
}

impl IndexMut<usize> for JitMemory {
    fn index_mut(&mut self, index: usize) -> &mut u8 {
        unsafe { &mut *self.contents.offset(index as isize) }
    }
}
