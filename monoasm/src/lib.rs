#![allow(dead_code)]
extern crate libc;
use std::mem;
use std::ops::{Add, Deref, DerefMut, Index, IndexMut};
mod jit_memory;
pub use jit_memory::*;

const PAGE_SIZE: usize = 4096;

/// Operands.
#[derive(Copy, Clone, PartialEq)]
pub enum Or {
    /// Immediate
    Imm(u64),
    /// Register
    Reg(Reg),
    /// Indirect
    Ind(Reg),
    /// Indirect + 8bit displacement
    IndD8(Reg, i8),
    /// Indirect + 32nit displacement
    IndD32(Reg, i32),
}

/// Destination.
#[derive(Copy, Clone, PartialEq)]
pub enum Dest {
    /// Register
    Reg(Reg),
    /// Relative
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

/// Position in JitMemory.
#[derive(Copy, Clone, PartialEq)]
pub struct Pos(usize);

impl Add<i32> for Pos {
    type Output = Pos;

    fn add(self, other: i32) -> Self {
        Pos((self.0 as i64 + other as i64) as usize)
    }
}

/// Id for destination label.
#[derive(Copy, Clone, PartialEq)]
pub struct DestLabel(usize);

/// Relocation
///
/// This struct holds a pair of a single destination (whether determined or not)
/// and multiple source positions.
#[derive(Clone, PartialEq)]
pub struct Reloc {
    /// Destination position in JitMemory.
    /// None for not yet determined.
    pub loc: Option<Pos>,
    /// Source positions. (opcode size, position in JitMemory)
    pub disp: Vec<(u8, Pos)>,
}

impl Reloc {
    fn new() -> Reloc {
        Reloc {
            loc: None,
            disp: vec![],
        }
    }
}

/// Relocation tabla.
pub struct Relocations(Vec<Reloc>);

impl Relocations {
    fn new() -> Self {
        Relocations(vec![])
    }

    fn push(&mut self, reloc: Reloc) {
        self.0.push(reloc)
    }
}

impl Index<DestLabel> for Relocations {
    type Output = Reloc;

    fn index(&self, dest: DestLabel) -> &Self::Output {
        &self.0[dest.0]
    }
}

impl IndexMut<DestLabel> for Relocations {
    fn index_mut(&mut self, dest: DestLabel) -> &mut Self::Output {
        &mut self.0[dest.0]
    }
}

impl Deref for Relocations {
    type Target = Vec<Reloc>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Relocations {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
/*
impl IntoIterator for Relocations {
    type Item = Reloc;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
*/
