#![feature(asm)]
#![feature(const_raw_ptr_to_usize_cast)]
extern crate libc;
use std::mem;
use std::ops::{Add, Deref, DerefMut, Index, IndexMut};
mod jit_memory;
pub mod test;
pub use jit_memory::*;
use monoasm_inst::{Mode, Reg};

const PAGE_SIZE: usize = 4096;

/// Operands.
#[derive(Copy, Clone, PartialEq, Debug)]
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

pub enum IndKind {
    Ind(Reg),
    IndD8(Reg, i8),
    IndD32(Reg, i32),
}

impl Or {
    fn op_to_rm(self) -> (Mode, Reg, Option<i32>) {
        match self {
            Or::Reg(r) => (Mode::Reg, r, None),
            Or::Ind(r) => (Mode::Ind, r, None),
            Or::IndD8(r, d) => (Mode::InD8, r, Some(d as i32)),
            Or::IndD32(r, d) => (Mode::InD32, r, Some(d)),
            rm_op => unreachable!("as_rm():{:?}", rm_op),
        }
    }
}

/// Destination.
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Dest {
    /// Register
    Reg(Reg),
    /// Relative
    Rel(usize),
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
