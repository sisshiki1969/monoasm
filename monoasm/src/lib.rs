extern crate libc;
use std::mem;
use std::ops::{Add, Deref, DerefMut, Index, IndexMut};
mod jit_memory;
pub mod test;
pub use jit_memory::*;

const PAGE_SIZE: usize = 4096;

/// Operands.
#[derive(Copy, Clone, PartialEq, Debug)]
pub struct Or {
    mode: Mode,
    base: Reg,
    disp: Disp,
}

impl Or {
    pub fn reg(reg: Reg) -> Self {
        Self {
            mode: Mode::Reg,
            base: reg,
            disp: Disp::None,
        }
    }
    pub fn ind_from(base: Reg, disp: i32) -> Self {
        match disp {
            0 => Self {
                mode: Mode::Ind,
                base,
                disp: Disp::None,
            },
            disp if std::i8::MIN as i32 <= disp && disp <= std::i8::MAX as i32 => Self {
                mode: Mode::InD8,
                base,
                disp: Disp::D8(disp as i8),
            },
            disp => Self {
                mode: Mode::InD32,
                base,
                disp: Disp::D32(disp),
            },
        }
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub struct Reg(u8);

impl Reg {
    pub fn from(num: u64) -> Self {
        Reg(num as u8)
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Mode {
    Ind = 0,   // [reg]
    InD8 = 1,  // [reg + disp8]
    InD32 = 2, // [rax + disp32]
    Reg = 3,   // reg
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Disp {
    None,
    D8(i8),
    D32(i32),
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
#[derive(Copy, Clone, PartialEq, Debug)]
pub struct Pos(usize);

impl Pos {
    pub fn from(pos: usize) -> Self {
        Pos(pos)
    }
}

impl Add<i32> for Pos {
    type Output = Pos;

    fn add(self, other: i32) -> Self {
        Pos((self.0 as i64 + other as i64) as usize)
    }
}

/// Id for destination label.
#[derive(Copy, Clone, PartialEq, Default, Debug)]
pub struct DestLabel(usize);

/// Relocation
///
/// This struct holds a pair of a single destination (whether determined or not)
/// and multiple source positions.
#[derive(Clone, PartialEq, Debug)]
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
#[derive(Debug)]
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
