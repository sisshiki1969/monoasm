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
    base: Reg,
    mode: Mode,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Mode {
    Reg,
    Ind,                 // [reg]
    InD8(i8),            // [reg + disp8]
    InD32(i32),          // [reg + disp32]
    IndLabel(DestLabel), // [reg + label]
}

impl Mode {
    pub fn encode(&self) -> u8 {
        match self {
            Mode::Reg => 3,
            Mode::Ind => 0,
            Mode::InD8(_) => 1,
            Mode::InD32(_) => 2,
            Mode::IndLabel(_) => 2,
        }
    }

    pub fn from_disp(disp: i32) -> Self {
        match disp {
            0 => Self::Ind,
            disp if std::i8::MIN as i32 <= disp && disp <= std::i8::MAX as i32 => {
                Self::InD8(disp as i8)
            }
            disp => Self::InD32(disp),
        }
    }

    pub fn from_label(label: DestLabel) -> Self {
        Self::IndLabel(label)
    }
}

impl Or {
    pub fn reg(base: Reg) -> Self {
        Self {
            base,
            mode: Mode::Reg,
        }
    }

    pub fn new(base: Reg, mode: Mode) -> Self {
        Self { base, mode }
    }

    pub fn rip_ind_from(rm: Or) -> Self {
        let disp = match rm.mode {
            Mode::Reg => unimplemented!(),
            Mode::InD32(d) => d,
            Mode::InD8(d) => d as i32,
            Mode::IndLabel(label) => {
                return Self {
                    base: Reg::from(5),
                    mode: Mode::IndLabel(label),
                }
            }
            Mode::Ind => 0i32,
        };
        Self {
            base: Reg::from(5),
            mode: Mode::InD32(disp),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub struct Reg(u8);

impl Reg {
    pub fn from(num: u64) -> Self {
        Reg(num as u8)
    }

    pub fn is_rip(&self) -> bool {
        self.0 == 16
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Disp {
    None,
    D8(i8),
    D32(i32),
    Label(DestLabel),
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
