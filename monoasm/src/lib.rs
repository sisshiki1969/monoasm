extern crate libc;
use std::mem;
use std::ops::{Add, Deref, DerefMut, Index, IndexMut, Sub};
mod jit_memory;
pub mod test;
pub use jit_memory::*;

const PAGE_SIZE: usize = 4096 * 256;

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Mode {
    Reg,
    Ind(Disp), // [reg + disp]
}

impl Mode {
    pub fn encode(&self) -> u8 {
        match self {
            Mode::Reg => 3,
            Mode::Ind(Disp::None) => 0,
            Mode::Ind(Disp::D8(_)) => 1,
            Mode::Ind(Disp::D32(_)) => 2,
            Mode::Ind(Disp::Label(_)) => 2,
        }
    }

    pub fn from_disp(disp: i32) -> Self {
        match disp {
            0 => Self::Ind(Disp::None),
            disp => {
                if let Ok(disp) = i8::try_from(disp) {
                    Self::Ind(Disp::D8(disp))
                } else {
                    Self::Ind(Disp::D32(disp))
                }
            }
        }
    }

    pub fn from_label(label: DestLabel) -> Self {
        Self::Ind(Disp::Label(label))
    }
}

/// Operands.
#[derive(Copy, Clone, PartialEq, Debug)]
pub struct Rm {
    base: Reg,
    mode: Mode,
}

impl Rm {
    pub fn reg(base: Reg) -> Self {
        Self {
            base,
            mode: Mode::Reg,
        }
    }

    pub fn new(base: Reg, mode: Mode) -> Self {
        Self { base, mode }
    }

    pub fn rip_ind_from(rm: Rm) -> Self {
        let disp = match rm.mode {
            Mode::Reg => unimplemented!(),
            Mode::Ind(Disp::D8(d)) => d as i32,
            Mode::Ind(Disp::D32(d)) => d,
            Mode::Ind(Disp::None) => 0,
            Mode::Ind(Disp::Label(label)) => {
                return Self {
                    base: Reg::from(5),
                    mode: Mode::Ind(Disp::Label(label)),
                }
            }
        };
        Self {
            base: Reg::from(5),
            mode: Mode::Ind(Disp::D32(disp)),
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

    pub fn is_cl(&self) -> bool {
        self.0 == 1
    }

    pub fn is_rax(&self) -> bool {
        self.0 == 0
    }
}
impl std::fmt::Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "R({})", self.0)
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

impl Sub<Pos> for Pos {
    type Output = usize;

    fn sub(self, other: Pos) -> Self::Output {
        self.0 - other.0
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
#[derive(Debug, Default)]
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
