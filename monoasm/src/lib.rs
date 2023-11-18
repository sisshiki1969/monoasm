extern crate libc;
use std::mem;
use std::ops::{Add, Deref, DerefMut, Index, IndexMut, Sub};
use std::ptr::NonNull;
mod jit_memory;
pub mod test;
pub use jit_memory::*;

const PAGE_SIZE: usize = 1024 * 1024 * 256;

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(transparent)]
pub struct CodePtr(NonNull<u8>);

impl std::ops::Sub<CodePtr> for CodePtr {
    type Output = i64;
    fn sub(self, rhs: CodePtr) -> Self::Output {
        (self.0.as_ptr() as usize as i64) - (rhs.0.as_ptr() as usize as i64)
    }
}

impl CodePtr {
    pub fn from(ptr: *mut u8) -> Self {
        Self(NonNull::new(ptr).unwrap())
    }

    pub fn as_ptr(&self) -> *mut u8 {
        self.0.as_ptr()
    }
}

/// Register.
#[derive(Copy, Clone, PartialEq, Debug)]
pub struct Reg(u8);

impl Reg {
    pub fn from(num: u64) -> Self {
        Reg(num as u8)
    }

    pub fn is_rip(&self) -> bool {
        self == &Self::rip()
    }

    pub fn is_cl(&self) -> bool {
        self == &Self::rcx()
    }

    pub fn is_rax(&self) -> bool {
        self == &Self::rax()
    }
}

impl Reg {
    fn rax() -> Self {
        Self::from(0)
    }
    fn rcx() -> Self {
        Self::from(1)
    }

    fn rbp() -> Self {
        Self::from(5)
    }

    fn rip() -> Self {
        Self::from(16)
    }
}

impl std::fmt::Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "R({})", self.0)
    }
}

/// Displacement for indirect addressing.
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Disp {
    None,
    D8(i8),
    D32(i32),
    Label(DestLabel),
}

impl Disp {
    pub fn from_disp(disp: i32) -> Self {
        match disp {
            0 => Disp::None,
            disp => {
                if let Ok(disp) = i8::try_from(disp) {
                    Disp::D8(disp)
                } else {
                    Disp::D32(disp)
                }
            }
        }
    }

    pub fn from_label(label: DestLabel) -> Self {
        Disp::Label(label)
    }
}

/// Scale index for indirect addressing.
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Scale {
    None,
    S1(u8, Reg),
}

impl Scale {
    fn index(&self) -> Reg {
        match self {
            Self::None => Reg(0),
            Self::S1(_, r) => *r,
        }
    }
}

pub enum Imm {
    None,
    B(i8),
    W(i16),
    L(i32),
    Q(i64),
}

impl Imm {
    pub fn offset(&self) -> u8 {
        match self {
            Self::None => 0,
            Self::B(_) => 1,
            Self::W(_) => 2,
            Self::L(_) => 4,
            Self::Q(_) => 8,
        }
    }
}

/// Destination for jump and call instructions.
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Dest {
    /// Register
    Reg(Reg),
    /// Relative
    Rel(usize),
}

/// Adressing modes.
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Mode {
    Reg,
    Ind(Scale, Disp), // [reg + disp]
}

impl Mode {
    fn encode(&self) -> u8 {
        match self {
            Mode::Reg => 3,
            Mode::Ind(_, Disp::None) => 0,
            Mode::Ind(_, Disp::D8(_)) => 1,
            Mode::Ind(_, Disp::D32(_)) => 2,
            Mode::Ind(_, Disp::Label(_)) => 2,
        }
    }

    fn scale(&self) -> Scale {
        match self {
            Mode::Reg => Scale::None,
            Mode::Ind(scale, _) => *scale,
        }
    }

    fn disp(&self) -> Disp {
        match self {
            Mode::Reg => Disp::None,
            Mode::Ind(_, disp) => *disp,
        }
    }

    fn is_indirect_no_disp(&self) -> bool {
        match self {
            Mode::Ind(_, Disp::None) => true,
            _ => false,
        }
    }
}

/// Register / Memory reference Operands.
#[derive(Copy, Clone, PartialEq, Debug)]
pub struct Rm {
    base: Reg,
    mode: Mode,
}

impl std::convert::From<Reg> for Rm {
    fn from(value: Reg) -> Self {
        Rm::reg(value)
    }
}

impl Rm {
    pub fn reg(base: Reg) -> Self {
        Self {
            base,
            mode: Mode::Reg,
        }
    }

    pub fn is_reg(&self) -> bool {
        self.mode == Mode::Reg
    }

    pub fn is_rax(&self) -> bool {
        self.mode == Mode::Reg && self.base.is_rax()
    }

    pub fn ind(base: Reg, disp: Disp, scale: Scale) -> Self {
        let mode = Mode::Ind(scale, disp);
        Self { base, mode }
    }

    pub fn rip_ind_from(rm: Rm) -> Self {
        let disp = match rm.mode {
            Mode::Reg => unimplemented!("register direct addressing is not allowed for RIP."),
            Mode::Ind(Scale::None, Disp::D8(d)) => d as i32,
            Mode::Ind(Scale::None, Disp::D32(d)) => d,
            Mode::Ind(Scale::None, Disp::None) => 0,
            Mode::Ind(Scale::None, Disp::Label(label)) => {
                return Self {
                    base: Reg::rbp(),
                    mode: Mode::Ind(Scale::None, Disp::Label(label)),
                }
            }
            _ => unimplemented!("scale index is not allowed for RIP."),
        };
        Self {
            base: Reg::rbp(),
            mode: Mode::Ind(Scale::None, Disp::D32(disp)),
        }
    }
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
#[repr(transparent)]
pub struct DestLabel(usize);

impl DestLabel {
    pub fn to_usize(&self) -> usize {
        self.0
    }
}

///
/// Relocation
///
/// This holds a pair of a location in JitMemory (whether determined or not)
/// and (possibly multiple) target positions for each *DestLabel*.
///
#[derive(Clone, PartialEq, Debug)]
struct LabelInfo {
    /// A location of each *DestLabel* in JitMemory.
    /// None for not yet determined.
    loc: Option<(Page, Pos)>,
    /// Target informations.
    target: Vec<TargetType>,
}

impl LabelInfo {
    fn new() -> LabelInfo {
        LabelInfo {
            loc: None,
            target: vec![],
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
enum TargetType {
    Rel { page: Page, offset: u8, pos: Pos },
    Abs { page: Page, pos: Pos },
}

/// Relocation tabla.
#[derive(Debug, Default)]
struct Labels(Vec<LabelInfo>);

impl Labels {
    fn new() -> Self {
        Labels(vec![])
    }

    fn new_label(&mut self) -> DestLabel {
        let label = DestLabel(self.0.len());
        self.0.push(LabelInfo::new());
        label
    }
}

impl Index<DestLabel> for Labels {
    type Output = LabelInfo;

    fn index(&self, dest: DestLabel) -> &Self::Output {
        &self.0[dest.0]
    }
}

impl IndexMut<DestLabel> for Labels {
    fn index_mut(&mut self, dest: DestLabel) -> &mut Self::Output {
        &mut self.0[dest.0]
    }
}

impl Deref for Labels {
    type Target = Vec<LabelInfo>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Labels {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
