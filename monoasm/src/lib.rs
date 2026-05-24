extern crate libc;
use std::mem;
use std::ops::{Add, Index, IndexMut, Sub};
#[cfg(target_arch = "aarch64")]
pub mod arm64;
mod jit_memory;
pub mod test;
#[cfg(target_arch = "x86_64")]
pub mod x64;
#[cfg(target_arch = "aarch64")]
pub use arm64::*;
pub use jit_memory::*;
#[cfg(target_arch = "x86_64")]
pub use x64::*;

const PAGE_SIZE: usize = 1024 * 1024 * 256;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct CodePtr(std::num::NonZero<usize>);

impl std::ops::Sub<CodePtr> for CodePtr {
    type Output = i64;
    fn sub(self, rhs: CodePtr) -> Self::Output {
        (self.0.get() as i64) - (rhs.0.get() as i64)
    }
}

impl std::ops::Add<usize> for CodePtr {
    type Output = CodePtr;
    fn add(self, rhs: usize) -> Self::Output {
        CodePtr::from(unsafe { self.as_ptr().add(rhs) })
    }
}

impl std::cmp::PartialOrd for CodePtr {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.as_ptr().partial_cmp(&other.as_ptr())
    }
}

impl CodePtr {
    pub fn from(ptr: *mut u8) -> Self {
        Self(std::num::NonZero::new(ptr as usize).unwrap())
    }

    pub fn as_ptr(&self) -> *mut u8 {
        self.0.get() as *mut u8
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
#[derive(Clone, PartialEq, Default, Debug)]
#[repr(transparent)]
pub struct DestLabel(std::rc::Rc<std::cell::RefCell<LabelInfo>>);

impl DestLabel {
    pub fn new() -> Self {
        Self(std::rc::Rc::new(std::cell::RefCell::new(LabelInfo::new())))
    }

    pub fn loc(&self) -> (Page, Pos) {
        self.0.borrow().loc()
    }

    fn bind(&mut self, page: Page, pos: Pos) -> LabelInfo {
        std::mem::replace(&mut *self.0.borrow_mut(), LabelInfo::Resolved((page, pos)))
    }
}
///
/// Relocation
///
/// This holds a pair of a location in JitMemory (whether determined or not)
/// and (possibly multiple) target positions for each *DestLabel*.
///
#[derive(Clone, PartialEq, Debug)]
enum LabelInfo {
    /// A location of each *DestLabel* in JitMemory.
    /// None for not yet determined.
    Resolved((Page, Pos)),
    /// Target informations.
    NotResolved(Vec<TargetType>),
}

impl std::default::Default for LabelInfo {
    fn default() -> Self {
        LabelInfo::NotResolved(vec![])
    }
}

impl LabelInfo {
    fn new() -> LabelInfo {
        LabelInfo::NotResolved(vec![])
    }

    fn loc(&self) -> (Page, Pos) {
        match self {
            LabelInfo::Resolved(loc) => *loc,
            _ => panic!("The DestLabel has not been resolved"),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
enum TargetType {
    #[cfg(target_arch = "x86_64")]
    Rel {
        page: Page,
        offset: u8,
        pos: Pos,
    },
    Abs {
        page: Page,
        pos: Pos,
    },
    /// AArch64 PC-relative branch/ADR: patch a scaled immediate into the
    /// bitfields of the instruction word already emitted at `pos`.
    #[cfg(target_arch = "aarch64")]
    Arm64 {
        page: Page,
        pos: Pos,
        kind: crate::Arm64Reloc,
    },
}
