extern crate proc_macro2;
extern crate quote;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};

#[derive(Copy, Clone, PartialEq, Debug)]
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

impl Reg {
    pub fn none() -> Self {
        Reg::Rax
    }

    pub fn from(num: u64) -> Self {
        match num {
            0 => Reg::Rax,
            1 => Reg::Rcx,
            2 => Reg::Rdx,
            3 => Reg::Rbx,
            4 => Reg::Rsp,
            5 => Reg::Rbp,
            6 => Reg::Rsi,
            7 => Reg::Rdi,
            8 => Reg::R8,
            9 => Reg::R9,
            10 => Reg::R10,
            11 => Reg::R11,
            12 => Reg::R12,
            13 => Reg::R13,
            14 => Reg::R14,
            15 => Reg::R15,
            _ => unreachable!("Illegal register number."),
        }
    }

    pub fn from_str(string: impl Into<String>) -> Option<Reg> {
        let mut string = string.into();
        string.make_ascii_lowercase();
        let reg = match string.as_str() {
            "rax" => Reg::Rax,
            "rcx" => Reg::Rcx,
            "rdx" => Reg::Rdx,
            "rbx" => Reg::Rbx,
            "rsp" => Reg::Rsp,
            "rbp" => Reg::Rbp,
            "rsi" => Reg::Rsi,
            "rdi" => Reg::Rdi,
            "r8" => Reg::R8,
            "r9" => Reg::R9,
            "r10" => Reg::R10,
            "r11" => Reg::R11,
            "r12" => Reg::R12,
            "r13" => Reg::R13,
            "r14" => Reg::R14,
            "r15" => Reg::R15,
            _ => return None,
        };
        Some(reg)
    }
}

impl ToTokens for Reg {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let r = *self as u64;
        let ts = quote!(Reg::from(#r));
        tokens.extend(ts);
    }
}

#[derive(Copy, Clone, PartialEq)]
pub enum Mode {
    Ind = 0,   // [reg]
    InD8 = 1,  // [reg + disp8]
    InD32 = 2, // [rax + disp32]
    Reg = 3,   // reg
}

pub mod util {
    use super::*;
    /// ModRM
    /// +-------+---+---+---+---+---+---+---+---+
    /// |  bit  | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
    /// +-------+---+---+---+---+---+---+---+---+
    /// | field |  mod  |    reg    |    r/m    |
    /// +-------+-------+-----------+-----------+
    /// |  rex  |       |     r     |     b     |
    /// +-------+-------+-----------+-----------+
    pub fn modrm_digit(digit: u8, mode: Mode, rm: Reg) -> u8 {
        (mode as u8) << 6 | (digit & 0b111) << 3 | (rm as u8) & 0b111
    }

    pub fn modrm(reg: Reg, mode: Mode, rm: Reg) -> u8 {
        (mode as u8) << 6 | ((reg as u8) & 0b111) << 3 | (rm as u8) & 0b111
    }

    /// Emit SIB
    /// +-------+---+---+---+---+---+---+---+---+
    /// |  bit  | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
    /// +-------+---+---+---+---+---+---+---+---+
    /// | field | scale |   index   |    base   |
    /// +-------+-------+-----------+-----------+
    /// |  rex  |       |     x     |     b     |
    /// +-------+-------+-----------+-----------+
    ///
    /// scale: 00|01|10|11
    ///  mul : na| 2| 4| 8
    ///
    /// index: register number (with rex.x)
    ///
    /// base: register number (with rex.b)
    ///     rex.b:0 base:101 => use RBP  mode:00/disp32 01/RBP+disp8 10/RBP+disp32
    ///     rex.b:1 base:101 => use R13  mode:00/disp32 01/R13+disp8 10/R13+disp32
    ///
    pub fn sib(scale: u8, index: Reg, base: Reg) -> u8 {
        assert!(scale < 4);
        assert!((index as u8) < 8);
        //assert!(base < 8);
        scale << 6 | (index as u8) << 3 | ((base as u8) & 0b111)
    }

    pub fn op_with_rd(op: u8, r: Reg) -> u8 {
        op | ((r as u8) & 0b0111)
    }

    /// REX.W
    ///      bit
    /// +---+---+------------------------------------------------+
    /// | W | 3 | 1 = 64 bit operand size                        |
    /// +---+---+------------------------------------------------+
    /// | R | 2 | rex_r = ext of reg field of ModRM              |
    /// +---+---+------------------------------------------------+
    /// | X | 1 | rex_i = ext of index field of SIB              |
    /// +---+---+------------------------------------------------+
    /// | B | 0 | rex_b = ext of r/m(ModRM) or base(SIB)         |
    /// |   |   |           or reg field of Op.                  |
    /// +---+---+------------------------------------------------+
    pub fn rexw(reg: Reg, base: Reg, index: Reg) -> u8 {
        0x48 | ((reg as u8) & 0b0000_1000) >> 1
            | ((index as u8) & 0b0000_1000) >> 2
            | ((base as u8) & 0b0000_1000) >> 3
    }

    pub fn rex(reg: Reg, base: Reg, index: Reg) -> Option<u8> {
        if base as u8 > 7 {
            let rex_prefix = 0x40
                | ((reg as u8) & 0b0000_1000) >> 1
                | ((index as u8) & 0b0000_1000) >> 2
                | ((base as u8) & 0b0000_1000) >> 3;
            Some(rex_prefix)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
