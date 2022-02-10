extern crate proc_macro2;
extern crate quote;
extern crate syn;
use super::parse::*;
use proc_macro2::Group;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream};
use syn::Expr;
use syn::{token, Error, Ident, LitInt, Token};

//----------------------------------------------------------------------
//
//  Assembly instructions.
//
//----------------------------------------------------------------------
#[derive(Clone, Debug)]
pub enum Inst {
    Label(Ident),

    Movq(Operand, Operand),
    Addq(Operand, Operand),
    Orq(Operand, Operand),
    Adcq(Operand, Operand),
    Sbbq(Operand, Operand),
    Andq(Operand, Operand),
    Subq(Operand, Operand),
    Xorq(Operand, Operand),
    Cmpq(Operand, Operand),

    Imul(Operand, Operand),
    Idiv(Operand),

    Movsd(XmmOperand, XmmOperand),
    Addsd(XmmOperand, XmmOperand),
    Subsd(XmmOperand, XmmOperand),
    Mulsd(XmmOperand, XmmOperand),
    Divsd(XmmOperand, XmmOperand),

    Pushq(Operand),
    Popq(Operand),

    Jmp(Dest),
    Jcc(Cond, Ident),

    Call(Dest),
    Ret,
    Syscall,
}

#[derive(Clone, Debug)]
pub enum Cond {
    Ne,
    Eq,
}

impl Parse for Inst {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        macro_rules! parse_2op {
            ($inst: ident) => (
                {
                    let op1 = input.parse()?;
                    input.parse::<Token![,]>()?;
                    let op2 = input.parse()?;
                    input.parse::<Token![;]>()?;
                    Ok(Inst::$inst(op1, op2))
                }
            )
        }

        macro_rules! parse_1op {
            ($inst: ident) => (
                {
                    let op = input.parse()?;
                    input.parse::<Token![;]>()?;
                    Ok(Inst::$inst(op))
                }
            )
        }

        macro_rules! parse_0op {
            ($inst: ident) => (
                {
                    input.parse::<Token![;]>()?;
                    Ok(Inst::$inst)
                }
            )
        }

        macro_rules! parse_jcc {
            ($inst: ident) => (
                {
                    let op = input.parse()?;
                    input.parse::<Token![;]>()?;
                    Ok(Inst::Jcc(Cond::$inst, op))
                }
            )
        }

        let inst: Ident = input.parse()?;
        if input.peek(Token![:]) {
            input.parse::<Token![:]>()?;
            Ok(Inst::Label(inst))
        } else {
            match inst.to_string().as_str() {
                "movq" => parse_2op!(Movq),
                "addq" => parse_2op!(Addq),
                "orq" => parse_2op!(Orq),
                "adcq" => parse_2op!(Adcq),
                "sbbq" => parse_2op!(Sbbq),
                "andq" => parse_2op!(Andq),
                "subq" => parse_2op!(Subq),
                "xorq" => parse_2op!(Xorq),
                "imul" => parse_2op!(Imul),
                "idiv" => parse_1op!(Idiv),
                "movsd" => parse_2op!(Movsd),
                "addsd" => parse_2op!(Addsd),
                "subsd" => parse_2op!(Subsd),
                "mulsd" => parse_2op!(Mulsd),
                "divsd" => parse_2op!(Divsd),

                "pushq" => parse_1op!(Pushq),
                "popq" => parse_1op!(Popq),
                "cmpq" => parse_2op!(Cmpq),
                "call" => parse_1op!(Call),
                "ret" => parse_0op!(Ret),
                "jmp" => parse_1op!(Jmp),
                "jne" => parse_jcc!(Ne),
                "jeq" => parse_jcc!(Eq),
                "syscall" => parse_0op!(Syscall),
                _ => Err(Error::new(inst.span(), "unimplemented instruction.")),
            }
        }
    }
}

///----------------------------------------------------------------------
///
///  General register / memory reference / immediate Operands.
///
///----------------------------------------------------------------------
#[derive(Clone, Debug)]
pub enum Operand {
    Imm(TokenStream),
    Reg(Register),
    Ind { base: Register, disp: Disp },
}

impl Parse for Operand {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        if input.peek(Ident) {
            // e.g. "rax"
            let reg = input.parse::<Register>()?;
            Ok(Operand::reg(reg.0))
        } else if input.peek(LitInt) && is_single(input) {
            // e.g. "42"
            let imm = input.parse::<LitInt>()?;
            Ok(Operand::Imm(quote! { #imm }))
        } else if input.peek(token::Bracket) {
            // e.g. "[rax + 4]", "[rax - (4)]"
            let addr = input.parse::<IndAddr>()?;
            Ok(Operand::Ind {
                base: addr.base,
                disp: addr.offset,
            })
        } else if input.peek(token::Paren) {
            // e.g. "(42)"
            let gr = input.parse::<Group>()?;
            Ok(Operand::Imm(gr.stream()))
        } else {
            Err(input.error("Expected register name, integer literal, memory reference, or Rust expression with parenthesis."))
        }
    }
}

impl std::fmt::Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::Imm(i) => write!(f, "{}", i),
            Operand::Reg(reg) => write!(f, "{}", reg),
            Operand::Ind { base, disp } => write!(f, "({})[{:?}]", disp, base),
        }
    }
}

impl ToTokens for Operand {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ts = match self {
            Operand::Imm(_) => unreachable!("immediate"),
            Operand::Reg(ts) => quote!(
                Or::reg(#ts)
            ),
            Operand::Ind { base, disp } => quote!(
                Or::ind_from(#base, #disp)
            ),
        };
        tokens.extend(ts);
    }
}

impl Operand {
    pub fn reg(expr: TokenStream) -> Self {
        Self::Reg(Register(expr))
    }
}

///----------------------------------------------------------------------
///
///  Floating pointer register(xmm) / memory reference Operands.
///
///----------------------------------------------------------------------
#[derive(Clone, Debug)]
pub enum XmmOperand {
    Xmm(TokenStream),
    Ind { base: Register, disp: Disp },
}

impl Parse for XmmOperand {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        if input.peek(Ident) {
            let reg = input.parse::<Ident>()?.to_string();
            if reg == "xmm" {
                if input.peek(token::Paren) {
                    let gr = input.parse::<Group>()?;
                    Ok(Self::Xmm(gr.stream()))
                } else {
                    Err(input.error(format!(
                        "Expected xmm register number. e.g. xmm0 or xmm(0) actual:{}",
                        reg,
                    )))
                }
            } else if reg.starts_with("xmm") {
                if let Ok(no) = reg[3..].parse::<u8>() {
                    if no > 15 {
                        Err(input.error(format!("Invalid xmm register name. {}", reg)))
                    } else {
                        Ok(Self::Xmm(quote!(#no as u64)))
                    }
                } else {
                    Err(input.error(format!("Invalid xmm register name. {}", reg)))
                }
            } else {
                Err(input.error("Expected xmm register name or memory reference."))
            }
        } else if input.peek(token::Bracket) {
            let addr: IndAddr = input.parse()?;
            Ok(Self::Ind {
                base: addr.base,
                disp: addr.offset,
            })
        } else {
            Err(input.error("Expected xmm register name or memory reference."))
        }
    }
}

impl std::fmt::Display for XmmOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Xmm(reg) => write!(f, "xmm({})", reg),
            Self::Ind { base, disp } => write!(f, "({})[{:?}]", disp, base),
        }
    }
}

impl ToTokens for XmmOperand {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ts = match self {
            Self::Xmm(ts) => quote!(
                Or::reg(Reg::from(#ts))
            ),
            Self::Ind { base, disp } => quote!(
                Or::ind_from(#base, #disp)
            ),
        };
        tokens.extend(ts);
    }
}

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
    RIP = 16,
}

impl Reg {
    pub fn none() -> Self {
        Reg::Rax
    }

    pub fn is_rip(&self) -> bool {
        *self == Reg::RIP
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
            16 => Reg::RIP,
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
            "rip" => Reg::RIP,
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
    InD32 = 2, // [reg + disp32]
    Reg = 3,   // reg
}

//----------------------------------------------------------------------
//
//  Designation for Register.
//
//----------------------------------------------------------------------
#[derive(Clone, Debug)]
pub struct Register(TokenStream);

impl Parse for Register {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let op = input.parse::<Ident>()?.to_string();
        if op == "R" {
            let content;
            syn::parenthesized!(content in input);
            let s = content.parse::<Expr>()?;
            Ok(Self(quote!(#s)))
        } else {
            let reg = Reg::from_str(&op).ok_or(input.error("Expected register name."))? as u64;
            Ok(Self(quote!(#reg)))
        }
    }
}

impl ToTokens for Register {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let reg = self.0.clone();
        let ts = quote! ( Reg::from(#reg) );
        tokens.extend(ts);
    }
}

impl std::fmt::Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

//----------------------------------------------------------------------
//
//  Modes for indirect addressing.
//
//----------------------------------------------------------------------
#[derive(Clone, Debug)]
struct IndAddr {
    base: Register,
    offset: Disp,
}

impl Parse for IndAddr {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let content;
        syn::bracketed!(content in input);
        let base = content.parse::<Register>()?;
        let offset = content.parse::<Disp>()?;
        Ok(IndAddr { base, offset })
    }
}

//----------------------------------------------------------------------
//
//  Destination.
//
//----------------------------------------------------------------------
#[derive(Clone, PartialEq, Debug)]
pub enum Dest {
    Reg(Reg),
    Rel(Ident),
}

impl Parse for Dest {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Ident) && is_single(input) {
            let dest: Ident = input.parse()?;
            let reg = Reg::from_str(&dest.to_string());
            match reg {
                Some(reg) => Ok(Dest::Reg(reg)),
                None => Ok(Dest::Rel(dest)),
            }
        } else {
            Err(lookahead.error())
        }
    }
}

fn is_single(input: ParseStream) -> bool {
    input.peek2(Token![,]) || input.peek2(Token![;])
}
