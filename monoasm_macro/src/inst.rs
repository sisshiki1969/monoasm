extern crate proc_macro2;
extern crate quote;
extern crate syn;
pub use super::parse::*;
use proc_macro2::Group;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream};
use syn::Expr;
use syn::LitFloat;
use syn::{token, Error, Ident, LitInt, Token};

///----------------------------------------------------------------------
///
///  Assembly statements.
///
///----------------------------------------------------------------------
#[derive(Clone)]
pub struct Stmts {
    pub base: syn::Expr,
    pub contents: Vec<Inst>,
}

impl Parse for Stmts {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let base: Expr = input.parse()?;
        input.parse::<Token![,]>()?;
        let mut stmts = Stmts {
            base,
            contents: vec![],
        };
        loop {
            if input.is_empty() {
                break;
            }
            let inst = input.parse::<Inst>()?;
            //println!("{:?}", &inst);
            stmts.contents.push(inst);
        }
        Ok(stmts)
    }
}

///----------------------------------------------------------------------
///
///  Assembly instructions.
///
///----------------------------------------------------------------------
#[derive(Clone, Debug)]
pub enum Inst {
    Label(Ident),

    F64(f64),
    I64(i64),

    Movq(MovOperand, MovOperand),
    Addq(Operand, Operand),
    Orq(Operand, Operand),
    Adcq(Operand, Operand),
    Sbbq(Operand, Operand),
    Andq(Operand, Operand),
    Subq(Operand, Operand),
    Xorq(Operand, Operand),
    Cmpq(Operand, Operand),
    Negq(Operand),

    Imul(Operand, Operand),
    Idiv(Operand),

    Movsd(XmmOperand, XmmOperand),
    Addsd(XmmOperand, XmmOperand),
    Subsd(XmmOperand, XmmOperand),
    Mulsd(XmmOperand, XmmOperand),
    Divsd(XmmOperand, XmmOperand),

    Cvtsi2sdq(XmmOperand, Operand),

    Pushq(Operand),
    Popq(Operand),

    Jmp(Dest),
    Jcc(Cond, Ident),

    Call(Dest),
    Ret,
    Syscall,
}

///----------------------------------------------------------------------
///
///  Comparison kinds.
///
///----------------------------------------------------------------------
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
                    let op = input.parse::<Ident>()?;
                    input.parse::<Token![;]>()?;
                    Ok(Inst::Jcc(Cond::$inst, op))
                }
            )
        }

        let ident = input.parse::<Ident>()?;
        if input.peek(Token![:]) {
            input.parse::<Token![:]>()?;
            Ok(Inst::Label(ident))
        } else {
            match ident.to_string().as_str() {
                "movq" => parse_2op!(Movq),
                "addq" => parse_2op!(Addq),
                "orq" => parse_2op!(Orq),
                "adcq" => parse_2op!(Adcq),
                "sbbq" => parse_2op!(Sbbq),
                "andq" => parse_2op!(Andq),
                "subq" => parse_2op!(Subq),
                "xorq" => parse_2op!(Xorq),
                "negq" => parse_1op!(Negq),
                "imul" => parse_2op!(Imul),
                "idiv" => parse_1op!(Idiv),

                "movsd" => parse_2op!(Movsd),
                "addsd" => parse_2op!(Addsd),
                "subsd" => parse_2op!(Subsd),
                "mulsd" => parse_2op!(Mulsd),
                "divsd" => parse_2op!(Divsd),

                "cvtsi2sdq" => parse_2op!(Cvtsi2sdq),

                "pushq" => parse_1op!(Pushq),
                "popq" => parse_1op!(Popq),
                "cmpq" => parse_2op!(Cmpq),
                "call" => parse_1op!(Call),
                "ret" => parse_0op!(Ret),
                "jmp" => parse_1op!(Jmp),
                "jne" => parse_jcc!(Ne),
                "jeq" => parse_jcc!(Eq),
                "syscall" => parse_0op!(Syscall),

                "dq" => {
                    if input.peek(LitFloat) {
                        let f = input.parse::<LitFloat>()?.base10_parse()?;
                        input.parse::<Token![;]>()?;
                        Ok(Inst::F64(f))
                    } else if input.peek(LitInt) {
                        let i = input.parse::<LitInt>()?.base10_parse()?;
                        input.parse::<Token![;]>()?;
                        Ok(Inst::I64(i))
                    } else {
                        Err(input.error("unimplemented literal."))
                    }
                }
                _ => Err(Error::new(ident.span(), "unimplemented instruction.")),
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
            Ok(Operand::reg(reg))
        } else if input.peek(LitInt) && is_single(input) {
            // e.g. "42"
            let imm = input.parse::<LitInt>()?;
            Ok(Operand::imm(imm))
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
            Operand::Reg(ts) => quote!(Or::reg(#ts)),
            Operand::Ind { base, disp } => quote!( Or::new(#base, #disp) ),
        };
        tokens.extend(ts);
    }
}

impl Operand {
    pub fn reg(reg: Register) -> Self {
        Self::Reg(reg)
    }

    pub fn imm(imm: impl ToTokens) -> Self {
        Self::Imm(quote! { #imm })
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
            Self::Ind { base, disp } => quote!( Or::new(#base, #disp) ),
        };
        tokens.extend(ts);
    }
}

///----------------------------------------------------------------------
///
///  General register / Xmm register / memory reference / immediate Operands.
///
///----------------------------------------------------------------------
#[derive(Clone, Debug)]
pub enum MovOperand {
    Imm(TokenStream),
    Reg(Register),
    Xmm(TokenStream),
    Ind { base: Register, disp: Disp },
}

impl Parse for MovOperand {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        if input.peek(Ident) {
            let ident = input.parse::<Ident>()?.to_string();
            if ident == "xmm" {
                if input.peek(token::Paren) {
                    // e.g. "xmm(0)"
                    let gr = input.parse::<Group>()?;
                    Ok(Self::Xmm(gr.stream()))
                } else {
                    Err(input.error(format!(
                        "Expected xmm register number. e.g. xmm0 or xmm(0) actual:{}",
                        ident,
                    )))
                }
            } else if ident.starts_with("xmm") {
                // e.g. "xmm0"
                if let Ok(no) = ident[3..].parse::<u8>() {
                    if no > 15 {
                        Err(input.error(format!("Invalid xmm register name. {}", ident)))
                    } else {
                        Ok(Self::Xmm(quote!(#no as u64)))
                    }
                } else {
                    Err(input.error(format!("Invalid xmm register name. {}", ident)))
                }
            } else {
                // e.g. "rax"
                let register = if ident == "R" {
                    let content;
                    syn::parenthesized!(content in input);
                    let s = content.parse::<Expr>()?;
                    Register::new(quote!(#s))
                } else {
                    let reg =
                        Reg::from_str(&ident).ok_or(input.error("Expected register name."))? as u64;
                    Register::new(quote!(#reg))
                };
                Ok(Self::reg(register))
            }
        } else if input.peek(LitInt) && is_single(input) {
            // e.g. "42"
            let imm = input.parse::<LitInt>()?;
            Ok(Self::imm(imm))
        } else if input.peek(token::Bracket) {
            // e.g. "[rax + 4]", "[rax - (4)]"
            let addr = input.parse::<IndAddr>()?;
            Ok(Self::Ind {
                base: addr.base,
                disp: addr.offset,
            })
        } else if input.peek(token::Paren) {
            // e.g. "(42)"
            let gr = input.parse::<Group>()?;
            Ok(Self::imm(gr.stream()))
        } else {
            Err(input.error("Expected register name, integer literal, memory reference, or Rust expression with parenthesis."))
        }
    }
}

impl std::fmt::Display for MovOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Imm(i) => write!(f, "{}", i),
            Self::Reg(reg) => write!(f, "{}", reg),
            Self::Xmm(reg) => write!(f, "xmm({})", reg),
            Self::Ind { base, disp } => write!(f, "({})[{:?}]", disp, base),
        }
    }
}

impl ToTokens for MovOperand {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ts = match self {
            Self::Imm(_) => unreachable!("immediate"),
            Self::Reg(ts) => quote!(Or::reg(#ts)),
            Self::Xmm(ts) => quote!(Or::reg(Reg::from(#ts))),
            Self::Ind { base, disp } => quote!( Or::new(#base, #disp) ),
        };
        tokens.extend(ts);
    }
}

impl MovOperand {
    pub fn reg(reg: Register) -> Self {
        Self::Reg(reg)
    }

    pub fn imm(imm: impl ToTokens) -> Self {
        Self::Imm(quote! { #imm })
    }
}
