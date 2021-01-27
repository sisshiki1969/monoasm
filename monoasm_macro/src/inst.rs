extern crate proc_macro2;
extern crate quote;
extern crate syn;
use super::parse::*;
use monoasm_inst::Reg;
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

//----------------------------------------------------------------------
//
//  Operands.
//
//----------------------------------------------------------------------
#[derive(Clone, Debug)]
pub enum Operand {
    Imm(TokenStream),
    Reg(TokenStream),
    Ind { base: TokenStream, disp: Disp },
}

impl Parse for Operand {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        if input.peek(Ident) {
            let reg = input.parse::<Register>()?;
            Ok(Operand::reg(reg.0))
        } else if input.peek(LitInt) && is_single(input) {
            let imm = input.parse::<LitInt>()?;
            Ok(Operand::Imm(quote! { #imm }))
        } else if input.peek(token::Bracket) {
            let addr: IndAddr = input.parse()?;
            Ok(Operand::Ind {
                base: addr.base,
                disp: addr.offset,
            })
        } else if input.peek(token::Paren) {
            let gr = input.parse::<Group>()?;
            Ok(Operand::Imm(gr.stream()))
        } else {
            return Err(input.error("Expected register name, integer literal, memory reference, or Rust expression with parenthesis."));
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
                Or::Reg(#ts)
            ),
            Operand::Ind { base, disp } => quote!(
                match (#disp) as i32 {
                    0 => Or::Ind(Reg::from(#base), Disp::None),
                    disp if std::i8::MIN as i32 <= disp && disp <= std::i8::MAX as i32 => Or::Ind(Reg::from(#base), Disp::D8(disp as i8)),
                    disp => Or::Ind(Reg::from(#base), Disp::D32(disp)),
                }
            ),
        };
        tokens.extend(ts);
    }
}

impl Operand {
    pub fn reg(expr: TokenStream) -> Self {
        Self::Reg(quote!(Reg::from(#expr as u64)))
    }
}

//----------------------------------------------------------------------
//
//  Designation for Register.
//
//----------------------------------------------------------------------
#[derive(Clone, Debug)]
struct Register(TokenStream);

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

//----------------------------------------------------------------------
//
//  Modes for indirect addressing.
//
//----------------------------------------------------------------------
#[derive(Clone, Debug)]
struct IndAddr {
    base: TokenStream,
    offset: Disp,
}

impl Parse for IndAddr {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let content;
        syn::bracketed!(content in input);
        let base: Register = content.parse()?;
        let offset: Disp = content.parse()?;
        Ok(IndAddr {
            base: base.0,
            offset,
        })
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
