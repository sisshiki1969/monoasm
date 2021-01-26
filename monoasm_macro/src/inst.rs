extern crate proc_macro2;
extern crate quote;
extern crate syn;
use super::parse::*;
use monoasm_inst::Mode;
use monoasm_inst::Reg;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream};
use syn::{Error, Ident};

//----------------------------------------------------------------------
//
//  Operands.
//
//----------------------------------------------------------------------

#[derive(Clone, Debug)]
pub enum Operand {
    Imm(TokenStream),
    //Reg(Reg),
    Reg(TokenStream),
    Ind(Reg, Disp),
}

impl std::fmt::Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::Imm(i) => write!(f, "{}", i),
            Operand::Reg(s) => write!(f, "{}", s),
            Operand::Ind(r, d) => match d {
                Disp::D8(d) => write!(f, "{}[{:?}]", d, r),
                Disp::D32(d) => write!(f, "{}[{:?}]", d, r),
                Disp::Expr(ts) => write!(f, "({})[{:?}]", ts, r),
                Disp::None => write!(f, "[{:?}]", r),
            },
        }
    }
}

impl ToTokens for Operand {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ts = match self {
            Operand::Imm(_) => unreachable!("immediate"),
            Operand::Reg(ts) => quote!(Or::Reg(#ts)),
            Operand::Ind(r, disp) => match disp {
                Disp::D8(i) => quote!(Or::Ind(#r, Disp::D8(#i))),
                Disp::D32(i) => quote!(Or::Ind(#r, Disp::D32(#i))),
                Disp::Expr(ts) => quote!(Or::Ind(#r, Disp::D32((#ts) as i32))),
                Disp::None => quote!(Or::Ind(#r, Disp::None)),
            },
        };
        tokens.extend(ts);
    }
}

impl Operand {
    /// Convert `self` to tupple of (Mode, Reg, Disp).
    pub fn op_to_rm(self) -> (Mode, Reg, Disp) {
        match self {
            //Operand::Reg(r) => (Mode::Reg, r, Disp::None),
            Operand::Ind(r, disp) => match disp {
                Disp::None => (Mode::Ind, r, Disp::None),
                Disp::D8(i) => (Mode::InD8, r, Disp::D8(i)),
                Disp::D32(i) => (Mode::InD32, r, Disp::D32(i)),
                Disp::Expr(expr) => (Mode::InD32, r, Disp::Expr(expr)),
            },
            _ => unreachable!("Illegal operand. {}", self),
        }
    }

    pub fn reg(expr: TokenStream) -> Self {
        Self::Reg(quote!(Reg::from(#expr as u64)))
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
