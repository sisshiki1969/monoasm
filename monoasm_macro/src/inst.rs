extern crate proc_macro2;
extern crate quote;
extern crate syn;
use monoasm_inst::Reg;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::Ident;

#[derive(Clone)]
pub struct Stmts {
    pub base: syn::Expr,
    pub contents: Vec<Inst>,
}

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

    Imull(Operand, Operand),

    Pushq(Operand),
    Popq(Operand),

    Jmp(Dest),
    Jne(Ident),
    Je(Ident),

    Call(Dest),
    Ret,
    Syscall,
}

#[derive(Clone, Debug)]
pub enum Operand {
    Imm(TokenStream),
    Reg(Reg),
    RegExpr(TokenStream),
    Ind(Reg, Option<Imm>),
}

impl std::fmt::Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::Imm(i) => write!(f, "Imm({})", i),
            Operand::Reg(r) => write!(f, "{:?}", r),
            Operand::RegExpr(s) => write!(f, "R({})", s),
            Operand::Ind(r, d) => match d {
                Some(d) => write!(f, "{}[{:?}]", d, r),
                None => write!(f, "[{:?}]", r),
            },
        }
    }
}

impl ToTokens for Operand {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ts = match self {
            Operand::Imm(_) => unreachable!(),
            Operand::Reg(r) => {
                quote!(Or::Reg(#r))
            }
            Operand::RegExpr(ts) => {
                quote!(Or::Reg(Reg::from((#ts) as u64)))
            }
            Operand::Ind(r, imm) => match imm {
                Some(Imm::Imm(i)) => {
                    quote!(Or::IndD32(#r, #i))
                }
                Some(Imm::Expr(ts)) => {
                    quote!(Or::IndD32(#r, (#ts) as i32))
                }
                None => {
                    quote!(Or::Ind(#r))
                }
            },
        };
        tokens.extend(ts);
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Dest {
    Reg(Reg),
    Rel(Ident),
}

#[derive(Clone, Debug)]
pub enum Imm {
    Imm(i32),
    Expr(TokenStream),
}

impl std::fmt::Display for Imm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Imm::Imm(i) => write!(f, "Imm({})", i),
            Imm::Expr(ts) => write!(f, "Expr({})", ts),
        }
    }
}
