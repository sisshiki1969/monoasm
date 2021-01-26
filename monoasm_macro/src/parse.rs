use super::asm::*;
use super::inst::*;
use monoasm_inst::Reg;
use proc_macro2::TokenStream;
use proc_macro2::{Group, Punct};
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    Expr,
};
use syn::{token, Error, Ident, LitInt, Token};

#[derive(Clone, Debug)]
pub enum Disp {
    None,
    D8(i8),
    D32(i32),
    Expr(TokenStream),
}

pub fn is_single(input: ParseStream) -> bool {
    input.peek2(Token![,]) || input.peek2(Token![;])
}

//----------------------------------------------------------------------
//
//  Modes for indirect addressing.
//
//----------------------------------------------------------------------
#[derive(Clone, Debug)]
struct IndAddr {
    reg: Reg,
    offset: Displacement,
}

impl Parse for IndAddr {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let content;
        syn::bracketed!(content in input);
        let ident: Ident = content.parse()?;
        let reg = match Reg::from_str(&ident.to_string()) {
            None => return Err(content.error("expected register name.")),
            Some(reg) => reg,
        };
        let offset: Displacement = content.parse()?;
        Ok(IndAddr { reg, offset })
    }
}

//----------------------------------------------------------------------
//
//  Displacement for indirect addressing.
//
//----------------------------------------------------------------------

#[derive(Clone, Debug)]
enum Displacement {
    Const(i32),
    Expr(TokenStream),
}

impl std::fmt::Display for Displacement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Displacement::Const(i) => write!(f, "{}", i),
            Displacement::Expr(ts) => write!(f, "({})", ts),
        }
    }
}

impl Parse for Displacement {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let lookahead = input.lookahead1();
        let offset = if input.is_empty() {
            Displacement::Const(0)
        } else if lookahead.peek(Token![-]) || lookahead.peek(Token![+]) {
            let sign = match input.parse::<Punct>()?.as_char() {
                '-' => -1,
                '+' => 1,
                _ => return Err(lookahead.error()),
            };
            let lookahead = input.lookahead1();
            if lookahead.peek(token::Paren) {
                let content;
                syn::parenthesized!(content in input);
                let expr: Expr = content.parse()?;
                let expr = if sign == 1 {
                    quote!(expr)
                } else {
                    quote!(-(#expr))
                };
                Displacement::Expr(expr)
            } else if lookahead.peek(LitInt) {
                let ofs: i32 = input.parse::<LitInt>()?.base10_parse()?;
                Displacement::Const(ofs * sign)
            } else {
                return Err(lookahead.error());
            }
        } else {
            return Err(lookahead.error());
        };
        Ok(offset)
    }
}

impl Parse for Operand {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        if input.peek(Ident) {
            let op = input.parse::<Ident>()?.to_string();
            if op == "R" {
                let content;
                syn::parenthesized!(content in input);
                let s = content.parse::<Expr>()?;
                Ok(Operand::reg(quote!(#s)))
            } else {
                let reg = Reg::from_str(&op).ok_or(input.error("Expected register name."))? as u64;
                Ok(Operand::reg(quote!(#reg)))
            }
        } else if input.peek(LitInt) && is_single(input) {
            let imm = input.parse::<LitInt>()?;
            Ok(Operand::Imm(quote! { #imm }))
        } else if input.peek(token::Bracket) {
            let addr: IndAddr = input.parse()?;
            match addr.offset {
                Displacement::Const(i) => {
                    let disp = if i == 0 {
                        Disp::None
                    } else if std::i8::MIN as i32 <= i && i <= std::i8::MAX as i32 {
                        Disp::D8(i as i8)
                    } else {
                        Disp::D32(i)
                    };
                    Ok(Operand::Ind(addr.reg, disp))
                }
                Displacement::Expr(e) => Ok(Operand::Ind(addr.reg, Disp::Expr(e))),
            }
        } else if input.peek(token::Paren) {
            let gr = input.parse::<Group>()?;
            Ok(Operand::Imm(gr.stream()))
        } else {
            return Err(input.error("Expected register name, integer literal, memory reference, or Rust expression with parenthesis."));
        }
    }
}

//----------------------------------------------------------------------
//
//  Instruction / Operands definitions.
//
//----------------------------------------------------------------------

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
            let inst = input.parse()?;
            //println!("{:?}", &inst);
            stmts.contents.push(inst);
        }
        Ok(stmts)
    }
}
