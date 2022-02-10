use super::inst::*;
use proc_macro2::Punct;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    Expr,
};
use syn::{token, Error, LitInt, Token};

//----------------------------------------------------------------------
//
//  Displacement for indirect addressing.
//
//----------------------------------------------------------------------
#[derive(Clone, Debug)]
pub enum Disp {
    Imm(TokenStream),
}

impl ToTokens for Disp {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Imm(ts) => tokens.extend(ts.clone()),
        }
    }
}

impl std::fmt::Display for Disp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Imm(ts) => write!(f, "{}", ts),
        }
    }
}

impl Parse for Disp {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let lookahead = input.lookahead1();
        if input.is_empty() {
            // e.g. "[rax]"
            Ok(Disp::Imm(quote!(0i32)))
        } else if lookahead.peek(Token![-]) || lookahead.peek(Token![+]) {
            let sign = match input.parse::<Punct>()?.as_char() {
                '-' => -1,
                '+' => 1,
                _ => return Err(lookahead.error()),
            };
            let lookahead = input.lookahead1();
            if lookahead.peek(token::Paren) {
                // e.g. "[rax - (4)]"
                let content;
                syn::parenthesized!(content in input);
                let expr = content.parse::<Expr>()?;
                let expr = if sign == 1 {
                    quote!(expr as i32)
                } else {
                    quote!(-(#expr) as i32)
                };
                Ok(Disp::Imm(expr))
            } else if lookahead.peek(LitInt) {
                // e.g. "[rax + 4]"
                let ofs: i32 = input.parse::<LitInt>()?.base10_parse()?;
                let expr = if sign == 1 {
                    quote!(#ofs as i32)
                } else {
                    quote!(-(#ofs) as i32)
                };
                Ok(Disp::Imm(expr))
            } else {
                Err(lookahead.error())
            }
        } else {
            Err(lookahead.error())
        }
    }
}
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
