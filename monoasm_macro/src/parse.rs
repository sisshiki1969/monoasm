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
pub struct Disp(TokenStream);

impl ToTokens for Disp {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(self.0.clone());
    }
}

impl std::fmt::Display for Disp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Parse for Disp {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let lookahead = input.lookahead1();
        let offset = if input.is_empty() {
            Disp(quote!(0))
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
                Disp(expr)
            } else if lookahead.peek(LitInt) {
                let ofs: i32 = input.parse::<LitInt>()?.base10_parse()?;
                let expr = if sign == 1 {
                    quote!(#ofs)
                } else {
                    quote!(-(#ofs))
                };
                Disp(expr)
            } else {
                return Err(lookahead.error());
            }
        } else {
            return Err(lookahead.error());
        };
        Ok(offset)
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
