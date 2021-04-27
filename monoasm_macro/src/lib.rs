#![feature(proc_macro_hygiene)]
#![allow(dead_code)]
#![allow(unreachable_patterns)]

extern crate proc_macro;
extern crate quote;
extern crate syn;
//use proc_macro2::TokenStream;
use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;
mod asm;
mod inst;
mod parse;
use asm::*;
use parse::*;

//----------------------------------------------------------------------
//
//  Entry point for monoasm proc-macro.
//
//----------------------------------------------------------------------

#[proc_macro]
pub fn monoasm(tokens: TokenStream) -> TokenStream {
    let stmts = parse_macro_input!(tokens as Stmts);
    let base = stmts.base;
    let mut ts = quote!(let mut jit = &mut #base;);
    ts.extend(stmts.contents.into_iter().map(|stmt| {
        let item = compile(stmt);
        #[cfg(debug_assertions)]
        eprintln!("{}", item);
        item
    }));
    ts.into()
}
