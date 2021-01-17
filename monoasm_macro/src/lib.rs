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
use inst::*;

#[proc_macro]
pub fn monoasm(tokens: TokenStream) -> TokenStream {
    let stmts = parse_macro_input!(tokens as Stmts);
    let base = stmts.base;
    let mut ts = quote!(let mut jit = &mut #base;);
    for stmt in stmts.contents {
        let item = compile(stmt);
        #[cfg(debug_assertions)]
        println!("{}", item);
        ts.extend(item);
    }
    ts.into()
}
