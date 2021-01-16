#![feature(proc_macro_hygiene)]
#![allow(dead_code)]
#![allow(unreachable_patterns)]

extern crate proc_macro;
extern crate quote;
extern crate syn;
//use proc_macro2::TokenStream;
use quote::quote;
use syn::parse_macro_input;
mod asm;
mod inst;
mod parse;
use asm::*;
use inst::*;

#[proc_macro]
pub fn monoasm(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    //println!("in: {}", tokens);
    let stmts = parse_macro_input!(tokens as Stmts);
    let base = stmts.base;
    let mut ts = quote!(let mut jit = &mut #base;);
    for stmt in stmts.contents {
        let item = compile(stmt);
        println!("{}", item);
        ts.extend(item);
    }
    //println!("out: {}", ts.clone());
    ts.into()
}
