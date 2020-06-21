#![feature(proc_macro_hygiene)]
#![allow(dead_code)]
#![allow(unreachable_patterns)]

extern crate proc_macro;
extern crate quote;
extern crate syn;
use proc_macro2::{TokenStream};
use syn::{parse_macro_input};
mod inst;
mod asm;
mod parse;
use inst::*;
use asm::*;

#[proc_macro]
pub fn monoasm(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    println!("in: {}", tokens);
    let stmts = parse_macro_input!(tokens as Stmts);
    let mut ts = TokenStream::new();
    for stmt in stmts.contents {
        let item = compile(stmt);
        println!("{}", item);
        ts.extend(item);
    }
    //println!("out: {}", ts.clone());
    ts.into()
}