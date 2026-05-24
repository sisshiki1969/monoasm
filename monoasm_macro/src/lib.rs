#![feature(proc_macro_hygiene)]
#![allow(dead_code)]
#![allow(unreachable_patterns)]

extern crate proc_macro;
extern crate quote;
extern crate syn;
use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;
mod arm64;
mod asm;
mod inst;
mod parse;
use asm::*;

//----------------------------------------------------------------------
//
//  Entry point for monoasm proc-macro.
//
//----------------------------------------------------------------------

#[proc_macro]
pub fn monoasm(tokens: TokenStream) -> TokenStream {
    let stmts = parse_macro_input!(tokens as inst::Stmts);
    let base = stmts.base;
    let mut ts = quote!(let mut jit = #base;);
    ts.extend(stmts.contents.into_iter().map(compile));
    let ts = quote!({ #ts }).into();
    //eprintln!("{}", ts);
    ts
}

//----------------------------------------------------------------------
//
//  Entry point for the AArch64 monoasm proc-macro.
//
//  A proc-macro cannot observe the target architecture, so AArch64 code
//  uses this separate entry point instead of `monoasm!`.
//
//----------------------------------------------------------------------

#[proc_macro]
pub fn monoasm_arm64(tokens: TokenStream) -> TokenStream {
    let stmts = parse_macro_input!(tokens as arm64::Stmts);
    let base = stmts.base;
    let mut ts = quote!(let mut jit = #base;);
    ts.extend(stmts.contents.into_iter().map(arm64::compile));
    quote!({ #ts }).into()
}
