#![feature(proc_macro_hygiene)]
#![allow(unused_imports)]
extern crate proc_macro;
extern crate quote;
use proc_macro2::{Delimiter, Group, Ident, Punct, Spacing, Span, TokenStream, TokenTree};
use quote::quote;

#[derive(Clone)]
struct Context {
    tokens: Vec<TokenTree>,
    current: usize,
}

#[derive(Copy, Clone, PartialEq)]
enum Inst {
    Movq,
    Syscall,
    End,
}

impl Context {
    pub fn new(tokens: TokenStream) -> Context {
        let mut ctx = Context {
            tokens: vec![],
            current: 0,
        };
        for tok in tokens {
            ctx.tokens.push(tok);
        }
        ctx
    }

    pub fn get(&mut self) -> Option<TokenTree> {
        if self.current >= self.tokens.len() {
            None
        } else {
            self.current += 1;
            Some(self.tokens[self.current - 1].clone())
        }
    }

    pub fn get_until(&mut self, ch: &str) -> TokenStream {
        let mut tokens = vec![];
        loop {
            match self.get() {
                None => panic!("Expected '{}'.", ch),
                Some(tok) => match tok {
                    TokenTree::Punct(ref punct) if punct.to_string().as_str() == ch => {
                        break;
                    }
                    _ => tokens.push(tok),
                },
            }
        }
        let mut ts = TokenStream::new();
        ts.extend(tokens);
        ts
    }

    pub fn expect_punct(&mut self, ch: &str) -> TokenTree {
        match self.get() {
            None => unimplemented!("Needs '{}'.", ch),
            Some(tok) => match tok {
                TokenTree::Punct(ref punct) => {
                    if punct.to_string().as_str() == ch {
                        tok
                    } else {
                        unimplemented!("Needs '{}'.", ch)
                    }
                }
                _ => unimplemented!("Needs '{}'.", ch),
            },
        }
    }
}

#[proc_macro]
pub fn monoasm(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let tokens = TokenStream::from(tokens);
    let mut ctx = Context::new(tokens);
    let mut ts = TokenStream::new();

    loop {
        let inst = match ctx.get() {
            None => Inst::End,
            Some(tok) => match tok {
                TokenTree::Ident(ref ident) => match ident.to_string().as_str() {
                    "movq" => Inst::Movq,
                    "syscall" => Inst::Syscall,
                    _ => unimplemented!("Unimplemented instruction."),
                },
                _ => unimplemented!("Illegal punctuator."),
            },
        };
        match inst {
            Inst::Movq => {
                let op1 = substitute(ctx.get_until(","));
                let op2 = substitute(ctx.get_until(";"));
                ts.extend(quote!(jit.movq(#op1, #op2);));
            }
            Inst::Syscall => {
                ctx.expect_punct(";");
                ts.extend(quote!(jit.syscall();));
            }
            Inst::End => break,
        }
    }
    println!("{:?}", ts);
    proc_macro::TokenStream::from(ts)
}

fn substitute(tokens: TokenStream) -> TokenStream {
    let mut ts = TokenStream::new();
    for tok in tokens {
        let subst = match tok {
            TokenTree::Ident(ref ident) => match &*ident.to_string() {
                "rax" => quote!(Or::Reg(Reg::Rax)),
                "rcx" => quote!(Or::Reg(Reg::Rcx)),
                "rdx" => quote!(Or::Reg(Reg::Rdx)),
                "rbx" => quote!(Or::Reg(Reg::Rbx)),
                "rsp" => quote!(Or::Reg(Reg::Rsp)),
                "rbp" => quote!(Or::Reg(Reg::Rbp)),
                "rsi" => quote!(Or::Reg(Reg::Rsi)),
                "rdi" => quote!(Or::Reg(Reg::Rdi)),
                "r8" => quote!(Or::Reg(Reg::R8)),
                "r9" => quote!(Or::Reg(Reg::R9)),
                "r10" => quote!(Or::Reg(Reg::R10)),
                "r11" => quote!(Or::Reg(Reg::R11)),
                "r12" => quote!(Or::Reg(Reg::R12)),
                "r13" => quote!(Or::Reg(Reg::R13)),
                "r14" => quote!(Or::Reg(Reg::R14)),
                "r15" => quote!(Or::Reg(Reg::R15)),
                _ => TokenStream::from(tok),
            },
            TokenTree::Group(ref group) => {
                let gr = Group::new(group.delimiter(), substitute(group.stream()));
                TokenStream::from(TokenTree::Group(gr))
            }
            _ => TokenStream::from(tok),
        };
        ts.extend(subst);
    }
    ts
}
