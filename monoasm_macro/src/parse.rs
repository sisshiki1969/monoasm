use super::inst::*;
use monoasm_inst::Reg;
use proc_macro2::{Group, Punct};
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    Expr, ExprLit, Lit,
};
use syn::{token, Error, Ident, LitInt, Token};

#[derive(Clone, Debug)]
struct Addr {
    reg: Reg,
    offset: Imm,
}

impl Parse for Operand {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        if input.peek(Ident) {
            let op = input.parse::<Ident>()?.to_string();
            if op == "R" {
                let content;
                syn::parenthesized!(content in input);
                let s = content.parse::<Expr>()?;
                match &s {
                    Expr::Lit(ExprLit {
                        lit: Lit::Int(i), ..
                    }) => {
                        let i = i.base10_parse::<u64>()?;
                        Ok(Operand::Reg(Reg::from(i)))
                    }
                    _ => Ok(Operand::RegExpr(quote!(#s))),
                }
            } else {
                let reg = Reg::from_str(&op).ok_or(input.error("Expected register name."))?;
                Ok(Operand::Reg(reg))
            }
        } else if input.peek(LitInt) && is_single(input) {
            let imm = input.parse::<LitInt>()?;
            Ok(Operand::Imm(quote! { #imm }))
        } else if input.peek(token::Bracket) {
            let addr: Addr = input.parse()?;
            match addr.offset {
                Imm::Imm(i) => {
                    let disp = if i == 0 {
                        Disp::None
                    } else if std::i8::MIN as i32 <= i && i <= std::i8::MAX as i32 {
                        Disp::D8(i as i8)
                    } else {
                        Disp::D32(i)
                    };
                    Ok(Operand::Ind(addr.reg, disp))
                }
                Imm::Expr(e) => Ok(Operand::Ind(addr.reg, Disp::Expr(e))),
            }
        } else if input.peek(token::Paren) {
            let gr = input.parse::<Group>()?;
            Ok(Operand::Imm(gr.stream()))
        } else {
            return Err(input.error("Expected register name, integer literal, memory reference, or Rust expression with parenthesis."));
        }
    }
}

impl Parse for Dest {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Ident) && is_single(input) {
            let dest: Ident = input.parse()?;
            let reg = Reg::from_str(&dest.to_string());
            match reg {
                Some(reg) => Ok(Dest::Reg(reg)),
                None => Ok(Dest::Rel(dest)),
            }
        } else {
            Err(lookahead.error())
        }
    }
}

impl Parse for Imm {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let lookahead = input.lookahead1();
        let offset = if input.is_empty() {
            Imm::Imm(0)
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
                Imm::Expr(expr)
            } else if lookahead.peek(LitInt) {
                let ofs: i32 = input.parse::<LitInt>()?.base10_parse()?;
                Imm::Imm(ofs * sign)
            } else {
                return Err(lookahead.error());
            }
        } else {
            return Err(lookahead.error());
        };
        Ok(offset)
    }
}

impl Parse for Addr {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let content;
        syn::bracketed!(content in input);
        let ident: Ident = content.parse()?;
        let reg = match Reg::from_str(&ident.to_string()) {
            None => return Err(content.error("expected register name.")),
            Some(reg) => reg,
        };
        let offset: Imm = content.parse()?;
        Ok(Addr { reg, offset })
    }
}

fn is_single(input: ParseStream) -> bool {
    input.peek2(Token![,]) || input.peek2(Token![;])
}

impl Parse for Inst {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        macro_rules! parse_2op {
            ($inst: ident) => (
                {
                    let op1 = input.parse()?;
                    input.parse::<Token![,]>()?;
                    let op2 = input.parse()?;
                    input.parse::<Token![;]>()?;
                    Ok(Inst::$inst(op1, op2))
                }
            )
        }

        macro_rules! parse_1op {
            ($inst: ident) => (
                {
                    let op = input.parse()?;
                    input.parse::<Token![;]>()?;
                    Ok(Inst::$inst(op))
                }
            )
        }

        macro_rules! parse_0op {
            ($inst: ident) => (
                {
                    input.parse::<Token![;]>()?;
                    Ok(Inst::$inst)
                }
            )
        }

        let inst: Ident = input.parse()?;
        if input.peek(Token![:]) {
            input.parse::<Token![:]>()?;
            Ok(Inst::Label(inst))
        } else {
            match inst.to_string().as_str() {
                "movq" => parse_2op!(Movq),
                "addq" => parse_2op!(Addq),
                "orq" => parse_2op!(Orq),
                "adcq" => parse_2op!(Adcq),
                "sbbq" => parse_2op!(Sbbq),
                "andq" => parse_2op!(Andq),
                "subq" => parse_2op!(Subq),
                "xorq" => parse_2op!(Xorq),
                "imull" => parse_2op!(Imull),

                "pushq" => parse_1op!(Pushq),
                "popq" => parse_1op!(Popq),
                "cmpq" => parse_2op!(Cmpq),
                "call" => parse_1op!(Call),
                "ret" => parse_0op!(Ret),
                "jmp" => parse_1op!(Jmp),
                "jne" => parse_1op!(Jne),
                "je" => parse_1op!(Je),
                "syscall" => parse_0op!(Syscall),
                _ => Err(Error::new(inst.span(), "unimplemented instruction.")),
            }
        }
    }
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
