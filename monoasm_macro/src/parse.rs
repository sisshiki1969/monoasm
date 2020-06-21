use syn::parse::{Parse, ParseStream};
use syn::{token, Error, Ident, LitInt, Token};
use proc_macro2::{Group, Punct};
use quote::quote;
use super::inst::*;

#[derive(Clone, Debug)]
struct Addr {
    reg: Reg,
    offset: Imm,
}

impl Parse for Operand {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Ident) && is_single(input) {
            let op: Ident = input.parse()?;
            let reg = Reg::from_str(op.to_string().as_str()).ok_or(lookahead.error())?;
            Ok(Operand::Reg(reg))
        } else if lookahead.peek(LitInt) && is_single(input) {
            let imm = input.parse::<LitInt>()?;
            Ok(Operand::Imm(imm.base10_parse()?))
        } else if lookahead.peek(token::Bracket) {
            let gr = input.parse::<Group>()?;
            let addr: Addr = syn::parse2(gr.stream())?;
            match addr.offset {
                Imm::Imm(i) if i == 0 => Ok(Operand::Ind(addr.reg)),
                _ => Ok(Operand::IndDisp(addr.reg, addr.offset)),
            }
        } else if lookahead.peek(token::Paren) {
            let gr = input.parse::<Group>()?;
            Ok(Operand::Expr(gr.stream()))
        } else {
            return Err(lookahead.error());
        }
    }
}

impl Parse for Dest {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Ident) && is_single(input) {
            let dest: Ident = input.parse()?;
            let reg = Reg::from_str(dest.to_string().as_str());
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
                let gr = input.parse::<Group>()?;
                let expr = if sign == 1 {
                    quote!( #gr )
                } else {
                    quote!( -(#gr) )
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
        let ident: Ident = input.parse()?;
        let reg = match Reg::from_str(ident.to_string().as_str()) {
            None => return Err(input.error("expected register name.")),
            Some(reg) => reg,
        };
        let offset: Imm = input.parse()?;
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
                "syscall" => parse_0op!(Syscall),
                _ => Err(Error::new(inst.span(), "unimplemented instruction.")),
            }
        }
    }
}

impl Parse for Stmts {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let mut stmts = Stmts { contents: vec![] };
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
