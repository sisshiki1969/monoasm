extern crate proc_macro2;
extern crate quote;
extern crate syn;
pub use super::parse::*;
use proc_macro2::Group;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream};
use syn::Expr;
use syn::LitFloat;
use syn::{token, Error, Ident, LitInt, Token};

///----------------------------------------------------------------------
///
///  Assembly statements.
///
///----------------------------------------------------------------------
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
            let inst = input.parse::<Inst>()?;
            //println!("{:?}", &inst);
            stmts.contents.push(inst);
        }
        Ok(stmts)
    }
}

///----------------------------------------------------------------------
///
///  Assembly instructions.
///
///----------------------------------------------------------------------
#[derive(Clone, Debug)]
pub enum Inst {
    Label(Ident),

    F64(f64),
    I64(i64),

    Movq(MovOperand, MovOperand),
    Movl(RmOperand, RmiOperand),
    Movsxl(Register, RmOperand),
    Movzxw(Register, RmOperand),
    Movsxw(Register, RmOperand),
    Movzxb(Register, RmOperand),
    Movsxb(Register, RmOperand),
    Addq(RmOperand, RmiOperand),
    Orq(RmOperand, RmiOperand),
    Adcq(RmOperand, RmiOperand),
    Sbbq(RmOperand, RmiOperand),
    Andq(RmOperand, RmiOperand),
    Subq(RmOperand, RmiOperand),
    Xorq(RmOperand, RmiOperand),
    Cmpq(RmOperand, RmiOperand),
    Cmpb(RmOperand, RmiOperand),
    Negq(RmOperand),

    Shlq(RmOperand, RiOperand),
    Shrq(RmOperand, RiOperand),
    Salq(RmOperand, RiOperand),
    Sarq(RmOperand, RiOperand),

    Imul(RmiOperand, RmiOperand),
    Idiv(RmOperand),

    Lea(RmOperand, RmOperand),

    Testq(RmOperand, RmiOperand),

    Setcc(Flag, RmOperand),
    Cqo,

    Movsd(XmOperand, XmOperand),
    Addsd(Xmm, XmOperand),
    Subsd(Xmm, XmOperand),
    Mulsd(Xmm, XmOperand),
    Divsd(Xmm, XmOperand),
    UComIsd(Xmm, XmOperand),

    Cvtsi2sdq(Xmm, RmOperand),

    Pushq(RmOperand),
    Popq(RmOperand),

    Jmp(Dest),
    Jcc(Cond, Ident),

    Call(Dest),
    Leave,
    Ret,
    Syscall,
}

///----------------------------------------------------------------------
///
///  Comparison kinds.
///
///----------------------------------------------------------------------
#[derive(Clone, Debug)]
pub enum Cond {
    Ne,
    Eq,
    Gt,
    Ge,
    Lt,
    Le,
    A,
    Ae,
    B,
    Be,
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

        macro_rules! parse_jcc {
            ($inst: ident) => (
                {
                    let op = input.parse::<Ident>()?;
                    input.parse::<Token![;]>()?;
                    Ok(Inst::Jcc(Cond::$inst, op))
                }
            )
        }

        macro_rules! parse_set {
            ($inst: ident, $flag: ident) => (
                {
                    let op = input.parse()?;
                    input.parse::<Token![;]>()?;
                    Ok(Inst::$inst(Flag::$flag, op))
                }
            )
        }

        let ident = input.parse::<Ident>()?;
        if input.peek(Token![:]) {
            input.parse::<Token![:]>()?;
            Ok(Inst::Label(ident))
        } else {
            match ident.to_string().as_str() {
                "movq" => parse_2op!(Movq),
                "movl" => parse_2op!(Movl),
                "movsxl" => parse_2op!(Movsxl),
                "movzxw" => parse_2op!(Movzxw),
                "movsxw" => parse_2op!(Movsxw),
                "movzxb" => parse_2op!(Movzxb),
                "movsxb" => parse_2op!(Movsxb),
                "addq" => parse_2op!(Addq),
                "orq" => parse_2op!(Orq),
                "adcq" => parse_2op!(Adcq),
                "sbbq" => parse_2op!(Sbbq),
                "andq" => parse_2op!(Andq),
                "subq" => parse_2op!(Subq),
                "xorq" => parse_2op!(Xorq),
                "negq" => parse_1op!(Negq),
                "imul" => parse_2op!(Imul),
                "idiv" => parse_1op!(Idiv),

                "testq" => parse_2op!(Testq),
                "lea" => parse_2op!(Lea),

                "seteq" => parse_set!(Setcc, Eq),
                "setne" => parse_set!(Setcc, Ne),
                "setgt" => parse_set!(Setcc, Gt),
                "setge" => parse_set!(Setcc, Ge),
                "setlt" => parse_set!(Setcc, Lt),
                "setle" => parse_set!(Setcc, Le),
                "seta" => parse_set!(Setcc, A),
                "setae" => parse_set!(Setcc, Ae),
                "setb" => parse_set!(Setcc, B),
                "setbe" => parse_set!(Setcc, Be),
                "cqo" => parse_0op!(Cqo),

                "shlq" => parse_2op!(Shlq),
                "shrq" => parse_2op!(Shrq),
                "salq" => parse_2op!(Salq),
                "sarq" => parse_2op!(Sarq),

                "movsd" => parse_2op!(Movsd),
                "addsd" => parse_2op!(Addsd),
                "subsd" => parse_2op!(Subsd),
                "mulsd" => parse_2op!(Mulsd),
                "divsd" => parse_2op!(Divsd),
                "ucomisd" => parse_2op!(UComIsd),

                "cvtsi2sdq" => parse_2op!(Cvtsi2sdq),

                "pushq" => parse_1op!(Pushq),
                "popq" => parse_1op!(Popq),
                "cmpq" => parse_2op!(Cmpq),
                "cmpb" => parse_2op!(Cmpb),
                "call" => parse_1op!(Call),
                "ret" => parse_0op!(Ret),
                "jmp" => parse_1op!(Jmp),
                "jne" => parse_jcc!(Ne),
                "jeq" => parse_jcc!(Eq),
                "jgt" => parse_jcc!(Gt),
                "jge" => parse_jcc!(Ge),
                "jlt" => parse_jcc!(Lt),
                "jle" => parse_jcc!(Le),
                "jae" => parse_jcc!(Ae),
                "ja" => parse_jcc!(A),
                "jbe" => parse_jcc!(Be),
                "jb" => parse_jcc!(B),
                "syscall" => parse_0op!(Syscall),
                "leave" => parse_0op!(Leave),

                "dq" => {
                    if input.peek(LitFloat) {
                        let f = input.parse::<LitFloat>()?.base10_parse()?;
                        input.parse::<Token![;]>()?;
                        Ok(Inst::F64(f))
                    } else if input.peek(LitInt) {
                        let i = input.parse::<LitInt>()?.base10_parse()?;
                        input.parse::<Token![;]>()?;
                        Ok(Inst::I64(i))
                    } else {
                        Err(input.error("unimplemented literal."))
                    }
                }
                _ => Err(Error::new(ident.span(), "unimplemented instruction.")),
            }
        }
    }
}

///----------------------------------------------------------------------
///
///  General register / memory reference.
///
///----------------------------------------------------------------------
#[derive(Clone, Debug)]
pub enum RmOperand {
    Reg(Register),
    Ind(IndAddr),
}

impl Parse for RmOperand {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        if input.peek(Ident) {
            // e.g. "rax"
            let reg = input.parse::<Register>()?;
            Ok(Self::reg(reg))
        } else if input.peek(token::Bracket) {
            // e.g. "[rax + 4]", "[rax - (4)]"
            Ok(Self::Ind(input.parse::<IndAddr>()?))
        } else {
            Err(input.error("Expected register name or memory reference."))
        }
    }
}

impl std::fmt::Display for RmOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Reg(reg) => write!(f, "{}", reg),
            Self::Ind(ind) => write!(f, "{}", ind),
        }
    }
}

impl ToTokens for RmOperand {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ts = match self {
            Self::Reg(ts) => quote!(Rm::reg(#ts)),
            Self::Ind(ind) => quote!(#ind),
        };
        tokens.extend(ts);
    }
}

impl RmOperand {
    pub fn reg(reg: Register) -> Self {
        Self::Reg(reg)
    }
}

///----------------------------------------------------------------------
///
///  General register / memory reference / immediate Operands.
///
///----------------------------------------------------------------------
#[derive(Clone, Debug)]
pub enum RmiOperand {
    Imm(TokenStream),
    Reg(Register),
    Ind(IndAddr),
}

impl Parse for RmiOperand {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        if input.peek(Ident) {
            // e.g. "rax"
            let reg = input.parse::<Register>()?;
            Ok(RmiOperand::reg(reg))
        } else if input.peek(LitInt) && is_single(input) {
            // e.g. "42"
            let imm = input.parse::<LitInt>()?;
            Ok(RmiOperand::imm(imm))
        } else if input.peek(token::Bracket) {
            // e.g. "[rax + 4]", "[rax - (4)]"
            Ok(RmiOperand::Ind(input.parse::<IndAddr>()?))
        } else if input.peek(token::Paren) {
            // e.g. "(42)"
            let gr = input.parse::<Group>()?;
            Ok(RmiOperand::Imm(gr.stream()))
        } else {
            Err(input.error("Expected register name, integer literal, memory reference, or Rust expression with parenthesis."))
        }
    }
}

impl std::fmt::Display for RmiOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RmiOperand::Imm(i) => write!(f, "{}", i),
            RmiOperand::Reg(reg) => write!(f, "{}", reg),
            RmiOperand::Ind(ind) => write!(f, "{}", ind),
        }
    }
}

impl ToTokens for RmiOperand {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ts = match self {
            RmiOperand::Imm(_) => unreachable!("immediate"),
            RmiOperand::Reg(ts) => quote!(Rm::reg(#ts)),
            RmiOperand::Ind(ind) => quote!(#ind),
        };
        tokens.extend(ts);
    }
}

impl RmiOperand {
    pub fn reg(reg: Register) -> Self {
        Self::Reg(reg)
    }

    pub fn imm(imm: impl ToTokens) -> Self {
        Self::Imm(quote! { #imm })
    }
}

///----------------------------------------------------------------------
///
///  General register / immediate Operands.
///
///----------------------------------------------------------------------
#[derive(Clone, Debug)]
pub enum RiOperand {
    Imm(TokenStream),
    Reg(Register),
}

impl Parse for RiOperand {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        if input.peek(Ident) {
            // e.g. "rax"
            let reg = input.parse::<Register>()?;
            Ok(RiOperand::reg(reg))
        } else if input.peek(LitInt) && is_single(input) {
            // e.g. "42"
            let imm = input.parse::<LitInt>()?;
            Ok(RiOperand::imm(imm))
        } else if input.peek(token::Paren) {
            // e.g. "(42)"
            let gr = input.parse::<Group>()?;
            Ok(RiOperand::Imm(gr.stream()))
        } else {
            Err(input.error("Expected register name, integer literal, memory reference, or Rust expression with parenthesis."))
        }
    }
}

impl std::fmt::Display for RiOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RiOperand::Imm(i) => write!(f, "{}", i),
            RiOperand::Reg(reg) => write!(f, "{}", reg),
        }
    }
}

impl ToTokens for RiOperand {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ts = match self {
            RiOperand::Imm(_) => unreachable!("immediate"),
            RiOperand::Reg(ts) => quote!(Rm::reg(#ts)),
        };
        tokens.extend(ts);
    }
}

impl RiOperand {
    pub fn reg(reg: Register) -> Self {
        Self::Reg(reg)
    }

    pub fn imm(imm: impl ToTokens) -> Self {
        Self::Imm(quote! { #imm })
    }
}

///----------------------------------------------------------------------
///
///  Floating pointer register(xmm) / memory reference Operands.
///
///----------------------------------------------------------------------
#[derive(Clone, Debug)]
pub enum XmOperand {
    Xmm(TokenStream),
    Ind(IndAddr),
}

fn parse_xmm(input: ParseStream, ident: &String) -> Result<TokenStream, Error> {
    assert!(ident.starts_with("xmm"));
    if ident.len() == 3 {
        if input.peek(token::Paren) {
            let gr = input.parse::<Group>()?;
            Ok(gr.stream())
        } else {
            Err(input.error(format!(
                "Expected xmm register number. e.g. xmm0 or xmm(0) actual:{}",
                ident,
            )))
        }
    } else {
        if let Ok(no) = ident[3..].parse::<u8>() {
            if no > 15 {
                Err(input.error(format!("Invalid xmm register name. {}", ident)))
            } else {
                Ok(quote!(#no as u64))
            }
        } else {
            Err(input.error(format!("Invalid xmm register name. {}", ident)))
        }
    }
}

impl Parse for XmOperand {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        if input.peek(Ident) {
            let reg = input.parse::<Ident>()?.to_string();
            if reg.starts_with("xmm") {
                Ok(Self::Xmm(parse_xmm(input, &reg)?))
            } else {
                Err(input.error("Expected xmm register name or memory reference."))
            }
        } else if input.peek(token::Bracket) {
            Ok(Self::Ind(input.parse::<IndAddr>()?))
        } else {
            Err(input.error("Expected xmm register name or memory reference."))
        }
    }
}

impl std::fmt::Display for XmOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Xmm(reg) => write!(f, "xmm({})", reg),
            Self::Ind(ind) => write!(f, "{}", ind),
        }
    }
}

impl ToTokens for XmOperand {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ts = match self {
            Self::Xmm(ts) => quote!(Rm::reg(Reg::from(#ts))),
            Self::Ind(ind) => quote!(#ind),
        };
        tokens.extend(ts);
    }
}

///----------------------------------------------------------------------
///
///  General register / Xmm register / memory reference / immediate Operands.
///
///----------------------------------------------------------------------
#[derive(Clone, Debug)]
pub enum MovOperand {
    Imm(TokenStream),
    Reg(Register),
    Xmm(TokenStream),
    Ind(IndAddr),
}

impl Parse for MovOperand {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        if input.peek(Ident) {
            let ident = input.parse::<Ident>()?.to_string();
            if ident.starts_with("xmm") {
                Ok(Self::Xmm(parse_xmm(input, &ident)?))
            } else {
                Ok(Self::reg(Register::parse_register(input, &ident)?))
            }
        } else if input.peek(LitInt) && is_single(input) {
            // e.g. "42"
            let imm = input.parse::<LitInt>()?;
            Ok(Self::imm(imm))
        } else if input.peek(token::Bracket) {
            // e.g. "[rax + 4]", "[rax - (4)]"
            Ok(Self::Ind(input.parse::<IndAddr>()?))
        } else if input.peek(token::Paren) {
            // e.g. "(42)"
            let gr = input.parse::<Group>()?;
            Ok(Self::imm(gr.stream()))
        } else {
            Err(input.error("Expected register name, integer literal, memory reference, or Rust expression with parenthesis."))
        }
    }
}

impl std::fmt::Display for MovOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Imm(i) => write!(f, "{}", i),
            Self::Reg(reg) => write!(f, "{}", reg),
            Self::Xmm(reg) => write!(f, "xmm({})", reg),
            Self::Ind(ind) => write!(f, "{}", ind),
        }
    }
}

impl ToTokens for MovOperand {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ts = match self {
            Self::Imm(_) => unreachable!("immediate"),
            Self::Reg(r) => quote!(Rm::reg(#r)),
            Self::Xmm(ts) => quote!(Rm::reg(Reg::from(#ts))),
            Self::Ind(ind) => quote!(#ind),
        };
        tokens.extend(ts);
    }
}

impl MovOperand {
    pub fn reg(reg: Register) -> Self {
        Self::Reg(reg)
    }

    pub fn imm(imm: impl ToTokens) -> Self {
        Self::Imm(quote! { #imm })
    }
}
