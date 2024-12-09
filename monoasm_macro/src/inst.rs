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
    Movw(RmOperand, RmiOperand),
    Movb(RmOperand, RmiOperand),
    Movsxl(Register, RmOperand),
    Movzxw(Register, RmOperand),
    Movsxw(Register, RmOperand),
    Movzxb(Register, RmOperand),
    Movsxb(Register, RmOperand),
    Add(OperandSize, RmOperand, RmiOperand),
    Sub(OperandSize, RmOperand, RmiOperand),
    Adc(OperandSize, RmOperand, RmiOperand),
    Sbb(OperandSize, RmOperand, RmiOperand),
    And(OperandSize, RmOperand, RmiOperand),
    Or(OperandSize, RmOperand, RmiOperand),
    Xor(OperandSize, RmOperand, RmiOperand),
    Cmp(OperandSize, RmOperand, RmiOperand),
    Xchg(OperandSize, RmOperand, RmOperand),
    Negq(RmOperand),
    Notq(RmOperand),

    Shl(OperandSize, RmOperand, RiOperand),
    Shr(OperandSize, RmOperand, RiOperand),
    Sal(OperandSize, RmOperand, RiOperand),
    Sar(OperandSize, RmOperand, RiOperand),
    Rol(OperandSize, RmOperand, RiOperand),
    Ror(OperandSize, RmOperand, RiOperand),

    Imul(RmiOperand, RmiOperand),
    Idiv(RmOperand),
    Idivl(RmOperand),
    Div(RmOperand),
    Divl(RmOperand),

    Lea(RmOperand, RmOperand),

    Testq(RmOperand, RiOperand),
    Testb(RmOperand, RiOperand),

    Setcc(Cond, RmOperand),
    Cmovcc(OperandSize, Cond, Register, RmOperand),
    Cqo,
    Cdq,

    Movsd(XmOperand, XmOperand),
    Addsd(Xmm, XmOperand),
    Subsd(Xmm, XmOperand),
    Mulsd(Xmm, XmOperand),
    Divsd(Xmm, XmOperand),
    Xorps(Xmm, XmOperand),
    UComIsd(Xmm, XmOperand),
    Minsd(Xmm, XmOperand),
    Maxsd(Xmm, XmOperand),

    Andpd(Xmm, XmOperand),
    Xorpd(Xmm, XmOperand),
    Roundpd(Xmm, XmOperand, ImmOperand),
    Cvtsi2sdq(Xmm, RmOperand),
    Cvttsd2siq(Register, XmOperand),
    Sqrtpd(Xmm, XmOperand),
    Sqrtsd(Xmm, XmOperand),

    Pushq(RmOperand),
    Popq(RmOperand),

    Jmp(Dest),
    Jcc(Cond, Ident),

    Call(Dest),
    Leave,
    Ret,
    Syscall,

    Lzcnt(OperandSize, Register, RmOperand),
    Tzcnt(OperandSize, Register, RmOperand),
    Popcnt(OperandSize, Register, RmOperand),

    Int3,
}

///----------------------------------------------------------------------
///
///  Operand size.
///
///----------------------------------------------------------------------
#[derive(Clone, Debug, PartialEq)]
pub enum OperandSize {
    QWORD = 8,
    DWORD = 4,
    WORD = 2,
    BYTE = 1,
}

///----------------------------------------------------------------------
///
///  Comparison kinds.
///
///----------------------------------------------------------------------
#[derive(Debug, Clone, Copy)]
pub enum Cond {
    O = 0,
    No = 1,
    B = 2,
    Ae = 3,
    Eq = 4,
    Ne = 5,
    Be = 6,
    A = 7,
    S = 8,
    Ns = 9,
    P = 10,
    Np = 11,
    Lt = 12,
    Ge = 13,
    Le = 14,
    Gt = 15,
}

impl Cond {
    pub fn from(s: &str) -> Option<Self> {
        let cond = match s {
            "o" => Cond::O,
            "no" => Cond::No,
            "b" => Cond::B,
            "c" => Cond::B,
            "nae" => Cond::B,
            "ae" => Cond::Ae,
            "nc" => Cond::Ae,
            "eq" => Cond::Eq,
            "e" => Cond::Eq,
            "z" => Cond::Eq,
            "ne" => Cond::Ne,
            "nz" => Cond::Ne,
            "be" => Cond::Be,
            "na" => Cond::Be,
            "a" => Cond::A,
            "nbe" => Cond::A,
            "s" => Cond::S,
            "ns" => Cond::Ns,
            "p" => Cond::P,
            "pe" => Cond::P,
            "np" => Cond::Np,
            "po" => Cond::Np,
            "lt" => Cond::Lt,
            "l" => Cond::Lt,
            "nge" => Cond::Lt,
            "ge" => Cond::Ge,
            "nl" => Cond::Ge,
            "le" => Cond::Le,
            "ng" => Cond::Le,
            "gt" => Cond::Gt,
            "g" => Cond::Gt,
            "nle" => Cond::Gt,
            _ => return None,
        };
        Some(cond)
    }
}

impl Parse for Inst {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        macro_rules! parse_2op_sized {
            ($inst: ident, $size: ident) => (
                {
                    let op1 = input.parse()?;
                    input.parse::<Token![,]>()?;
                    let op2 = input.parse()?;
                    input.parse::<Token![;]>()?;
                    Ok(Inst::$inst(OperandSize::$size, op1, op2))
                }
            )
        }

        macro_rules! parse_3op {
            ($inst: ident) => (
                {
                    let op1 = input.parse()?;
                    input.parse::<Token![,]>()?;
                    let op2 = input.parse()?;
                    input.parse::<Token![,]>()?;
                    let op3 = input.parse()?;
                    input.parse::<Token![;]>()?;
                    Ok(Inst::$inst(op1, op2, op3))
                }
            )
        }

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
            ($flag: ident) => (
                {
                    let op = input.parse()?;
                    input.parse::<Token![;]>()?;
                    Ok(Inst::Setcc($flag, op))
                }
            )
        }

        macro_rules! parse_cmov {
            ($size: ident, $flag: ident) => (
                {
                    let op1 = input.parse()?;
                    input.parse::<Token![,]>()?;
                    let op2 = input.parse()?;
                    input.parse::<Token![;]>()?;
                    Ok(Inst::Cmovcc($size, $flag, op1, op2))
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
                "movw" => parse_2op!(Movw),
                "movb" => parse_2op!(Movb),
                "movsxl" => parse_2op!(Movsxl),
                "movzxw" => parse_2op!(Movzxw),
                "movsxw" => parse_2op!(Movsxw),
                "movzxb" => parse_2op!(Movzxb),
                "movsxb" => parse_2op!(Movsxb),

                "addq" => parse_2op_sized!(Add, QWORD),
                "addl" => parse_2op_sized!(Add, DWORD),
                "addw" => parse_2op_sized!(Add, WORD),
                "addb" => parse_2op_sized!(Add, BYTE),
                "orq" => parse_2op_sized!(Or, QWORD),
                "orl" => parse_2op_sized!(Or, DWORD),
                "orw" => parse_2op_sized!(Or, WORD),
                "orb" => parse_2op_sized!(Or, BYTE),
                "adcq" => parse_2op_sized!(Adc, QWORD),
                "adcl" => parse_2op_sized!(Adc, DWORD),
                "adcw" => parse_2op_sized!(Adc, WORD),
                "adcb" => parse_2op_sized!(Adc, BYTE),
                "sbbq" => parse_2op_sized!(Sbb, QWORD),
                "sbbl" => parse_2op_sized!(Sbb, DWORD),
                "sbbw" => parse_2op_sized!(Sbb, WORD),
                "sbbb" => parse_2op_sized!(Sbb, BYTE),
                "andq" => parse_2op_sized!(And, QWORD),
                "andl" => parse_2op_sized!(And, DWORD),
                "andw" => parse_2op_sized!(And, WORD),
                "andb" => parse_2op_sized!(And, BYTE),
                "subq" => parse_2op_sized!(Sub, QWORD),
                "subl" => parse_2op_sized!(Sub, DWORD),
                "subw" => parse_2op_sized!(Sub, WORD),
                "subb" => parse_2op_sized!(Sub, BYTE),
                "xorq" => parse_2op_sized!(Xor, QWORD),
                "xorl" => parse_2op_sized!(Xor, DWORD),
                "xorw" => parse_2op_sized!(Xor, WORD),
                "xorb" => parse_2op_sized!(Xor, BYTE),
                "cmpq" => parse_2op_sized!(Cmp, QWORD),
                "cmpl" => parse_2op_sized!(Cmp, DWORD),
                "cmpw" => parse_2op_sized!(Cmp, WORD),
                "cmpb" => parse_2op_sized!(Cmp, BYTE),
                "xchgq" => parse_2op_sized!(Xchg, QWORD),
                "xchgl" => parse_2op_sized!(Xchg, DWORD),
                "xchgw" => parse_2op_sized!(Xchg, WORD),
                "xchgb" => parse_2op_sized!(Xchg, BYTE),

                "negq" => parse_1op!(Negq),
                "notq" => parse_1op!(Notq),
                "imul" => parse_2op!(Imul),
                "idiv" => parse_1op!(Idiv),
                "idivq" => parse_1op!(Idiv),
                "idivl" => parse_1op!(Idivl),
                "div" => parse_1op!(Div),
                "divq" => parse_1op!(Div),
                "divl" => parse_1op!(Divl),

                "testq" => parse_2op!(Testq),
                "testb" => parse_2op!(Testb),
                "lea" => parse_2op!(Lea),

                "cqo" => parse_0op!(Cqo),
                "cdq" => parse_0op!(Cdq),
                "int3" => parse_0op!(Int3),

                "shlq" => parse_2op_sized!(Shl, QWORD),
                "shrq" => parse_2op_sized!(Shr, QWORD),
                "salq" => parse_2op_sized!(Sal, QWORD),
                "sarq" => parse_2op_sized!(Sar, QWORD),
                "rolq" => parse_2op_sized!(Rol, QWORD),
                "rorq" => parse_2op_sized!(Ror, QWORD),

                "shll" => parse_2op_sized!(Sal, DWORD),
                "shrl" => parse_2op_sized!(Shr, DWORD),
                "sall" => parse_2op_sized!(Sal, DWORD),
                "sarl" => parse_2op_sized!(Sar, DWORD),
                "roll" => parse_2op_sized!(Rol, DWORD),
                "rorl" => parse_2op_sized!(Ror, DWORD),

                "movsd" => parse_2op!(Movsd),
                "addsd" => parse_2op!(Addsd),
                "subsd" => parse_2op!(Subsd),
                "mulsd" => parse_2op!(Mulsd),
                "divsd" => parse_2op!(Divsd),
                "minsd" => parse_2op!(Minsd),
                "maxsd" => parse_2op!(Maxsd),
                "xorps" => parse_2op!(Xorps),
                "ucomisd" => parse_2op!(UComIsd),

                "andpd" => parse_2op!(Andpd),
                "xorpd" => parse_2op!(Xorpd),
                "roundpd" => parse_3op!(Roundpd),
                "cvtsi2sdq" => parse_2op!(Cvtsi2sdq),
                "cvttsd2siq" => parse_2op!(Cvttsd2siq),
                "sqrtpd" => parse_2op!(Sqrtpd),
                "sqrtsd" => parse_2op!(Sqrtsd),

                "pushq" => parse_1op!(Pushq),
                "popq" => parse_1op!(Popq),
                "call" => parse_1op!(Call),
                "ret" => parse_0op!(Ret),
                "jmp" => parse_1op!(Jmp),

                "jo" => parse_jcc!(O),
                "jno" => parse_jcc!(No),
                "jb" => parse_jcc!(B),
                "jc" => parse_jcc!(B),
                "jnae" => parse_jcc!(B),
                "jae" => parse_jcc!(Ae),
                "jnc" => parse_jcc!(Ae),
                "jeq" => parse_jcc!(Eq),
                "je" => parse_jcc!(Eq),
                "jz" => parse_jcc!(Eq),
                "jne" => parse_jcc!(Ne),
                "jnz" => parse_jcc!(Ne),
                "jbe" => parse_jcc!(Be),
                "jna" => parse_jcc!(Be),
                "ja" => parse_jcc!(A),
                "jnbe" => parse_jcc!(A),
                "js" => parse_jcc!(S),
                "jns" => parse_jcc!(Ns),
                "jp" => parse_jcc!(P),
                "jpe" => parse_jcc!(P),
                "jnp" => parse_jcc!(Np),
                "jpo" => parse_jcc!(Np),
                "jlt" => parse_jcc!(Lt),
                "jnge" => parse_jcc!(Lt),
                "jge" => parse_jcc!(Ge),
                "jnl" => parse_jcc!(Ge),
                "jle" => parse_jcc!(Le),
                "jng" => parse_jcc!(Le),
                "jgt" => parse_jcc!(Gt),
                "jnle" => parse_jcc!(Gt),

                "syscall" => parse_0op!(Syscall),
                "leave" => parse_0op!(Leave),

                "lzcntq" => parse_2op_sized!(Lzcnt, QWORD),
                "tzcntq" => parse_2op_sized!(Tzcnt, QWORD),
                "popcntq" => parse_2op_sized!(Popcnt, QWORD),
                "lzcntl" => parse_2op_sized!(Lzcnt, DWORD),
                "tzcntl" => parse_2op_sized!(Tzcnt, DWORD),
                "popcntl" => parse_2op_sized!(Popcnt, DWORD),

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

                s if s.starts_with("cmov") => {
                    let width = match s.as_bytes()[s.len() - 1] {
                        b'q' => OperandSize::QWORD,
                        b'l' => OperandSize::DWORD,
                        b'w' => OperandSize::WORD,
                        _ => {
                            return Err(Error::new(
                                ident.span(),
                                "illegal cmov operand size suffix.",
                            ))
                        }
                    };
                    if 5 >= s.len() {
                        return Err(Error::new(ident.span(), "illegal cmov condition."));
                    }
                    let cond = match Cond::from(&s[4..s.len() - 1]) {
                        Some(cond) => cond,
                        None => return Err(Error::new(ident.span(), "illegal cmov condition.")),
                    };
                    parse_cmov!(width, cond)
                }

                s if s.starts_with("set") => {
                    if 3 >= s.len() {
                        return Err(Error::new(ident.span(), "illegal setcc condition."));
                    }
                    let cond = match Cond::from(&s[3..]) {
                        Some(cond) => cond,
                        None => return Err(Error::new(ident.span(), "illegal setcc condition.")),
                    };
                    parse_set!(cond)
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
            let ident = input.parse::<Ident>()?;
            // e.g. "rax"
            let reg = Register::parse_register(input, ident.to_string())?;
            Ok(RmiOperand::reg(reg))
        } else if input.peek(LitInt) && is_single(input) {
            // e.g. "42"
            let imm = input.parse::<LitInt>()?;
            Ok(RmiOperand::Imm(imm.to_token_stream()))
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
            RmiOperand::Imm(..) => unreachable!("immediate"),
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
///  Immediate Operands.
///
///----------------------------------------------------------------------
#[derive(Clone, Debug)]
pub struct ImmOperand(pub TokenStream);

impl Parse for ImmOperand {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        if input.peek(LitInt) && is_single(input) {
            // e.g. "42"
            let imm = input.parse::<LitInt>()?;
            Ok(ImmOperand::new(imm))
        } else if input.peek(token::Paren) {
            // e.g. "(42)"
            let gr = input.parse::<Group>()?;
            Ok(ImmOperand(gr.stream()))
        } else {
            Err(input.error("Expected integer literal or Rust expression with parenthesis."))
        }
    }
}

impl std::fmt::Display for ImmOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ImmOperand(i) => write!(f, "{}", i),
        }
    }
}

impl ImmOperand {
    pub fn new(imm: impl ToTokens) -> Self {
        Self(quote! { #imm })
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
    } else if let Ok(no) = ident[3..].parse::<u8>() {
        if no > 15 {
            Err(input.error(format!("Invalid xmm register name. {}", ident)))
        } else {
            Ok(quote!(#no as u64))
        }
    } else {
        Err(input.error(format!("Invalid xmm register name. {}", ident)))
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
    Imm(TokenStream, Option<OperandSize>),
    Reg(Register),
    Xmm(TokenStream),
    Ind(IndAddr),
}

impl Parse for MovOperand {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        if input.peek(Ident) {
            let ident = input.parse::<Ident>()?.to_string();
            if ident == "qword" {
                let imm = input.parse::<Immediate>()?;
                Ok(Self::Imm(imm.0, Some(OperandSize::QWORD)))
            } else if ident.starts_with("xmm") {
                Ok(Self::Xmm(parse_xmm(input, &ident)?))
            } else {
                Ok(Self::reg(Register::parse_register(input, ident)?))
            }
        } else if input.peek(LitInt) && is_single(input) {
            // e.g. "42"
            let imm = input.parse::<LitInt>()?;
            Ok(Self::Imm(imm.to_token_stream(), None))
        } else if input.peek(token::Bracket) {
            // e.g. "[rax + 4]", "[rax - (4)]"
            Ok(Self::Ind(input.parse::<IndAddr>()?))
        } else if input.peek(token::Paren) {
            // e.g. "(42)"
            let gr = input.parse::<Group>()?;
            Ok(Self::Imm(gr.stream(), None))
        } else {
            Err(input.error("Expected register name, integer literal, memory reference, or Rust expression with parenthesis."))
        }
    }
}

impl std::fmt::Display for MovOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Imm(i, size) => match size {
                Some(size) => write!(f, "{}:{:?}", i, size),
                None => write!(f, "{}", i),
            },
            Self::Reg(reg) => write!(f, "{}", reg),
            Self::Xmm(reg) => write!(f, "xmm({})", reg),
            Self::Ind(ind) => write!(f, "{}", ind),
        }
    }
}

impl ToTokens for MovOperand {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ts = match self {
            Self::Imm(..) => unreachable!("immediate"),
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
}
