#![feature(proc_macro_hygiene)]
#![allow(unused_imports)]
#![allow(dead_code)]
#![allow(unreachable_patterns)]

extern crate proc_macro;
extern crate quote;
extern crate syn;
use proc_macro2::{TokenStream, TokenTree, Delimiter};
use quote::quote;
use quote::TokenStreamExt;
use syn::parse::{Parse, ParseStream};
use syn::{parse_macro_input, Error, Ident, LitInt, Token};

#[derive(Clone, Debug)]
struct Stmts {
    contents: Vec<Inst>,
}

#[derive(Clone, Debug)]
enum Inst {
    Movq(Operand, Operand),
    Addq(Operand, Operand),
    Orq(Operand, Operand),
    Adcq(Operand, Operand),
    Sbbq(Operand, Operand),
    Andq(Operand, Operand),
    Subq(Operand, Operand),
    Xorq(Operand, Operand),
    Cmpq(Operand, Operand),

    Jne(Ident),

    Call(Dest),
    Ret,
    Syscall,
}

#[derive(Clone, Debug)]
enum Operand {
    Imm(u64),
    Expr(TokenStream),
    Reg(Reg),
    Ind(Reg),
    IndDisp(Reg, i32),
}

impl std::fmt::Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::Imm(i) => write!(f, "Imm({})", i),
            Operand::Expr(ts) => write!(f, "Expr({})", ts),
            Operand::Reg(r) => write!(f, "Reg({:?})", r),
            Operand::Ind(r) => write!(f, "Ind({:?})", r),
            Operand::IndDisp(r, i) => write!(f, "IndDisp({:?}, {})", r, i),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
enum Dest {
    Reg(Reg),
    Rel(usize),
}

#[derive(Copy, Clone, PartialEq, Debug)]
enum Reg {
    Rax = 0,
    Rcx = 1,
    Rdx = 2,
    Rbx = 3,
    Rsp = 4,
    Rbp = 5,
    Rsi = 6,
    Rdi = 7,
    R8 = 8,
    R9 = 9,
    R10 = 10,
    R11 = 11,
    R12 = 12,
    R13 = 13,
    R14 = 14,
    R15 = 15,
}

impl Reg {
    fn from_str(string: &str) -> Reg {
            match string {
                "rax" => Reg::Rax,
                "rcx" => Reg::Rcx,
                "rdx" => Reg::Rdx,
                "rbx" => Reg::Rbx,
                "rsp" => Reg::Rsp,
                "rbp" => Reg::Rbp,
                "rsi" => Reg::Rsi,
                "rdi" => Reg::Rdi,
                "r8" => Reg::R8,
                "r9" => Reg::R9,
                "r10" => Reg::R10,
                "r11" => Reg::R11,
                "r12" => Reg::R12,
                "r13" => Reg::R13,
                "r14" => Reg::R14,
                "r15" => Reg::R15,
                _ => panic!("Illegal register name."),
            }
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
enum Mode {
    Ind = 0,   // (rax)
    InD8 = 1,  // (rax + disp8)
    InD32 = 2, // (rax + disp32)
    Reg = 3,   // rax
}

impl Parse for Operand {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Ident) && is_single(input) {
            let op: Ident = input.parse()?;
            let reg = Reg::from_str(op.to_string().as_str());
            Ok(Operand::Reg(reg))
        } else if lookahead.peek(LitInt) && is_single(input) {
            let imm = input.parse::<LitInt>()?;
            Ok(Operand::Imm(imm.base10_parse()?))
        } else {
            let tok = input.parse::<TokenTree>()?;
            match tok {
                TokenTree::Group(gr) => {
                    match gr.delimiter() {
                        Delimiter::Parenthesis => Ok(Operand::Expr(gr.stream())),
                        Delimiter::Bracket => {
                            let stream = gr.stream();
                            let reg: Ident = syn::parse2(stream)?;
                            let reg = Reg::from_str(reg.to_string().as_str());
                            Ok(Operand::Ind(reg))
                        }
                        _ => unimplemented!("Unimplemented delimiter."),
                    }
                }   
                _ => unreachable!(),
            }
        }
    }
}

impl Parse for Dest {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Ident) && is_single(input) {
            let op: Ident = input.parse()?;
            let reg = Reg::from_str(op.to_string().as_str());
            Ok(Dest::Reg(reg))
        } else {
            unimplemented!();
        }
    }
}

fn is_single(input: ParseStream) -> bool {
    input.peek2(Token![,]) || input.peek2(Token![;])
}

macro_rules! parse_2op {
    ($input:ident, $inst: ident) => (
        {
            let op1 = $input.parse()?;
            $input.parse::<Token![,]>()?;
            let op2 = $input.parse()?;
            $input.parse::<Token![;]>()?;
            Ok(Inst::$inst(op1, op2))
        }
    )
}

macro_rules! parse_1op {
    ($input:ident, $inst: ident) => (
        {
            let op = $input.parse()?;
            $input.parse::<Token![;]>()?;
            Ok(Inst::$inst(op))
        }
    )
}

macro_rules! parse_0op {
    ($input:ident, $inst: ident) => (
        {
            $input.parse::<Token![;]>()?;
            Ok(Inst::$inst)
        }
    )
}

impl Parse for Inst {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let inst: Ident = input.parse()?;
        match inst.to_string().as_str() {
            "movq" => parse_2op!(input, Movq),

            "addq" => parse_2op!(input, Addq),
            "orq" => parse_2op!(input, Orq),
            "adcq" => parse_2op!(input, Adcq),
            "sbbq" => parse_2op!(input, Sbbq),
            "andq" => parse_2op!(input, Andq),
            "subq" => parse_2op!(input, Subq),
            "xorq" => parse_2op!(input, Xorq),
            "cmpq" => parse_2op!(input, Cmpq),
            "call" => parse_1op!(input, Call),
            "ret" => parse_0op!(input, Ret),
            "jne" =>  parse_1op!(input, Jne),
            "syscall" => parse_0op!(input, Syscall),
            _ => Err(Error::new(inst.span(), "unimplemented instruction.")),
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
            println!("{:?}", &inst);
            stmts.contents.push(inst);
        }
        Ok(stmts)
    }
}

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

fn compile(inst: Inst) -> TokenStream {
    match inst {
        Inst::Movq(op1, op2) => {
            match (op1, op2) {
                (Operand::Imm(_), _) => panic!("Invalid op: moveq Imm, _"),
                // MOV r/m64, imm32
                (Operand::Reg(r), Operand::Imm(i)) if i <= 0xffff_ffff => {
                    let mut ts = TokenStream::new();
                    ts.extend(enc_m(0xc7, Mode::Reg, r));
                    ts.extend(quote!( jit.emitl(#i as u32); ));
                    ts
                }
                (Operand::Ind(r), Operand::Imm(i)) if i <= 0xffff_ffff => {
                    let mut ts = TokenStream::new();
                    ts.extend(enc_m(0xc7, Mode::Ind, r));
                    ts.extend(quote!( jit.emitl(#i as u32); ));
                    ts
                }
                // MOV r64, imm64
                (Operand::Reg(r), Operand::Imm(i)) => {
                    let mut ts = TokenStream::new();
                    ts.extend(enc_o(0xb8, r));
                    ts.extend(quote!( jit.emitq(#i); ));
                    ts
                }
                (Operand::Reg(r), Operand::Expr(expr)) => {
                    let mut ts = TokenStream::new();
                    ts.extend(enc_o(0xb8, r));
                    ts.extend(quote!( jit.emitq(#expr); ));
                    ts
                }
                // MOV r/m64,r64
                (Operand::Reg(r1), Operand::Reg(r2)) => enc_mr(0x89, Mode::Reg, r2, r1, 0),
                (Operand::Ind(r1), Operand::Reg(r2)) => enc_mr(0x89, Mode::Ind, r2, r1, 0),
                (Operand::IndDisp(r1, disp), Operand::Reg(r2)) => {
                    enc_mr(0x89, Mode::InD8, r2, r1, disp as i32)
                }
                (Operand::IndDisp(r1, disp), Operand::Reg(r2)) => {
                    enc_mr(0x89, Mode::InD32, r2, r1, disp)
                }
                // MOV r64,m64
                (Operand::Reg(r1), Operand::Ind(r2)) => enc_mr(0x8b, Mode::Ind, r1, r2, 0),
                (Operand::Reg(r1), Operand::IndDisp(r2, disp)) => {
                    enc_mr(0x8b, Mode::InD8, r1, r2, disp as i32)
                }
                (Operand::Reg(r1), Operand::IndDisp(r2, disp)) => {
                    enc_mr(0x8b, Mode::InD32, r1, r2, disp)
                }
                _ => unimplemented!(),
            }
        }
        Inst::Addq(op1, op2) => binary_op(0x81, 0x01, 0, op1, op2),
        Inst::Orq(op1, op2) => binary_op(0x81, 0x09, 1, op1, op2),
        Inst::Adcq(op1, op2) => binary_op(0x81, 0x11, 2, op1, op2),
        Inst::Sbbq(op1, op2) => binary_op(0x81, 0x19, 3, op1, op2),
        Inst::Andq(op1, op2) => binary_op(0x81, 0x21, 4, op1, op2),
        Inst::Subq(op1, op2) => binary_op(0x81, 0x29, 5, op1, op2),
        Inst::Xorq(op1, op2) => binary_op(0x81, 0x31, 6, op1, op2),
        Inst::Cmpq(op1, op2) => binary_op(0x81, 0x39, 7, op1, op2),
        Inst::Call(dest) => {
            let mut ts = TokenStream::new();
            match dest {
                Dest::Reg(r) => {
                    if r as u8 > 7 {
                        panic!("Can not CALL R8-R15");
                    }
                    ts.extend(quote!( jit.emitb(0xff); ));
                    ts.extend(modrm_digit(Mode::Reg, 2, r));
                }
                Dest::Rel(dest) => {
                    ts.extend(quote!( jit.emitb(0xe8); ));
                    ts.extend(quote!( jit.reloc[dest.0].disp.push((4, jit.counter)) ));
                    ts.extend(quote!( jit.emitl(0); ));
                }
            }
            ts
        }
        Inst::Ret => quote!( jit.emitb(0xc3); ),
        Inst::Jne(dest) => quote!(
            jit.emitb(0x0f);
            jit.emitb(0x85);
            jit.reloc[#dest].disp.push((4, jit.counter));
            jit.emitl(0);
        ),
        Inst::Syscall => quote!(
            jit.emitb(0x0f);
            jit.emitb(0x05);
        ),
    }
}

/// Encoding: Opcode + rd
fn enc_o(op: u8, reg: Reg) -> TokenStream {
    let mut ts = TokenStream::new();
    ts.extend(rexw(Reg::Rax, reg));
    ts.extend(emitb_with_rd(op, reg));
    ts
}

/// Encoding: M
fn enc_m(op: u8, mode: Mode, reg_rm: Reg) -> TokenStream {
    enc_mr(op, mode, Reg::Rax, reg_rm, 0)
}

/// Encoding: MR or RM
fn enc_mr(op: u8, mode: Mode, reg: Reg, reg_rm: Reg, disp: i32) -> TokenStream {
    // TODO: If mode != Reg and r/m == 4(rsp/r12), use SIB.
    // TODO: If mode == Ind and r/m == 5, becomes [rip + disp32].
    let mut ts = TokenStream::new();
    ts.extend(rexw(reg, reg_rm));
    ts.extend(quote!( jit.emitb(#op); ));
    ts.extend(modrm(mode, reg, reg_rm));
    if mode == Mode::InD8 {
        ts.extend(quote!( jit.emitb(#disp as i8 as u8); ));
    } else if mode == Mode::InD32 {
        ts.extend(quote!( jit.emitl(#disp as u32); ));
    }
    ts
}

fn modrm_digit(mode: Mode, digit: u8, rm: Reg) -> TokenStream {
    let modrm = (mode as u8) << 6 | (digit & 0b111) << 3 | (rm as u8) & 0b111;
    quote!( jit.emitb(#modrm); )
}

fn modrm(mode: Mode, reg: Reg, rm: Reg) -> TokenStream {
    let modrm = (mode as u8) << 6 | ((reg as u8) & 0b111) << 3 | (rm as u8) & 0b111;
    quote!( jit.emitb(#modrm); )
}

fn rexw(rex_r: Reg, rex_b: Reg) -> TokenStream {
    let rex_prefix = 0x48 | ((rex_r as u8) & 0b0000_1000) >> 1 | ((rex_b as u8) & 0b0000_1000) >> 3;
    quote!( jit.emitb(#rex_prefix); )
}

fn emitb_with_rd(val: u8, r: Reg) -> TokenStream {
    let val = val | ((r as u8) & 0b0111);
    quote!( jit.emitb(#val); )
}

fn binary_op(
    opcode_imm: u8,
    opcode_rm_reg: u8,
    digit: u8,
    op1: Operand,
    op2: Operand,
) -> TokenStream {
    let mut ts = TokenStream::new();
    match (op1.clone(), op2.clone()) {
        // ADD r/m64, imm32
        (Operand::Reg(ref r), Operand::Imm(i))
        | (Operand::Ind(ref r), Operand::Imm(i))
        | (Operand::IndDisp(ref r, _), Operand::Imm(i)) => {
            if i > 0xffff_ffff {
                panic!("'XXX r/m64, imm64' does not exists.");
            }
            ts.extend(rexw(Reg::Rax, *r));
            ts.extend(quote!( jit.emitb(#opcode_imm); ));
            match op1 {
                Operand::Reg(r) => ts.extend(modrm_digit(Mode::Reg, digit, r)),
                Operand::Ind(r) => ts.extend(modrm_digit(Mode::Ind, digit, r)),
                Operand::IndDisp(r, disp) => {
                    ts.extend(modrm_digit(Mode::InD32, digit, r));
                    ts.extend(quote!( jit.emitl(#disp as u32); ));
                }
                _ => unreachable!(),
            }
            ts.extend(quote!( jit.emitl(#i as u32); ));
        }
        (Operand::Reg(r1), Operand::Reg(r2)) => {
            ts.extend(enc_mr(opcode_rm_reg, Mode::Reg, r2, r1, 0));
        }
        _ => unimplemented!(),
    }
    ts
}
