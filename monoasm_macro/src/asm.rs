use super::inst::*;
use super::parse::Disp;
use monoasm_inst::{util, Mode, Reg};
use proc_macro2::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::{Error, Ident, Token};

//----------------------------------------------------------------------
//
//  Assembly instructions.
//
//----------------------------------------------------------------------

#[derive(Clone, Debug)]
pub enum Inst {
    Label(Ident),

    Movq(Operand, Operand),
    Addq(Operand, Operand),
    Orq(Operand, Operand),
    Adcq(Operand, Operand),
    Sbbq(Operand, Operand),
    Andq(Operand, Operand),
    Subq(Operand, Operand),
    Xorq(Operand, Operand),
    Cmpq(Operand, Operand),

    Imull(Operand, Operand),

    Pushq(Operand),
    Popq(Operand),

    Jmp(Dest),
    Jne(Ident),
    Je(Ident),

    Call(Dest),
    Ret,
    Syscall,
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

pub fn compile(inst: Inst) -> TokenStream {
    match inst {
        Inst::Label(ident) => quote!( jit.bind_label(#ident); ),
        Inst::Movq(op1, op2) => movq(op1, op2),
        Inst::Addq(op1, op2) => binary_op("ADD", 0x81, 0x01, 0x03, 0, op1, op2),
        Inst::Orq(op1, op2) => binary_op("OR", 0x81, 0x09, 0x0b, 1, op1, op2),
        Inst::Adcq(op1, op2) => binary_op("ADC", 0x81, 0x11, 0x13, 2, op1, op2),
        Inst::Sbbq(op1, op2) => binary_op("SBB", 0x81, 0x19, 0x1b, 3, op1, op2),
        Inst::Andq(op1, op2) => binary_op("AND", 0x81, 0x21, 0x23, 4, op1, op2),
        Inst::Subq(op1, op2) => binary_op("SUB", 0x81, 0x29, 0x2b, 5, op1, op2),
        Inst::Xorq(op1, op2) => binary_op("XOR", 0x81, 0x31, 0x33, 6, op1, op2),
        Inst::Cmpq(op1, op2) => binary_op("CMP", 0x81, 0x39, 0x3b, 7, op1, op2),

        Inst::Imull(op1, op2) => {
            // IMUL r32, r/m32: r32 <- r32 * r/m32
            match (op1, op2) {
                // IMUL r32, r/m32
                // 0F AF /r
                // RM
                (Operand::Reg(r1), op2) => {
                    let (mode, reg, disp) = op_to_rm(op2);
                    let modrm = modrm(r1, mode, reg);
                    let disp = imm_to_ts(disp);
                    quote! {
                        jit.emitb(0x0f);
                        jit.emitb(0xaf);
                        #modrm
                        #disp
                    }
                }
                _ => unimplemented!(),
            }
        }

        Inst::Pushq(op) => push_pop(0x50, op),
        Inst::Popq(op) => push_pop(0x58, op),

        Inst::Call(dest) => match dest {
            Dest::Reg(r) => {
                // CALL r/m64
                // FF /2
                let rex = rex(Reg::none(), r, Reg::none());
                let modrm = modrm_digit(2, Mode::Reg, r);
                quote! {
                    #rex
                    jit.emitb(0xff);
                    #modrm
                }
            }
            Dest::Rel(dest) => {
                // CALL rel32
                // E8 cd
                quote! {
                    jit.emitb(0xe8);
                    jit.save_reloc(#dest, 4);
                    jit.emitl(0);
                }
            }
            dest => unimplemented!("CALL {:?}", dest),
        },
        Inst::Ret => quote!( jit.emitb(0xc3); ),
        Inst::Jmp(dest) => match dest {
            Dest::Reg(r) => {
                // JMP r/m64
                // FF /4
                // M
                let rex = rex(Reg::none(), r, Reg::none());
                let modrm = modrm_digit(4, Mode::Reg, r);
                quote! (
                    #rex
                    jit.emitb(0xff);
                    #modrm
                )
            }
            Dest::Rel(dest) => {
                // JMP rel32
                // E9 cd
                // D
                quote! {
                    jit.emitb(0xe9);
                    jit.save_reloc(#dest, 4);
                    jit.emitl(0);
                }
            }
            dest => unimplemented!("JMP {:?}", dest),
        },
        Inst::Jne(dest) => quote!(
            // JNE rel32
            // 0F 85 cd
            // TODO: support rel8
            jit.emitb(0x0f);
            jit.emitb(0x85);
            jit.save_reloc(#dest, 4);
            jit.emitl(0);
        ),
        Inst::Je(dest) => quote!(
            // JE rel32
            // 0F 84 cd
            // TODO: support rel8
            jit.emitb(0x0f);
            jit.emitb(0x84);
            jit.save_reloc(#dest, 4);
            jit.emitl(0);
        ),
        Inst::Syscall => quote!(
            jit.emitb(0x0f);
            jit.emitb(0x05);
        ),
    }
}

fn op_to_rm(op: Operand) -> (Mode, Reg, Disp) {
    match op {
        Operand::Reg(r) => (Mode::Reg, r, Disp::None),
        Operand::Ind(r, disp) => match disp {
            Disp::None => (Mode::Ind, r, Disp::None),
            Disp::D8(i) => (Mode::InD8, r, Disp::D8(i)),
            Disp::D32(i) => (Mode::InD32, r, Disp::D32(i)),
            Disp::Expr(expr) => (Mode::InD32, r, Disp::Expr(expr)),
        },
        _ => unreachable!(),
    }
}

fn imm_to_ts(disp: Disp) -> TokenStream {
    match disp {
        Disp::None => quote!(),
        Disp::D8(i) => quote!( jit.emitb(#i as u8); ),
        Disp::D32(i) => quote!( jit.emitl(#i as u32); ),
        Disp::Expr(expr) => quote!( jit.emitl((#expr) as u32); ),
    }
}

/// Encoding: Opcode + rd  
/// REX.W Op+ rd
fn enc_o(op: u8, reg: Reg) -> TokenStream {
    let rexw = rexw(Reg::none(), reg, Reg::none());
    let op_with_rd = op_with_rd(op, reg);
    quote!(
        #rexw
        #op_with_rd
    )
}

/// Encoding: MI  
/// ModRM:r/m
fn enc_mi(op: u8, rm_op: Operand) -> TokenStream {
    enc_mr(op, Reg::none(), rm_op)
}

/// Encoding: MR or RM
/// MR-> ModRM:r/m(w) ModRM:reg(r)
/// RM-> ModRM:reg(r) ModRM:r/m(w)
fn enc_mr(op: u8, reg: Reg, rm_op: Operand) -> TokenStream {
    let (mode, rm, disp) = op_to_rm(rm_op);
    let enc_mr = enc_mr_main(op, reg, mode, rm);
    // TODO: If mode == Ind and r/m == 5, becomes [rip + disp32].
    let disp = imm_to_ts(disp);
    quote! {
        #enc_mr
        #disp
    }
}

fn enc_expr_mr(op: u8, expr: TokenStream, rm_op: Operand) -> TokenStream {
    quote! {
        let r1 = Reg::from((#expr) as u64);
        jit.enc_mr(#op, r1, #rm_op);
    }
}

fn enc_mr_main(op: u8, reg: Reg, mode: Mode, rm: Reg) -> TokenStream {
    if mode != Mode::Reg && (rm == Reg::Rsp || rm == Reg::R12) {
        // TODO: If mode != Reg and r/m == 4 (rsp/r12), use SIB.
        // Currently, only Mode::Ind is supported.
        assert!(mode == Mode::Ind);
        // set index to 4 when [rm] is to be used.
        let index = Reg::Rsp; // magic number
        let scale = 0;
        let rex = rexw(reg, rm, index);
        let modrm = modrm(reg, mode, rm);
        let sib = sib(scale, index, rm as u8);
        quote! {
            #rex
            jit.emitb(#op);
            #modrm
            #sib
        }
    } else {
        let rex = rexw(reg, rm, Reg::none());
        let modrm = modrm(reg, mode, rm);
        quote! {
            #rex
            jit.emitb(#op);
            #modrm
        }
    }
}

fn op_with_rd(op: u8, r: Reg) -> TokenStream {
    let val = util::op_with_rd(op, r);
    quote!( jit.emitb(#val); )
}

fn modrm_digit(digit: u8, mode: Mode, rm: Reg) -> TokenStream {
    let modrm = util::modrm_digit(digit, mode, rm);
    quote!( jit.emitb(#modrm); )
}

fn modrm(reg: Reg, mode: Mode, rm: Reg) -> TokenStream {
    let modrm = util::modrm(reg, mode, rm);
    quote!( jit.emitb(#modrm); )
}

fn rexw(reg: Reg, base: Reg, index: Reg) -> TokenStream {
    let rex_prefix = util::rexw(reg, base, index);
    quote!( jit.emitb(#rex_prefix); )
}

fn rex(reg: Reg, base: Reg, index: Reg) -> TokenStream {
    match util::rex(reg, base, index) {
        Some(rex) => quote!(jit.emitb(#rex);),
        None => quote!(),
    }
}

fn sib(scale: u8, index: Reg, base: u8) -> TokenStream {
    let sib = util::sib(scale, index, base);
    quote!( jit.emitb(#sib); )
}

fn movq(op1: Operand, op2: Operand) -> TokenStream {
    match (op1, op2) {
        (Operand::Imm(_), op2) => panic!("'MOV Imm, {}' doen not exists.", op2),
        // MOV r/m64, imm32
        // REX.W + C7 /0 id
        // MI
        //
        // MOV r64, imm64
        // REX.W + B8+ rd io
        // OI
        (Operand::Reg(r), Operand::Imm(i)) => {
            let op_0 = enc_mi(0xc7, Operand::Reg(r)); // MOV r/m64, imm32
            let op_1 = enc_o(0xb8, r); // MOV r64, imm64
            quote! {
                let imm = (#i) as u64;
                if  imm <= 0xffff_ffff {
                    #op_0
                    jit.emitl(imm as u32);
                } else {
                    #op_1
                    jit.emitq(imm);
                }
            }
        }
        (Operand::RegExpr(expr), Operand::Imm(i)) => {
            quote!(
                let imm = (#i) as u64;
                let r = Reg::from(#expr as u64);
                let rm_op = Or::Reg(r);
                if  imm <= 0xffff_ffff {
                    // MOV r/m64, imm32
                    jit.enc_mi(0xc7, rm_op);
                    jit.emitl(imm as u32);
                } else {
                    // MOV r64, imm64
                    jit.enc_o(0xb8, r);
                    jit.emitq(imm);
                };
            )
        }
        // MOV r/m64, imm32
        // REX.W + C7 /0 id
        // MI
        (op1, Operand::Imm(i)) => {
            let op1_str = format!("{:?}", op1);
            let op_1 = enc_mi(0xc7, op1);
            quote! {
                let imm = (#i) as u64;
                if  imm <= 0xffff_ffff {
                    #op_1
                    jit.emitl(imm as u32);
                } else {
                    panic!("'MOV {}, imm64' does not exists.", #op1_str);
                }
            }
        }
        // MOV r/m64,r64
        // REX.W + 89 /r
        // MR
        (op1, Operand::RegExpr(expr)) => enc_expr_mr(0x89, expr, op1),
        // MOV r64,m64
        // REX.W + 8B /r
        // RM
        (Operand::RegExpr(expr), op2) => enc_expr_mr(0x8b, expr, op2),
        // MOV r/m64,r64
        // REX.W + 89 /r
        // MR
        (op1, Operand::Reg(r2)) => enc_mr(0x89, r2, op1),
        // MOV r64,m64
        // REX.W + 8B /r
        // RM
        (Operand::Reg(r1), op2) => enc_mr(0x8b, r1, op2),
        (op1, op2) => unimplemented!("MOV {}, {}", op1, op2),
    }
}

fn binary_op(
    op_name: &str,
    op_imm: u8,
    op_mr: u8,
    op_rm: u8,
    digit: u8,
    op1: Operand,
    op2: Operand,
) -> TokenStream {
    match (op1, op2) {
        (Operand::Imm(_), op2) => panic!("'{} imm, {}' does not exists.", op_name, op2),
        // OP r/m64, imm32
        // OP=ADD REX.W 81 /0 id
        // OP=OR  REX.W 81 /1 id
        // OP=ADC REX.W 81 /2 id
        // OP=SBB REX.W 81 /3 id
        // OP=AND REX.W 81 /4 id
        // OP=SUB REX.W 81 /5 id
        // MI
        (op1, Operand::Imm(i)) => {
            let op1_str = format!("{}", op1);
            let (mode, reg, disp) = op_to_rm(op1);
            let rex = rexw(Reg::none(), reg, Reg::none());
            let modrm = modrm_digit(digit, mode, reg);
            let disp = imm_to_ts(disp);
            quote! {
                let imm = (#i) as u64;
                if imm > 0xffff_ffff {
                    panic!("'{} {}, imm64' does not exists.", #op_name, #op1_str);
                }
                #rex
                jit.emitb(#op_imm);
                #modrm
                #disp
                jit.emitl(imm as u32);
            }
        }
        // OP r/m64, r64
        // REX.W op_mr /r
        // MR
        (op1, Operand::RegExpr(expr)) => {
            quote! {
                let r2 = Reg::from((#expr) as u64);
                jit.enc_mr(#op_mr, r2, #op1);
            }
        }
        (op1, Operand::Reg(r2)) => enc_mr(op_mr, r2, op1),
        // OP r64, m64
        // REX.W op_rm /r
        // RM
        (Operand::RegExpr(expr), op2) => {
            quote! {
                let r1 = Reg::from((#expr) as u64);
                jit.enc_mr(#op_rm, r1, #op2);
            }
        }
        (Operand::Reg(r1), op2) => enc_mr(op_rm, r1, op2),
        (op1, op2) => unimplemented!("{} {}, {}", op_name, op1, op2),
    }
}

fn push_pop(opcode: u8, op: Operand) -> TokenStream {
    match op {
        // PUSH r64     POP 64
        // 50 +rd       58 +rd
        // O            O
        Operand::Reg(r) => {
            let rex = rex(Reg::none(), r, Reg::none());
            let op = op_with_rd(opcode, r);
            quote! (
                #rex
                #op
            )
        }
        op => unimplemented!("PUSH/POP {:?}", op),
    }
}
