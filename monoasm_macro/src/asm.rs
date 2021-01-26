use super::inst::*;
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

    Imul(Operand, Operand),

    Pushq(Operand),
    Popq(Operand),

    Jmp(Dest),
    Jne(Ident),
    Jeq(Ident),

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
                "imul" => parse_2op!(Imul),

                "pushq" => parse_1op!(Pushq),
                "popq" => parse_1op!(Popq),
                "cmpq" => parse_2op!(Cmpq),
                "call" => parse_1op!(Call),
                "ret" => parse_0op!(Ret),
                "jmp" => parse_1op!(Jmp),
                "jne" => parse_1op!(Jne),
                "jeq" => parse_1op!(Jeq),
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
        Inst::Xorq(op1, op2) => xor(op1, op2),
        Inst::Cmpq(op1, op2) => binary_op("CMP", 0x81, 0x39, 0x3b, 7, op1, op2),

        Inst::Imul(op1, op2) => {
            // IMUL r64, r/m64: r64 <- r64 * r/m64
            match (op1, op2) {
                // IMUL r64, r/m64
                // REX.W 0F AF /r
                // RM
                (Operand::Reg(expr), op2) => {
                    quote! {
                        jit.enc_mr_main(&[0x0f,0xaf], JitMemory::rexw, #expr, #op2);
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
                quote! {
                    jit.rex(Reg::none(), #r, Reg::none());
                    jit.emitb(0xff);
                    jit.modrm_digit(2, Mode::Reg, #r);
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
                quote! (
                    jit.rex(Reg::none(), #r, Reg::none());
                    jit.emitb(0xff);
                    jit.modrm_digit(4, Mode::Reg, #r);
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
        Inst::Jeq(dest) => quote!(
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
        (Operand::Reg(expr), Operand::Imm(i)) => {
            let xor = xor(Operand::Reg(expr.clone()), Operand::Reg(expr.clone()));
            quote!(
                let imm = (#i) as u64;
                let rm_op = Or::Reg(#expr);
                if imm == 0 {
                    #xor
                } else if imm <= 0xffff_ffff {
                    // MOV r/m64, imm32
                    jit.enc_rexw_mi(0xc7, rm_op);
                    jit.emitl(imm as u32);
                } else {
                    // MOV r64, imm64
                    jit.enc_rexw_o(0xb8, #expr);
                    jit.emitq(imm);
                };
            )
        }
        // MOV r/m64, imm32
        // REX.W + C7 /0 id
        // MI
        (op1, Operand::Imm(i)) => {
            let op1_str = format!("{:?}", op1);
            quote! {
                let imm = (#i) as u64;
                if imm <= 0xffff_ffff {
                    jit.enc_rexw_mi(0xc7, #op1);
                    jit.emitl(imm as u32);
                } else {
                    panic!("'MOV {}, imm64' does not exists.", #op1_str);
                }
            }
        }
        // MOV r/m64,r64
        // REX.W + 89 /r
        // MR
        (op1, Operand::Reg(expr)) => quote!( jit.enc_rexw_mr(0x89, #expr, #op1); ),
        // MOV r64,m64
        // REX.W + 8B /r
        // RM
        (Operand::Reg(expr), op2) => quote!( jit.enc_rexw_mr(0x8b, #expr, #op2); ),
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
        (Operand::Reg(expr), Operand::Imm(i)) => {
            quote! {
                let imm = (#i) as u64;
                if imm > 0xffff_ffff {
                    panic!("{} {:?}, imm64' does not exists.", #op_name, #expr);
                }
                let rm_op = Or::Reg(#expr);
                let (mode, reg, disp) = rm_op.op_to_rm();
                jit.rexw(Reg::none(), reg, Reg::none());
                jit.emitb(#op_imm);
                jit.modrm_digit(#digit, mode, reg);
                jit.emit_disp(disp);
                jit.emitl(imm as u32);
            }
        }
        (op1, Operand::Imm(i)) => {
            let op1_str = format!("{}", op1);
            quote! {
                let (mode, reg, disp) = (#op1).op_to_rm();
                let imm = (#i) as u64;
                if imm > 0xffff_ffff {
                    panic!("'{} {}, imm64' does not exists.", #op_name, #op1_str);
                }
                jit.rexw(Reg::none(), reg, Reg::none());
                jit.emitb(#op_imm);
                jit.modrm_digit(#digit, mode, reg);
                jit.emit_disp(disp);
                jit.emitl(imm as u32);
            }
        }
        // OP r/m64, r64
        // REX.W op_mr /r
        // MR
        (op1, Operand::Reg(expr)) => quote! ( jit.enc_rexw_mr(#op_mr, #expr, #op1); ),
        // OP r64, m64
        // REX.W op_rm /r
        // RM
        (Operand::Reg(expr), op2) => quote! ( jit.enc_rexw_mr(#op_rm, #expr, #op2); ),
        (op1, op2) => unimplemented!("{} {}, {}", op_name, op1, op2),
    }
}

fn xor(op1: Operand, op2: Operand) -> TokenStream {
    binary_op("XOR", 0x81, 0x31, 0x33, 6, op1, op2)
}

fn push_pop(opcode: u8, op: Operand) -> TokenStream {
    match op {
        // PUSH r64     POP 64
        // 50 +rd       58 +rd
        // O            O
        Operand::Reg(reg) => {
            quote! (
                jit.rex(Reg::none(), #reg, Reg::none());
                jit.op_with_rd(#opcode, #reg);
            )
        }
        op => unimplemented!("PUSH/POP {:?}", op),
    }
}
