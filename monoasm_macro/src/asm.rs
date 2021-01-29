use super::inst::*;
use proc_macro2::TokenStream;
use quote::quote;

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
                (Operand::Reg(expr), op2) => quote! (
                    jit.enc_mr_main(&[0x0f,0xaf], true, #expr, #op2);
                ),

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
                    jit.enc_digit(&[0xff], #r, 2);
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
                    jit.enc_digit(&[0xff],#r, 4);
                )
            }
            // JMP rel32
            // E9 cd
            // D
            Dest::Rel(dest) => quote! ( jit.enc_d(&[0xe9], #dest); ),
            dest => unimplemented!("JMP {:?}", dest),
        },
        Inst::Jcc(cond, dest) => match cond {
            // JNE rel32
            // 0F 85 cd
            // TODO: support rel8
            Cond::Ne => quote!( jit.enc_d(&[0x0f, 0x85], #dest); ),
            // JE rel32
            // 0F 84 cd
            // TODO: support rel8
            Cond::Eq => quote!( jit.enc_d(&[0x0f, 0x84], #dest); ),
        },

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
                let rm_op = Or::reg(#expr);
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
                let rm_op = Or::reg(#expr);
                jit.enc_rexw_digit(&[#op_imm], rm_op, #digit);
                jit.emitl(imm as u32);
            }
        }
        (op1, Operand::Imm(i)) => {
            let op1_str = format!("{}", op1);
            quote! {
                let imm = (#i) as u64;
                if imm > 0xffff_ffff {
                    panic!("'{} {}, imm64' does not exists.", #op_name, #op1_str);
                }
                jit.enc_rexw_digit(&[#op_imm], #op1, #digit);
                jit.emitl(imm as u32);
            }
        }
        // OP r/m64, r64
        // REX.W op_mr /r
        // MR
        (op1, Operand::Reg(expr)) => quote! (
            jit.enc_rexw_mr(#op_mr, #expr, #op1);
        ),
        // OP r64, m64
        // REX.W op_rm /r
        // RM
        (Operand::Reg(expr), op2) => quote! (
            jit.enc_rexw_mr(#op_rm, #expr, #op2);
        ),
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
        Operand::Reg(reg) => quote! ( jit.enc_o(#opcode, #reg); ),
        op => unimplemented!("PUSH/POP {:?}", op),
    }
}
