use super::inst::*;
use proc_macro2::TokenStream;
use quote::quote;

pub fn compile(inst: Inst) -> TokenStream {
    match inst {
        Inst::Label(ident) => quote!( jit.bind_label(#ident); ),
        Inst::F64(f) => {
            quote!( jit.emit(&#f.to_ne_bytes()); )
        }
        Inst::I64(i) => {
            quote!( jit.emitq(#i as u64); )
        }

        Inst::Movq(op1, op2) => movq(op1, op2),
        Inst::Addq(op1, op2) => binary_op("ADD", 0x81, 0x01, 0x03, 0, op1, op2),
        Inst::Orq(op1, op2) => binary_op("OR", 0x81, 0x09, 0x0b, 1, op1, op2),
        Inst::Adcq(op1, op2) => binary_op("ADC", 0x81, 0x11, 0x13, 2, op1, op2),
        Inst::Sbbq(op1, op2) => binary_op("SBB", 0x81, 0x19, 0x1b, 3, op1, op2),
        Inst::Andq(op1, op2) => binary_op("AND", 0x81, 0x21, 0x23, 4, op1, op2),
        Inst::Subq(op1, op2) => binary_op("SUB", 0x81, 0x29, 0x2b, 5, op1, op2),
        Inst::Xorq(op1, op2) => binary_op("XOR", 0x81, 0x31, 0x33, 6, op1, op2),
        Inst::Cmpq(op1, op2) => binary_op("CMP", 0x81, 0x39, 0x3b, 7, op1, op2),
        Inst::Cmpb(op1, op2) => {
            match (op1, op2) {
                // cmp r/m8, imm8
                // REX 80 /7 ib
                (op1, RmiOperand::Imm(i)) => {
                    quote! {
                        let imm = (#i) as i64;
                        if let Ok(imm) = i8::try_from(imm) {
                            jit.enc_rex_digit(&[0x80], #op1, 7, Imm::B(imm));
                        } else {
                            panic!("{} is out of 8bit.", #i);
                        }
                    }
                }
                // cmp r/m8, r8
                // REX 38 /r
                // MR
                (op1, RmiOperand::Reg(expr)) => quote! ( jit.enc_rex_mr(&[0x38], #expr, #op1); ),
                // cmp r8, r/m8
                // REX 3A /r
                // RM
                (RmOperand::Reg(expr), op2) => quote! ( jit.enc_rex_mr(&[0x3a], #expr, #op2); ),
                (op1, op2) => unimplemented!("cmp {}, {}", op1, op2),
            }
        }

        Inst::Shlq(op1, op2) => shift_op(op1, op2),

        Inst::Setcc(flag, op) => {
            let flag: u8 = match flag {
                Flag::Eq => 0x94,
                Flag::Ne => 0x95,
                Flag::Gt => 0x9f,
                Flag::Ge => 0x9d,
                Flag::Lt => 0x9c,
                Flag::Le => 0x9e,
                Flag::A => 0x97,
                Flag::Ae => 0x93,
                Flag::B => 0x92,
                Flag::Be => 0x96,
            };
            quote!( jit.enc_rex_mr2(&[0x0f, #flag], #op); )
        }

        Inst::Cqo => {
            quote! ( jit.emit(&[0x48, 0x99]); )
        }

        Inst::Negq(op) => match op {
            op => quote! ( jit.enc_rexw_digit(&[0xf7], #op, 3, Imm::None); ),
        },

        Inst::Imul(op1, op2) => {
            // IMUL r64, r/m64: r64 <- r64 * r/m64
            match (op1, op2) {
                // IMUL r64, r/m64
                // REX.W 0F AF /r
                // RM
                (RmiOperand::Reg(expr), op2) => quote! (
                    jit.enc_rexw_mr(&[0x0f,0xaf], #expr, #op2);
                ),

                _ => unimplemented!(),
            }
        }

        Inst::Idiv(op) => {
            // IDIV r/m64: RAX:quo RDX:rem <- RDX:RAX / r/m64
            match op {
                // IDIV r/m64
                // REX.W F7 /7
                op => quote! { jit.enc_rexw_digit(&[0xf7], #op, 7, Imm::None); },
            }
        }

        Inst::Movsd(op1, op2) => match (op1, op2) {
            (XmOperand::Xmm(op1), op2) => quote! {
                jit.emitb(0xf2);
                jit.enc_rex_mr(&[0x0f, 0x10], Reg::from(#op1), #op2);
            },
            (op1, XmOperand::Xmm(op2)) => quote! {
                jit.emitb(0xf2);
                jit.enc_rex_mr(&[0x0f, 0x11], Reg::from(#op2), #op1);
            },
            _ => {
                panic!("'MOVSD m64, m64' does not exists.")
            }
        },
        Inst::Addsd(op1, op2) => binary_sd_op(0x58, op1, op2),
        Inst::Subsd(op1, op2) => binary_sd_op(0x5c, op1, op2),
        Inst::Mulsd(op1, op2) => binary_sd_op(0x59, op1, op2),
        Inst::Divsd(op1, op2) => binary_sd_op(0x5e, op1, op2),

        Inst::Cvtsi2sdq(Xmm(op1), op2) => {
            quote! {
                jit.emitb(0xf2);
                jit.enc_rexw_mr(&[0x0f, 0x2a], Reg::from(#op1), #op2);
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
        Inst::Jcc(cond, dest) => {
            let cond: u8 = match cond {
                // JNE rel32
                // 0F 85 cd
                // TODO: support rel8
                Cond::Ne => 0x85,
                // JE rel32
                // 0F 84 cd
                // TODO: support rel8
                Cond::Eq => 0x84,
                // JGE rel32
                // 0F 8D cd
                // TODO: support rel8
                Cond::Ge => 0x8D,
                // JG rel32
                // 0F 8F cd
                // TODO: support rel8
                Cond::Gt => 0x8f,
                // JLE rel32
                // 0F 8E cd
                // TODO: support rel8
                Cond::Le => 0x8e,
                // JL rel32
                // 0F 8C cd
                // TODO: support rel8
                Cond::Lt => 0x8c,
                // JAE rel32
                // 0F 83 cd
                // TODO: support rel8
                Cond::Ae => 0x83,
                // JA rel32
                // 0F 87 cd
                // TODO: support rel8
                Cond::A => 0x87,
                // JBE rel32
                // 0F 86 cd
                // TODO: support rel8
                Cond::Be => 0x86,
                // JB rel32
                // 0F 82 cd
                // TODO: support rel8
                Cond::B => 0x82,
            };
            quote!( jit.enc_d(&[0x0f, #cond], #dest); )
        }

        Inst::Syscall => quote!(
            jit.emit(&[0x0f, 0x05]);
        ),
        Inst::UComIsd(op1, op2) => {
            let op1 = op1.0;
            quote!(
                jit.emitb(0x66);
                jit.enc_rex_mr(&[0x0f, 0x2e], Reg::from(#op1), #op2);
            )
        }
    }
}

fn movq(op1: MovOperand, op2: MovOperand) -> TokenStream {
    match (op1, op2) {
        (MovOperand::Xmm(op1), op2) => match &op2 {
            MovOperand::Imm(_) => panic!("'MOV xmm, Imm' does not exists."),
            MovOperand::Reg(op2) => quote! {
                let op2 = Or::reg(#op2);
                jit.emitb(0x66);
                jit.enc_rexw_mr(&[0x0f, 0x6e],  Reg::from(#op1), op2);
            },
            op2 => quote! {
                jit.emitb(0xf3);
                jit.enc_rex_mr(&[0x0f, 0x7e],  Reg::from(#op1), #op2);
            },
        },
        (MovOperand::Imm(_), op2) => panic!("'MOV Imm, {}' does not exists.", op2),
        // MOV r/m64, imm32
        // REX.W + C7 /0 id
        // MI
        //
        // MOV r64, imm64
        // REX.W + B8+ rd io
        // OI
        (MovOperand::Reg(expr), MovOperand::Imm(i)) => {
            quote!(
                let imm = (#i) as i64;
                let rm_op = Or::reg(#expr);
                if let Ok(imm) = i32::try_from(imm) {
                    // MOV r/m64, imm32
                    jit.enc_rexw_mi(0xc7, rm_op, Imm::L(imm));
                } else {
                    // MOV r64, imm64
                    jit.enc_rexw_o(0xb8, #expr);
                    jit.emitq(imm as u64);
                };
            )
        }
        // MOV r/m64, imm32
        // REX.W + C7 /0 id
        // MI
        (op1, MovOperand::Imm(i)) => {
            let op1_str = format!("{}", op1);
            quote! {
                let imm = (#i) as i64;
                if let Ok(imm) = i32::try_from(imm)  {
                    jit.enc_rexw_mi(0xc7, #op1, Imm::L(imm));
                } else {
                    panic!("'MOV {}, imm64' does not exists.", #op1_str);
                }
            }
        }
        // MOV r/m64,r64
        // REX.W + 89 /r
        // MR
        (op1, MovOperand::Reg(expr)) => quote!( jit.enc_rexw_mr(&[0x89], #expr, #op1); ),
        (MovOperand::Reg(op1), MovOperand::Xmm(op2)) => quote! {
            let op1 = Or::reg(#op1);
            jit.emitb(0x66);
            jit.enc_rexw_mr(&[0x0f, 0x7e],  Reg::from(#op2), op1);
        },
        (op1, MovOperand::Xmm(op2)) => quote! {
            jit.emitb(0x66);
            jit.enc_rex_mr(&[0x0f, 0xd6], Reg::from(#op2), #op1);
        },
        // MOV r64,m64
        // REX.W + 8B /r
        // RM
        (MovOperand::Reg(op1), op2) => quote!( jit.enc_rexw_mr(&[0x8b], #op1, #op2); ),
        (op1, op2) => unimplemented!("MOV {}, {}", op1, op2),
    }
}

fn binary_op(
    op_name: &str,
    op_imm: u8,
    op_mr: u8,
    op_rm: u8,
    digit: u8,
    op1: RmOperand,
    op2: RmiOperand,
) -> TokenStream {
    match (op1, op2) {
        // OP r/m64, imm32
        // OP=ADD REX.W 81 /0 id
        // OP=OR  REX.W 81 /1 id
        // OP=ADC REX.W 81 /2 id
        // OP=SBB REX.W 81 /3 id
        // OP=AND REX.W 81 /4 id
        // OP=SUB REX.W 81 /5 id
        // MI
        (RmOperand::Reg(expr), RmiOperand::Imm(i)) => {
            quote! {
                let imm = (#i) as i64;
                if let Ok(imm) = i32::try_from(imm) {
                    let rm_op = Or::reg(#expr);
                    jit.enc_rexw_digit(&[#op_imm], rm_op, #digit, Imm::L(imm));
                } else {
                    panic!("{} {:?}, imm64' does not exists.", #op_name, #expr);
                }
            }
        }
        (op1, RmiOperand::Imm(i)) => {
            let op1_str = format!("{}", op1);
            quote! {
                let imm = (#i) as i64;
                if let Ok(imm) = i32::try_from(imm) {
                    jit.enc_rexw_digit(&[#op_imm], #op1, #digit, Imm::L(imm));
                } else {
                    panic!("'{} {}, imm64' does not exists.", #op_name, #op1_str);
                }
            }
        }
        // OP r/m64, r64
        // REX.W op_mr /r
        // MR
        (op1, RmiOperand::Reg(expr)) => quote! ( jit.enc_rexw_mr(&[#op_mr], #expr, #op1); ),
        // OP r64, m64
        // REX.W op_rm /r
        // RM
        (RmOperand::Reg(expr), op2) => quote! ( jit.enc_rexw_mr(&[#op_rm], #expr, #op2); ),
        (op1, op2) => unimplemented!("{} {}, {}", op_name, op1, op2),
    }
}

// Shl r/m64, imm8
// REX.W C1 /4 ib
//
// Shl r/m64, 1
// REX.W D1 /4
//
// Shl r/m64, Cl
// REX.W D3 /4
fn shift_op(op1: RmOperand, op2: RiOperand) -> TokenStream {
    match (op1, op2) {
        (op1, RiOperand::Imm(i)) => {
            let op1_str = format!("{}", op1);
            // Shl r/m64, imm8
            // REX.W C1 /4 ib
            quote! {
                let imm = (#i) as i64;
                if imm == 1 {
                    jit.enc_rexw_digit(&[0xd1], #op1, 4, Imm::None);
                } else
                if let Ok(imm) = i8::try_from(imm) {
                    jit.enc_rexw_digit(&[0xc1], #op1, 4, Imm::B(imm));
                } else {
                    panic!("'shl {}, imm' imm should be 8 bit.", #op1_str);
                }
            }
        }
        // Shl r/m64, Cl
        // REX.W D3 /4
        (op1, RiOperand::Reg(reg)) => {
            let op1_str = format!("{}", op1);
            quote! {
                if !#reg.is_cl() {
                    panic!("'shl {}, reg' reg should be CL.", #op1_str);
                };
                jit.enc_rexw_digit(&[0xd3], #op1, 4, Imm::None);
            }
        }
    }
}

fn binary_sd_op(operand: u8, op1: Xmm, op2: XmOperand) -> TokenStream {
    let op1 = op1.0;
    quote! {
        jit.emitb(0xf2);
        jit.enc_rex_mr(&[0x0f, #operand], Reg::from(#op1), #op2);
    }
}

fn push_pop(opcode: u8, op: RmiOperand) -> TokenStream {
    match op {
        // PUSH r64     POP r64
        // 50 +rd       58 +rd
        // O            O
        RmiOperand::Reg(reg) => quote! ( jit.enc_o(#opcode, #reg); ),
        op => unimplemented!("PUSH/POP {:?}", op),
    }
}
