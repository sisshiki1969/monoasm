use super::inst::*;
use proc_macro2::TokenStream;
use quote::quote;

pub fn compile(inst: Inst) -> TokenStream {
    match inst {
        Inst::Label(ident) => quote!( jit.bind_label(&#ident); ),
        Inst::F64(f) => {
            quote!( jit.emit(&#f.to_ne_bytes()); )
        }
        Inst::I64(i) => {
            quote!( jit.emitq(#i as u64); )
        }

        Inst::Movq(op1, op2) => movq(op1, op2),
        Inst::Movl(op1, op2) => movl(op1, op2),
        Inst::Movw(op1, op2) => movw(op1, op2),
        Inst::Movb(op1, op2) => movb(op1, op2),
        Inst::Movsxl(op1, op2) => quote!( jit.enc_rexw_mr(&[0x63], #op1, #op2); ),
        Inst::Movzxw(op1, op2) => quote!( jit.enc_rexw_mr(&[0x0f, 0xb7], #op1, #op2); ),
        Inst::Movsxw(op1, op2) => quote!( jit.enc_rexw_mr(&[0x0f, 0xbf], #op1, #op2); ),
        Inst::Movzxb(op1, op2) => quote!( jit.enc_rexw_mr(&[0x0f, 0xb6], #op1, #op2); ),
        Inst::Movsxb(op1, op2) => quote!( jit.enc_rexw_mr(&[0x0f, 0xbe], #op1, #op2); ),
        Inst::Add(size, op1, op2) => binary_op(size, "ADD", 0x01, 0, op1, op2),
        Inst::Or(size, op1, op2) => binary_op(size, "OR", 0x09, 1, op1, op2),
        Inst::Adc(size, op1, op2) => binary_op(size, "ADC", 0x11, 2, op1, op2),
        Inst::Sbb(size, op1, op2) => binary_op(size, "SBB", 0x19, 3, op1, op2),
        Inst::And(size, op1, op2) => binary_op(size, "AND", 0x21, 4, op1, op2),
        Inst::Sub(size, op1, op2) => binary_op(size, "SUB", 0x29, 5, op1, op2),
        Inst::Xor(size, op1, op2) => binary_op(size, "XOR", 0x31, 6, op1, op2),
        Inst::Cmp(size, op1, op2) => binary_op(size, "CMP", 0x39, 7, op1, op2),

        Inst::Xchg(size, op1, op2) => match size {
            OperandSize::QWORD => match (&op1, &op2) {
                (RmOperand::Reg(r1), RmOperand::Reg(r2)) => quote! {
                    if #r1 == Reg::from(0) {
                        if #r2 == Reg::from(0) {
                            jit.emitb(0x90);
                        } else {
                            jit.enc_rexw_o(0x90, #r2);
                        }
                    } else if #r2 == Reg::from(0) {
                        jit.enc_rexw_o(0x90, #r1);
                    } else {
                        jit.enc_rexw_mr(&[0x87], #r2, #op1);
                    }
                },
                (op1, RmOperand::Reg(op2)) => quote! {
                    jit.enc_rexw_mr(&[0x87], #op2, #op1);
                },
                (RmOperand::Reg(op1), RmOperand::Ind(op2)) => quote! {
                    jit.enc_rexw_mr(&[0x87], #op1, #op2);
                },
                (op1, op2) => {
                    panic!("'Xchg {op1}, {op2}' does not exists.")
                }
            },
            _ => panic!("'Xchg {:?} {op1}, {op2}' does not exists.", size),
        },

        Inst::Testq(op1, op2) => match op2 {
            RiOperand::Imm(i) => {
                let op1_str = format!("{}", op1);
                match &op1 {
                    RmOperand::Reg(reg) => {
                        quote! {
                            let imm = (#i) as i64;
                            if let Ok(imm) = i32::try_from(imm)  {
                                if #reg.is_rax() {
                                    jit.emit(&[0x48, 0xa9]);
                                    jit.emitl(imm as u32);
                                } else {
                                    jit.enc_rexw_mi(0xf7, #op1, Imm::L(imm));
                                }
                            } else {
                                panic!("'TEST {}, imm64' does not exists.", #op1_str);
                            }
                        }
                    }
                    _ => {
                        quote! {
                            let imm = (#i) as i64;
                            if let Ok(imm) = i32::try_from(imm)  {
                                jit.enc_rexw_mi(0xf7, #op1, Imm::L(imm));
                            } else {
                                panic!("'TEST {}, imm64' does not exists.", #op1_str);
                            }
                        }
                    }
                }
            }
            RiOperand::Reg(expr) => quote!( jit.enc_rexw_mr(&[0x85], #expr, #op1); ),
        },
        Inst::Testb(op1, op2) => match op2 {
            RiOperand::Imm(i) => {
                let op1_str = format!("{}", op1);
                match &op1 {
                    RmOperand::Reg(reg) => {
                        quote! {
                            let imm = (#i) as i64;
                            if let Ok(imm) = i8::try_from(imm)  {
                                if #reg.is_rax() {
                                    jit.emit(&[0xa8]);
                                    jit.emitb(imm as u8);
                                } else {
                                    jit.enc_mi_byte(0xf6, #op1, Imm::B(imm));
                                }
                            } else {
                                panic!("'TEST {}, imm64' does not exists.", #op1_str);
                            }
                        }
                    }
                    _ => {
                        quote! {
                            let imm = (#i) as i64;
                            if let Ok(imm) = i8::try_from(imm)  {
                                jit.enc_mi_byte(0xf6, #op1, Imm::B(imm));
                            } else {
                                panic!("'TEST {}, imm64' does not exists.", #op1_str);
                            }
                        }
                    }
                }
            }
            RiOperand::Reg(expr) => quote!( jit.enc_mr_byte(&[0x84], #expr, #op1); ),
        },

        Inst::Rol(width, op1, op2) => shift_op(width, 0, op1, op2, "ROL"),
        Inst::Ror(width, op1, op2) => shift_op(width, 1, op1, op2, "ROR"),
        Inst::Shl(width, op1, op2) => shift_op(width, 4, op1, op2, "SHL"),
        Inst::Shr(width, op1, op2) => shift_op(width, 5, op1, op2, "SHR"),
        Inst::Sal(width, op1, op2) => shift_op(width, 4, op1, op2, "SAL"),
        Inst::Sar(width, op1, op2) => shift_op(width, 7, op1, op2, "SAR"),

        Inst::Setcc(cond, op) => {
            let cond: u8 = 0x90 + cond as u8;
            quote!( jit.enc_m_byte(&[0x0f, #cond], #op); )
        }
        Inst::Cmovcc(size, cond, op1, op2) => {
            let cond: u8 = 0x40 + cond as u8;
            match size {
                OperandSize::QWORD => quote!( jit.enc_rexw_mr(&[0x0f, #cond], #op1, #op2); ),
                OperandSize::DWORD => quote!( jit.enc_mr(&[0x0f, #cond], #op1, #op2); ),
                OperandSize::WORD => quote!(
                    jit.emitb(0x66);
                    jit.enc_mr(&[0x0f, #cond], #op1, #op2);
                ),
                _ => unimplemented!(),
            }
        }

        Inst::Cqo => quote! ( jit.emit(&[0x48, 0x99]); ),
        Inst::Cdq => quote! ( jit.emit(&[0x99]); ),
        Inst::Int3 => quote! ( jit.emit(&[0xcc]); ),

        Inst::Negq(op) => quote! ( jit.enc_rexw_digit(&[0xf7], #op, 3, Imm::None); ),
        Inst::Notq(op) => quote! ( jit.enc_rexw_digit(&[0xf7], #op, 2, Imm::None); ),

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

        Inst::Idiv(op) => divide_op(OperandSize::QWORD, true, op),
        Inst::Idivl(op) => divide_op(OperandSize::DWORD, true, op),
        Inst::Div(op) => divide_op(OperandSize::QWORD, false, op),
        Inst::Divl(op) => divide_op(OperandSize::DWORD, false, op),

        Inst::Movsd(op1, op2) => match (op1, op2) {
            (XmOperand::Xmm(op1), op2) => quote! {
                jit.emitb(0xf2);
                jit.enc_mr(&[0x0f, 0x10], Reg::from(#op1), #op2);
            },
            (op1, XmOperand::Xmm(op2)) => quote! {
                jit.emitb(0xf2);
                jit.enc_mr(&[0x0f, 0x11], Reg::from(#op2), #op1);
            },
            _ => {
                panic!("'MOVSD m64, m64' does not exists.")
            }
        },
        Inst::Addsd(op1, op2) => binary_sd_op(0x58, op1, op2),
        Inst::Subsd(op1, op2) => binary_sd_op(0x5c, op1, op2),
        Inst::Mulsd(op1, op2) => binary_sd_op(0x59, op1, op2),
        Inst::Divsd(op1, op2) => binary_sd_op(0x5e, op1, op2),
        Inst::Minsd(op1, op2) => binary_sd_op(0x5d, op1, op2),
        Inst::Maxsd(op1, op2) => binary_sd_op(0x5f, op1, op2),
        Inst::Xorps(Xmm(op1), op2) => {
            quote! {
                jit.enc_mr(&[0x0f, 0x57], Reg::from(#op1), #op2);
            }
        }

        Inst::Lea(op1, op2) => match (op1, op2) {
            (RmOperand::Reg(op1), RmOperand::Ind(op2)) => quote! {
                jit.enc_rexw_mr(&[0x8d], #op1, #op2);
            },
            (op1, op2) => {
                panic!("'Lea {}, {}' does not exists.", op1, op2)
            }
        },

        Inst::Cvtsi2sdq(Xmm(op1), op2) => {
            quote! {
                jit.emitb(0xf2);
                jit.enc_rexw_mr(&[0x0f, 0x2a], Reg::from(#op1), #op2);
            }
        }

        Inst::Cvttsd2siq(op1, op2) => {
            quote! {
                jit.emitb(0xf2);
                jit.enc_mr(&[0x0f, 0x2c], #op1, #op2);
            }
        }

        Inst::Andpd(Xmm(op1), op2) => {
            quote! {
                jit.emitb(0x66);
                jit.enc_mr(&[0x0f, 0x54], Reg::from(#op1), #op2);
            }
        }

        Inst::Xorpd(Xmm(op1), op2) => {
            quote! {
                jit.emitb(0x66);
                jit.enc_mr(&[0x0f, 0x57], Reg::from(#op1), #op2);
            }
        }

        Inst::Roundpd(Xmm(op1), op2, ImmOperand(op3)) => {
            quote! {
                jit.emitb(0x66);
                jit.enc_mr(&[0x0f, 0x3a, 0x09], Reg::from(#op1), #op2);
                jit.emitb(#op3);
            }
        }

        Inst::Sqrtpd(Xmm(op1), op2) => {
            quote! {
                jit.emitb(0x66);
                jit.enc_mr(&[0x0f, 0x51], Reg::from(#op1), #op2);
            }
        }
        Inst::Sqrtsd(Xmm(op1), op2) => {
            quote! {
                jit.emitb(0xf2);
                jit.enc_mr(&[0x0f, 0x51], Reg::from(#op1), #op2);
            }
        }

        Inst::Pushq(op) => push_pop(0x50, (0xff, 6), op),
        Inst::Popq(op) => push_pop(0x58, (0x8f, 0), op),

        Inst::Call(dest) => match dest {
            Dest::Reg(r) => {
                // CALL r/m64
                // FF /2
                quote! {
                    jit.enc_digit(&[0xff], #r, 2);
                }
            }
            Dest::Ind(ind) => {
                // CALL r/m64
                // FF /2
                quote! {
                    jit.enc_digit(&[0xff], #ind, 2);
                }
            }
            Dest::Rel(dest) => {
                // CALL rel32
                // E8 cd
                quote! {
                    jit.emitb(0xe8);
                    jit.emit_reloc(#dest, 4);
                }
            }
            Dest::Disp(imm) => {
                quote! {
                    if let Ok(imm) = i32::try_from(#imm as i64) {
                        jit.emitb(0xe8);
                        jit.emitl(imm as u32);
                    } else {
                        panic!("'CALL ({})' out of range.", #imm);
                    }
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
                    jit.enc_digit(&[0xff], #r, 4);
                )
            }
            Dest::Ind(ind) => {
                // JMP r/m64
                // FF /4
                // M
                quote! (
                    jit.enc_digit(&[0xff], #ind.clone(), 4);
                )
            }
            // JMP rel32
            // E9 cd
            // D
            Dest::Rel(dest) => quote! ( jit.enc_d(&[0xe9], &#dest); ),
            dest => unimplemented!("JMP {:?}", dest),
        },
        Inst::Jcc(cond, dest) => {
            let cond = 0x80 + cond as u8;
            quote!( jit.enc_d(&[0x0f, #cond], &#dest); )
        }

        Inst::UComIsd(op1, op2) => {
            let op1 = op1.0;
            quote!(
                jit.emitb(0x66);
                jit.enc_mr(&[0x0f, 0x2e], Reg::from(#op1), #op2);
            )
        }

        Inst::Syscall => quote!(
            jit.emit(&[0x0f, 0x05]);
        ),
        Inst::Leave => quote!(
            jit.emitb(0xc9);
        ),
        Inst::Tzcnt(width, op1, op2) => count_op(width, 0xbc, op1, op2),
        Inst::Lzcnt(width, op1, op2) => count_op(width, 0xbd, op1, op2),
        Inst::Popcnt(width, op1, op2) => count_op(width, 0xb8, op1, op2),
    }
}

fn movq(op1: MovOperand, op2: MovOperand) -> TokenStream {
    match (op1, op2) {
        (MovOperand::Xmm(op1), op2) => match &op2 {
            MovOperand::Imm(..) => panic!("'MOV xmm, Imm' does not exists."),
            MovOperand::Reg(op2) => quote! {
                let op2 = Rm::reg(#op2);
                jit.emitb(0x66);
                jit.enc_rexw_mr(&[0x0f, 0x6e], Reg::from(#op1), op2);
            },
            op2 => quote! {
                jit.emitb(0xf3);
                jit.enc_mr(&[0x0f, 0x7e], Reg::from(#op1), #op2);
            },
        },
        (MovOperand::Imm(..), op2) => panic!("'MOV Imm, {}' does not exists.", op2),
        // MOV r/m64, imm32
        // REX.W + C7 /0 id
        // MI
        //
        // MOV r64, imm64
        // REX.W + B8+ rd io
        // OI
        (MovOperand::Reg(expr), MovOperand::Imm(i, size)) => {
            if let Some(size) = size {
                assert_eq!(OperandSize::QWORD, size);
                quote!(
                    let imm = (#i) as i64;
                    let rm_op = Rm::reg(#expr);
                    // MOV r64, imm64
                    jit.enc_rexw_o(0xb8, #expr);
                    jit.emitq(imm as u64);
                )
            } else {
                quote!(
                    let imm = (#i) as i64;
                    let rm_op = Rm::reg(#expr);
                    //if let Ok(imm) = i32::try_from(imm) {
                    //    // MOV r/m64, imm32
                    //    jit.enc_rexw_mi(0xc7, rm_op, Imm::L(imm));
                    //} else {
                    //    // MOV r64, imm64
                        jit.enc_rexw_o(0xb8, #expr);
                        jit.emitq(imm as u64);
                    //};
                )
            }
        }
        // MOV r/m64, imm32
        // REX.W + C7 /0 id
        // MI
        (op1, MovOperand::Imm(i, None)) => {
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
            let op1 = Rm::reg(#op1);
            jit.emitb(0x66);
            jit.enc_rexw_mr(&[0x0f, 0x7e],  Reg::from(#op2), op1);
        },
        (op1, MovOperand::Xmm(op2)) => quote! {
            jit.emitb(0x66);
            jit.enc_mr(&[0x0f, 0xd6], Reg::from(#op2), #op1);
        },
        // MOV r64,m64
        // REX.W + 8B /r
        // RM
        (MovOperand::Reg(op1), op2) => quote!( jit.enc_rexw_mr(&[0x8b], #op1, #op2); ),
        (op1, op2) => unimplemented!("MOV {}, {}", op1, op2),
    }
}

fn movl(op1: RmOperand, op2: RmiOperand) -> TokenStream {
    match (op1, op2) {
        // MOV r32, imm32
        // B8+ rd id
        // OI
        (RmOperand::Reg(expr), RmiOperand::Imm(i)) => {
            quote!(
                let imm = (#i) as i64;
                if let Ok(imm) = i32::try_from(imm) {
                    // MOV r32, imm32
                    jit.enc_oi(0xb8, #expr);
                    jit.emitl(imm as u32);
                } else {
                    panic!("'MOVL {}, {}' does not exists.", #expr, imm);
                };
            )
        }
        // MOV r/m32, imm32
        // C7 /0 id
        // MI
        (op1, RmiOperand::Imm(i)) => {
            let op1_str = format!("{}", op1);
            quote! {
                let imm = (#i) as i64;
                if let Ok(imm) = i32::try_from(imm)  {
                    jit.enc_mi(0xc7, #op1, Imm::L(imm));
                } else {
                    panic!("'MOVL {}, imm64' does not exists.", #op1_str);
                }
            }
        }
        // MOV r/m32, r32
        // 89 /r
        // MR
        (op1, RmiOperand::Reg(expr)) => quote!( jit.enc_mr(&[0x89], #expr, #op1); ),
        // MOV r32, m32
        // 8B /r
        // RM
        (RmOperand::Reg(op1), op2) => quote!( jit.enc_mr(&[0x8b], #op1, #op2); ),
        (op1, op2) => unimplemented!("MOV {}, {}", op1, op2),
    }
}

fn movw(op1: RmOperand, op2: RmiOperand) -> TokenStream {
    match (op1, op2) {
        // MOV r16, imm16
        // B8+ rw iw
        (RmOperand::Reg(expr), RmiOperand::Imm(i)) => {
            quote!(
                let imm = (#i) as i64;
                if let Ok(imm) = i16::try_from(imm) {
                    // MOV r16, imm16
                    jit.emitb(0x66);
                    jit.enc_oi(0xb8, #expr);
                    jit.emitw(imm as u16);
                } else {
                    panic!("'MOVW {}, {}' does not exists.", #expr, imm);
                };
            )
        }
        // MOV r/m16, imm16
        // C7 /0 iw
        (op1, RmiOperand::Imm(i)) => {
            let op1_str = format!("{}", op1);
            quote! {
                let imm = (#i) as i64;
                if let Ok(imm) = i16::try_from(imm)  {
                    jit.emitb(0x66);
                    jit.enc_mi(0xc7, #op1, Imm::W(imm));
                } else {
                    panic!("'MOVW {}, {}' does not exists.", #op1_str, imm);
                }
            }
        }
        // MOV r/m16, r16
        // 89 /r
        (op1, RmiOperand::Reg(expr)) => quote!(
            jit.emitb(0x66);
            jit.enc_mr(&[0x89], #expr, #op1);
        ),
        // MOV r16, m16
        // 8B /r
        (RmOperand::Reg(op1), op2) => quote!(
            jit.emitb(0x66);
            jit.enc_mr(&[0x8b], #op1, #op2);
        ),
        (op1, op2) => unimplemented!("MOV {}, {}", op1, op2),
    }
}

fn movb(op1: RmOperand, op2: RmiOperand) -> TokenStream {
    match (op1, op2) {
        // MOV r8, imm8
        // B0+ rb ib
        (RmOperand::Reg(expr), RmiOperand::Imm(i)) => {
            quote!(
                let imm = (#i) as i64;
                if let Ok(imm) = i8::try_from(imm) {
                    // MOV r8, imm8
                    //jit.emitb(0x66);
                    jit.enc_oi_byte(0xb0, #expr);
                    jit.emitb(imm as u8);
                } else {
                    panic!("'MOVW {}, {}' does not exists.", #expr, imm);
                };
            )
        }
        // MOV r/m8, imm8
        // C6 /0 ib
        (op1, RmiOperand::Imm(i)) => {
            let op1_str = format!("{}", op1);
            quote! {
                let imm = (#i) as i64;
                if let Ok(imm) = i8::try_from(imm)  {
                    jit.enc_mi_byte(0xc6, #op1, Imm::B(imm));
                } else {
                    panic!("'MOVW {}, {}' does not exists.", #op1_str, imm);
                }
            }
        }
        // MOV r/m8, r8
        // 88 /r
        (op1, RmiOperand::Reg(expr)) => quote!(
            //jit.emitb(0x66);
            jit.enc_mr_byte(&[0x88], #expr, #op1);
        ),
        // MOV r8, m8
        // 8A /r
        (RmOperand::Reg(op1), op2) => quote!(
            //jit.emitb(0x66);
            jit.enc_mr_byte(&[0x8a], #op1, #op2);
        ),
        (op1, op2) => unimplemented!("MOV {}, {}", op1, op2),
    }
}

fn binary_op(
    size: OperandSize,
    op_name: &str,
    op_mr: u8,
    digit: u8,
    op1: RmOperand,
    op2: RmiOperand,
) -> TokenStream {
    match size {
        OperandSize::QWORD => binary_opq(op_name, op_mr, digit, op1, op2),
        OperandSize::DWORD => binary_opl(op_name, op_mr, digit, op1, op2),
        OperandSize::WORD => binary_opw(op_name, op_mr, digit, op1, op2),
        OperandSize::BYTE => binary_opb(op_name, op_mr - 1, digit, op1, op2),
    }
}

fn binary_opq(op_name: &str, op_mr: u8, digit: u8, op1: RmOperand, op2: RmiOperand) -> TokenStream {
    let op_rm = op_mr + 2;
    match (op1, op2) {
        // OP r/m64, imm32
        // OP=ADD REX.W 81 /0 id
        // OP=OR  REX.W 81 /1 id
        // OP=ADC REX.W 81 /2 id
        // OP=SBB REX.W 81 /3 id
        // OP=AND REX.W 81 /4 id
        // OP=SUB REX.W 81 /5 id
        // OP=XOR REX.W 81 /6 id
        // OP=CMP REX.W 81 /7 id
        //
        // OP r/m64, imm8
        // OP=ADD REX.W 83 /0 id
        // OP=OR  REX.W 83 /1 id
        // OP=ADC REX.W 83 /2 id
        // OP=SBB REX.W 83 /3 id
        // OP=AND REX.W 83 /4 id
        // OP=SUB REX.W 83 /5 id
        // OP=XOR REX.W 83 /6 id
        // OP=CMP REX.W 83 /7 id
        (op1, RmiOperand::Imm(i)) => {
            let op1_str = format!("{}", op1);
            quote! {
                let imm = (#i) as i64;
                if let Ok(imm) = i8::try_from(imm) {
                    jit.enc_rexw_digit(&[0x83], #op1, #digit, Imm::B(imm));
                } else if let Ok(imm) = i32::try_from(imm) {
                    jit.enc_rexw_digit(&[0x81], #op1, #digit, Imm::L(imm));
                } else {
                    panic!("'{} {}, imm64' does not exists.", #op_name, #op1_str);
                }
            }
        }
        // OP r/m64, r64
        // OP=ADD REX.W 01 /r
        // OP=OR  REX.W 09 /r
        // OP=ADC REX.W 11 /r
        // OP=SBB REX.W 19 /r
        // OP=AND REX.W 21 /r
        // OP=SUB REX.W 29 /r
        // OP=XOR REX.W 31 /r
        // OP=CMP REX.W 39 /r
        (op1, RmiOperand::Reg(expr)) => quote! ( jit.enc_rexw_mr(&[#op_mr], #expr, #op1); ),
        // OP r64, m64
        // OP=ADD REX.W 03 /r
        // OP=OR  REX.W 0b /r
        // OP=ADC REX.W 13 /r
        // OP=SBB REX.W 1b /r
        // OP=AND REX.W 23 /r
        // OP=SUB REX.W 2b /r
        // OP=XOR REX.W 33 /r
        // OP=CMP REX.W 3b /r
        (RmOperand::Reg(expr), op2) => quote! ( jit.enc_rexw_mr(&[#op_rm], #expr, #op2); ),
        (op1, op2) => unimplemented!("{} {}, {}", op_name, op1, op2),
    }
}

fn binary_opl(op_name: &str, op_mr: u8, digit: u8, op1: RmOperand, op2: RmiOperand) -> TokenStream {
    let op_rm = op_mr + 2;
    match (op1, op2) {
        // OP r/m32, imm32
        // OP=ADD 81 /0 id
        // OP=OR  81 /1 id
        // OP=ADC 81 /2 id
        // OP=SBB 81 /3 id
        // OP=AND 81 /4 id
        // OP=SUB 81 /5 id
        // OP=XOR 81 /6 id
        // OP=CMP 81 /7 id
        //
        // OP r/m32, imm8
        // OP=ADD 83 /0 id
        // OP=OR  83 /1 id
        // OP=ADC 83 /2 id
        // OP=SBB 83 /3 id
        // OP=AND 83 /4 id
        // OP=SUB 83 /5 id
        // OP=XOR 83 /6 id
        // OP=CMP 83 /7 id
        (op1, RmiOperand::Imm(i)) => {
            let op1_str = format!("{}", op1);
            quote! {
                let imm = (#i) as i64;
                if let Ok(imm) = i8::try_from(imm) {
                    jit.enc_digit_imm(&[0x83], #op1, #digit, Imm::B(imm));
                } else if let Ok(imm) = i32::try_from(imm) {
                    jit.enc_digit_imm(&[0x81], #op1, #digit, Imm::L(imm));
                } else {
                    panic!("'{} {}, imm64' does not exists.", #op_name, #op1_str);
                }
            }
        }
        // OP r/m32, r32
        // OP=ADD 01 /r
        // OP=OR  09 /r
        // OP=ADC 11 /r
        // OP=SBB 19 /r
        // OP=AND 21 /r
        // OP=SUB 29 /r
        // OP=XOR 31 /r
        // OP=CMP 39 /r
        (op1, RmiOperand::Reg(expr)) => quote! ( jit.enc_mr(&[#op_mr], #expr, #op1); ),
        // OP r32, m32
        // OP=ADD 03 /r
        // OP=OR  0b /r
        // OP=ADC 13 /r
        // OP=SBB 1b /r
        // OP=AND 23 /r
        // OP=SUB 2b /r
        // OP=XOR 33 /r
        // OP=CMP 3b /r
        (RmOperand::Reg(expr), op2) => quote! ( jit.enc_mr(&[#op_rm], #expr, #op2); ),
        (op1, op2) => unimplemented!("{} {}, {} does not exists.", op_name, op1, op2),
    }
}

fn binary_opw(op_name: &str, op_mr: u8, digit: u8, op1: RmOperand, op2: RmiOperand) -> TokenStream {
    let op_rm = op_mr + 2;
    match (op1, op2) {
        // OP r/m16, imm16
        // OP=ADD 81 /0 iw
        // OP=OR  81 /1 iw
        // OP=ADC 81 /2 iw
        // OP=SBB 81 /3 iw
        // OP=AND 81 /4 iw
        // OP=SUB 81 /5 iw
        // OP=XOR 81 /6 iw
        // OP=CMP 81 /7 iw
        //
        // OP r/m16, imm8
        // OP=ADD 83 /0 ib
        // OP=OR  83 /1 ib
        // OP=ADC 83 /2 ib
        // OP=SBB 83 /3 ib
        // OP=AND 83 /4 ib
        // OP=SUB 83 /5 ib
        // OP=XOR 83 /6 ib
        // OP=CMP 83 /7 ib
        (op1, RmiOperand::Imm(i)) => {
            let op1_str = format!("{}", op1);
            quote! {
                let imm = (#i) as i64;
                jit.emitb(0x66);
                if let Ok(imm) = i8::try_from(imm) {
                    jit.enc_digit_imm(&[0x83], #op1, #digit, Imm::B(imm));
                } else if let Ok(imm) = i16::try_from(imm) {
                    jit.enc_digit_imm(&[0x81], #op1, #digit, Imm::W(imm));
                } else {
                    panic!("'{} {}, imm64' does not exists.", #op_name, #op1_str);
                }
            }
        }
        // OP r/m16, r16
        // OP=ADD 01 /r
        // OP=OR  09 /r
        // OP=ADC 11 /r
        // OP=SBB 19 /r
        // OP=AND 21 /r
        // OP=SUB 29 /r
        // OP=XOR 31 /r
        // OP=CMP 39 /r
        (op1, RmiOperand::Reg(expr)) => quote! {
            jit.emitb(0x66);
            jit.enc_mr(&[#op_mr], #expr, #op1);
        },
        // OP r16, m16
        // OP=ADD 03 /r
        // OP=OR  0b /r
        // OP=ADC 13 /r
        // OP=SBB 1b /r
        // OP=AND 23 /r
        // OP=SUB 2b /r
        // OP=XOR 33 /r
        // OP=CMP 3b /r
        (RmOperand::Reg(expr), op2) => quote! {
            jit.emitb(0x66);
            jit.enc_mr(&[#op_rm], #expr, #op2);
        },
        (op1, op2) => unimplemented!("{} {}, {} does not exists.", op_name, op1, op2),
    }
}

fn binary_opb(op_name: &str, op_mr: u8, digit: u8, op1: RmOperand, op2: RmiOperand) -> TokenStream {
    let op_rm = op_mr + 2;
    match (op1, op2) {
        // OP r/m8, imm8
        // OP=ADD REX 80 /0 ib
        // OP=OR  REX 80 /1 ib
        // OP=ADC REX 80 /2 ib
        // OP=SBB REX 80 /3 ib
        // OP=AND REX 80 /4 ib
        // OP=SUB REX 80 /5 ib
        // OP=XOR REX 80 /6 ib
        // OP=CMP REX 80 /7 ib

        // OP al, imm8
        // OP=ADD 04 ib
        // OP=OR  0c ib
        // OP=ADC 14 ib
        // OP=SBB 1c ib
        // OP=AND 24 ib
        // OP=SUB 2c ib
        // OP=XOR 34 ib
        // OP=CMP 3c ib
        (op1, RmiOperand::Imm(i)) => {
            let op_al = op_mr + 4;
            quote! {
                let imm = (#i) as i64;
                if let Ok(imm) = i8::try_from(imm) {
                    jit.enc_digit_imm_byte(&[0x80], #op_al, #op1, #digit, Imm::B(imm));
                } else {
                    panic!("{} is out of 8bit.", #i);
                }
            }
        }
        // cmp r/m8, r8
        // OP=ADD REX 00 /r
        // OP=OR  REX 08 /r
        // OP=ADC REX 10 /r
        // OP=SBB REX 18 /r
        // OP=AND REX 20 /r
        // OP=SUB REX 28 /r
        // OP=XOR REX 30 /r
        // OP=CMP REX 38 /r
        (op1, RmiOperand::Reg(expr)) => quote! ( jit.enc_mr_byte(&[#op_mr], #expr, #op1); ),
        // OP r8, m8
        // OP=ADD REX 02 /r
        // OP=OR  REX 0a /r
        // OP=ADC REX 12 /r
        // OP=SBB REX 1a /r
        // OP=AND REX 22 /r
        // OP=SUB REX 2a /r
        // OP=XOR REX 32 /r
        // OP=CMP REX 3a /r
        (RmOperand::Reg(expr), op2) => quote! ( jit.enc_mr_byte(&[#op_rm], #expr, #op2); ),
        (op1, op2) => unimplemented!("{} {}, {} does not exists.", op_name, op1, op2),
    }
}

fn divide_op(width: OperandSize, signed: bool, op: RmOperand) -> TokenStream {
    // IDIV r/m64: RAX:quo RDX:rem <- RDX:RAX / r/m64
    // REX.W F7 /7
    // IDIV r/m32: EAX:quo EDX:rem <- EDX:EAX / r/m32
    // REX F7 /7
    // DIV r/m64: RAX:quo RDX:rem <- RDX:RAX / r/m64
    // REX.W F7 /6
    // DIV r/m32: EAX:quo EDX:rem <- EDX:EAX / r/m32
    // REX F7 /6
    let signed = if signed { 7u8 } else { 6u8 };
    match width {
        OperandSize::QWORD => {
            quote! { jit.enc_rexw_digit(&[0xf7], #op, #signed, Imm::None); }
        }
        OperandSize::DWORD => {
            quote! { jit.enc_digit(&[0xf7], #op, #signed); }
        }
        _ => unimplemented!("DIV/IDIV for {:?} is not yet supported.", width),
    }
}

///
/// Shift and rotate operation.
///
/// #### Addressiong modes
///
/// ##### XXX r/m64, imm8
/// - REX.W C1 /Y ib
///
/// ##### XXX r/m64, 1
/// - REX.W D1 /Y
///
/// ##### XXX r/m64, Cl
/// - REX.W D3 /Y
///
/// #### Instructions
/// - XXX=Rol Y=0
/// - XXX=Ror Y=1
/// - XXX=Shl Y=4
/// - XXX=Sal Y=4
/// - XXX=Shr Y=5
/// - XXX=Sar Y=7
///
fn shift_op(
    width: OperandSize,
    digit: u8,
    op1: RmOperand,
    op2: RiOperand,
    inst_str: &str,
) -> TokenStream {
    match width {
        OperandSize::QWORD => match (op1, op2) {
            (op1, RiOperand::Imm(i)) => {
                let op1_str = format!("{}", op1);
                // Shl r/m64, imm8
                // REX.W C1 /4 ib
                quote! {
                    let imm = (#i) as i64;
                    if imm == 1 {
                        jit.enc_rexw_digit(&[0xd1], #op1, #digit, Imm::None);
                    } else if let Ok(imm) = i8::try_from(imm) {
                        jit.enc_rexw_digit(&[0xc1], #op1, #digit, Imm::B(imm));
                    } else {
                        panic!("'{} {}, imm' imm should be 8 bit.", #inst_str, #op1_str);
                    }
                }
            }
            // Shl r/m64, Cl
            // REX.W D3 /4
            (op1, RiOperand::Reg(reg)) => {
                let op1_str = format!("{}", op1);
                quote! {
                    if !#reg.is_cl() {
                        panic!("'{} {}, reg' reg should be CL.", #inst_str, #op1_str);
                    };
                    jit.enc_rexw_digit(&[0xd3], #op1, #digit, Imm::None);
                }
            }
        },
        OperandSize::DWORD => match (op1, op2) {
            (op1, RiOperand::Imm(i)) => {
                let op1_str = format!("{}", op1);
                // Shl r/m32, imm8
                // REX C1 /4 ib
                quote! {
                    let imm = (#i) as i64;
                    if imm == 1 {
                        jit.enc_digit(&[0xd1], #op1, #digit);
                    } else if let Ok(imm) = i8::try_from(imm) {
                        jit.enc_digit_imm(&[0xc1], #op1, #digit, Imm::B(imm));
                    } else {
                        panic!("'{} {}, imm' imm should be 8 bit.", #inst_str, #op1_str);
                    }
                }
            }
            // Shl r/m32, Cl
            // REX D3 /4
            (op1, RiOperand::Reg(reg)) => {
                let op1_str = format!("{}", op1);
                quote! {
                    if !#reg.is_cl() {
                        panic!("'{} {}, reg' reg should be CL.", #inst_str, #op1_str);
                    };
                    jit.enc_digit(&[0xd3], #op1, #digit);
                }
            }
        },
        OperandSize::WORD | OperandSize::BYTE => todo!("not implemented yet."),
    }
}

fn binary_sd_op(operand: u8, op1: Xmm, op2: XmOperand) -> TokenStream {
    let op1 = op1.0;
    quote! {
        jit.emitb(0xf2);
        jit.enc_mr(&[0x0f, #operand], Reg::from(#op1), #op2);
    }
}

fn push_pop(opcode_r: u8, opcode_m: (u8, u8), op: RmOperand) -> TokenStream {
    match op {
        // PUSH r64     POP r64
        // 50 +rd       58 +rd
        // O            O
        RmOperand::Reg(reg) => quote! ( jit.enc_o(#opcode_r, #reg); ),
        // PUSH r/m64   POP r/m64
        // ff /6        8f /0
        // M            M
        op => {
            let (opcode, digit) = opcode_m;
            quote! ( jit.enc_digit(&[#opcode], #op, #digit); )
        }
    }
}

fn count_op(width: OperandSize, opcode: u8, op1: Register, op2: RmOperand) -> TokenStream {
    match width {
        OperandSize::QWORD => {
            quote!(
                jit.emitb(0xf3);
                jit.enc_rexw_mr(&[0x0f, #opcode], #op1, #op2);
            )
        }
        OperandSize::DWORD => {
            quote!(
                jit.emitb(0xf3);
                jit.enc_mr(&[0x0f, #opcode], #op1, #op2);
            )
        }
        _ => unimplemented!("Count operation for {:?} is not yet supported.", width),
    }
}
