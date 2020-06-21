use super::inst::*;
use proc_macro2::{TokenStream};
use quote::quote;

#[derive(Copy, Clone, PartialEq, Debug)]
enum Mode {
    Ind = 0,   // (rax)
    InD8 = 1,  // (rax + disp8)
    InD32 = 2, // (rax + disp32)
    Reg = 3,   // rax
}

impl Mode {
    fn from_disp(offset: Imm) -> Self {
        match offset {
            Imm::Imm(i) => {
                if std::i8::MIN as i32 <= i && i <= std::i8::MAX as i32 {
                    Mode::InD8
                } else {
                    Mode::InD32
                }
            }
            Imm::Expr(_) => Mode::InD32,
        }
    }
}

pub fn compile(inst: Inst) -> TokenStream {
    match inst {
        Inst::Label(ident) => quote!( jit.bind_label(#ident); ),
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
                (Operand::Reg(r1), Operand::Reg(r2)) => enc_mr(0x89, Mode::Reg, r2, r1),
                (Operand::Ind(r1), Operand::Reg(r2)) => enc_mr(0x89, Mode::Ind, r2, r1),
                (Operand::IndDisp(r1, disp), Operand::Reg(r2)) => {
                    enc_mr_disp(0x89, Mode::from_disp(disp.clone()), r2, r1, disp)
                }
                // MOV r64,m64
                (Operand::Reg(r1), Operand::Ind(r2)) => enc_mr(0x8b, Mode::Ind, r1, r2),
                (Operand::Reg(r1), Operand::IndDisp(r2, disp)) => {
                    enc_mr_disp(0x8b, Mode::from_disp(disp.clone()), r1, r2, disp)
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

        Inst::Imull(op1, op2) => {
            // IMUL r32, r/m32: r32 <- r32 * r/m32
            let mut ts = TokenStream::new();
            match (op1, op2) {
                (Operand::Reg(r1), Operand::Ind(r2)) => {
                    ts.extend(quote!(
                        jit.emitb(0x0f);
                        jit.emitb(0xaf);
                    ));
                    ts.extend(modrm(Mode::Ind, r1, r2));
                }
                (Operand::Reg(r1), Operand::IndDisp(r2, offset)) => {
                    ts.extend(quote!(
                        jit.emitb(0x0f);
                        jit.emitb(0xaf);
                    ));
                    ts.extend(modrm(Mode::InD32, r1, r2));
                    match offset {
                        Imm::Imm(i) => ts.extend(quote!(
                            jit.emitl(#i as u32);
                        )),
                        Imm::Expr(expr) => ts.extend(quote!(
                            jit.emitl((#expr) as u32);
                        )),
                    }
                }
                _ => unimplemented!(),
            }
            ts
        }

        Inst::Pushq(op) => push_pop(0x50, op),
        Inst::Popq(op) => push_pop(0x58, op),

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
                    ts.extend(quote!(
                        jit.emitb(0xe8);
                        jit.save_reloc(#dest, 4);
                        jit.emitl(0);
                    ));
                }
            }
            ts
        }
        Inst::Ret => quote!( jit.emitb(0xc3); ),
        Inst::Jmp(dest) => {
            let mut ts = TokenStream::new();
            match dest {
                Dest::Rel(dest) => {
                    ts.extend(quote!(
                        jit.emitb(0xe9);
                        jit.save_reloc(#dest, 4);
                        jit.emitl(0);
                    ));
                }
                _ => unimplemented!(),
            }
            ts
        }
        Inst::Jne(dest) => quote!(
            jit.emitb(0x0f);
            jit.emitb(0x85);
            jit.save_reloc(#dest, 4);
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
    enc_mr(op, mode, Reg::Rax, reg_rm)
}

/// Encoding: MR or RM
fn enc_mr(op: u8, mode: Mode, reg: Reg, reg_rm: Reg) -> TokenStream {
    // TODO: If mode != Reg and r/m == 4(rsp/r12), use SIB.
    // TODO: If mode == Ind and r/m == 5, becomes [rip + disp32].
    let rex = rexw(reg, reg_rm);
    let modrm = modrm(mode, reg, reg_rm);
    quote! {
        #rex
        jit.emitb(#op);
        #modrm
    }
}

/// Encoding: MR or RM
fn enc_mr_disp(op: u8, mode: Mode, reg: Reg, reg_rm: Reg, disp: Imm) -> TokenStream {
    // TODO: If mode != Reg and r/m == 4(rsp/r12), use SIB.
    // TODO: If mode == Ind and r/m == 5, becomes [rip + disp32].
    let enc_mr = enc_mr(op, mode, reg, reg_rm);
    let disp = match disp {
        Imm::Imm(i) if mode == Mode::InD8 => {
            quote!( jit.emitb(#i as i8 as u8); )
        }
        Imm::Imm(i) if mode == Mode::InD32 => {
            quote!( jit.emitl(#i as u32); )
        }
        Imm::Expr(expr) => quote!( jit.emitl((#expr) as u32); ),
        _ => unimplemented!(),
    };
    quote!{
        #enc_mr
        #disp
    }
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
    match (op1.clone(), op2.clone()) {
        // ADD r/m64, imm32
        (Operand::Reg(ref r), Operand::Imm(i))
        | (Operand::Ind(ref r), Operand::Imm(i))
        | (Operand::IndDisp(ref r, _), Operand::Imm(i)) => {
            if i > 0xffff_ffff {
                panic!("'XXX r/m64, imm64' does not exists.");
            }
            let rex = rexw(Reg::Rax, *r);
            let modrm = match op1 {
                Operand::Reg(r) => modrm_digit(Mode::Reg, digit, r),
                Operand::Ind(r) => modrm_digit(Mode::Ind, digit, r),
                Operand::IndDisp(r, disp) => {
                    let mut ts = modrm_digit(Mode::InD32, digit, r);
                    match disp {
                        Imm::Imm(i) => {
                            ts.extend(quote!( jit.emitl(#i as u32); ));
                        }
                        Imm::Expr(expr) => {
                            ts.extend(quote!( jit.emitl((#expr) as u32); ));
                        }
                    }
                    ts
                }
                _ => unreachable!(),
            };
            quote!{
                #rex
                jit.emitb(#opcode_imm);
                #modrm
                jit.emitl(#i as u32);
            }
        }
        (Operand::Reg(r1), Operand::Reg(r2)) => {
            enc_mr(opcode_rm_reg, Mode::Reg, r2, r1)
        }
        _ => unimplemented!(),
    }
}

fn push_pop(opcode: u8, op: Operand) -> TokenStream {
    let mut ts = TokenStream::new();
    match op {
        Operand::Reg(r) => {
            if (r as u8) > 7 {
                ts.extend(rexw(Reg::Rax, r));
            }
            ts.extend(emitb_with_rd(opcode, r));
        }
        _ => unimplemented!(),
    }
    ts
}
