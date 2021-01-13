use super::inst::*;
use proc_macro2::TokenStream;
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
                // REX.W + C7 /0 id
                // MI
                (Operand::Reg(r), Operand::Imm(i)) if i <= 0xffff_ffff => {
                    let op = enc_m(0xc7, Mode::Reg, r);
                    quote! {
                        #op
                        jit.emitl(#i as u32);
                    }
                }
                (Operand::Ind(r), Operand::Imm(i)) if i <= 0xffff_ffff => {
                    let op = enc_m(0xc7, Mode::Ind, r);
                    quote! {
                        #op
                        jit.emitl(#i as u32);
                    }
                }
                // MOV r64, imm64
                // REX.W + B8+ rd io
                // OI
                (Operand::Reg(r), Operand::Imm(i)) => {
                    let op = enc_o(0xb8, r);
                    quote! {
                        #op
                        jit.emitq(#i);
                    }
                }
                (Operand::Reg(r), Operand::Expr(expr)) => {
                    let op = enc_o(0xb8, r);
                    quote! {
                        #op
                        jit.emitq(#expr);
                    }
                }
                // MOV r/m64,r64
                // REX.W + 89 /r
                // MR
                (Operand::Reg(r1), Operand::Reg(r2)) => enc_mr(0x89, r2, Mode::Reg, r1),
                (Operand::Ind(r1), Operand::Reg(r2)) => enc_mr(0x89, r2, Mode::Ind, r1),
                (Operand::IndDisp(r1, disp), Operand::Reg(r2)) => enc_mr_disp(0x89, r2, r1, disp),
                // MOV r64,m64
                // REX.W + 8B /r
                // RM
                (Operand::Reg(r1), Operand::Ind(r2)) => enc_mr(0x8b, r1, Mode::Ind, r2),
                (Operand::Reg(r1), Operand::IndDisp(r2, disp)) => enc_mr_disp(0x8b, r1, r2, disp),
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
            match (op1, op2) {
                // IMUL r32, r/m32
                // RM
                (Operand::Reg(r1), Operand::Ind(r2)) => {
                    let modrm = modrm(Mode::Ind, r1, r2);
                    quote! {
                        jit.emitb(0x0f);
                        jit.emitb(0xaf);
                        #modrm
                    }
                }
                // IMUL r32, r/m32
                // RM
                (Operand::Reg(r1), Operand::IndDisp(r2, offset)) => {
                    let modrm = modrm(Mode::InD32, r1, r2);
                    let offset = match offset {
                        Imm::Imm(i) => quote!(
                            jit.emitl(#i as u32);
                        ),
                        Imm::Expr(expr) => quote!(
                            jit.emitl((#expr) as u32);
                        ),
                    };
                    quote! {
                        jit.emitb(0x0f);
                        jit.emitb(0xaf);
                        #modrm
                        #offset
                    }
                }
                _ => unimplemented!(),
            }
        }

        Inst::Pushq(op) => push_pop(0x50, op),
        Inst::Popq(op) => push_pop(0x58, op),

        Inst::Call(dest) => match dest {
            Dest::Reg(r) => {
                if r as u8 > 7 {
                    panic!("Can not CALL R8-R15");
                }
                let modrm = modrm_digit(Mode::Reg, 2, r);
                quote! {
                    jit.emitb(0xff);
                    #modrm
                }
            }
            Dest::Rel(dest) => {
                quote! {
                    jit.emitb(0xe8);
                    jit.save_reloc(#dest, 4);
                    jit.emitl(0);
                }
            }
        },
        Inst::Ret => quote!( jit.emitb(0xc3); ),
        Inst::Jmp(dest) => match dest {
            Dest::Rel(dest) => {
                quote! {
                    jit.emitb(0xe9);
                    jit.save_reloc(#dest, 4);
                    jit.emitl(0);
                }
            }
            _ => unimplemented!(),
        },
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
/// REX.W Op+ rd
fn enc_o(op: u8, reg: Reg) -> TokenStream {
    let mut ts = TokenStream::new();
    ts.extend(rexw(Reg::Rax, reg));
    ts.extend(op_with_rd(op, reg));
    ts
}

/// Encoding: M  
/// ModRM:r/m
fn enc_m(op: u8, mode: Mode, rm: Reg) -> TokenStream {
    enc_mr(op, Reg::Rax, mode, rm)
}

/// Encoding: MR or RM
/// MR-> ModRM:r/m(w) ModRM:reg(r)
/// RM-> ModRM:reg(r) ModRM:r/m(w)
fn enc_mr(op: u8, reg: Reg, mode: Mode, rm: Reg) -> TokenStream {
    // TODO: If mode != Reg and r/m == 4(rsp/r12), use SIB.
    // TODO: If mode == Ind and r/m == 5, becomes [rip + disp32].
    let rex = rexw(reg, rm);
    let modrm = modrm(mode, reg, rm);
    quote! {
        #rex
        jit.emitb(#op);
        #modrm
    }
}

/// Encoding: MR or RM
/// MR-> ModRM:r/m(w) ModRM:reg(r)
/// RM-> ModRM:reg(r) ModRM:r/m(w)
fn enc_mr_disp(op: u8, reg: Reg, rm: Reg, disp: Imm) -> TokenStream {
    // TODO: If mode != Reg and r/m == 4(rsp/r12), use SIB.
    // TODO: If mode == Ind and r/m == 5, becomes [rip + disp32].
    let mode = Mode::from_disp(disp.clone());
    let enc_mr = enc_mr(op, reg, mode, rm);
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
    quote! {
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

fn rex(rex_r: Reg, rex_b: Reg) -> TokenStream {
    let rex_prefix = 0x40 | ((rex_r as u8) & 0b0000_1000) >> 1 | ((rex_b as u8) & 0b0000_1000) >> 3;
    quote!( jit.emitb(#rex_prefix); )
}

fn op_with_rd(op: u8, r: Reg) -> TokenStream {
    let val = op | ((r as u8) & 0b0111);
    quote!( jit.emitb(#val); )
}

fn binary_op(
    opcode_imm: u8,
    opcode_rm_reg: u8,
    digit: u8,
    op1: Operand,
    op2: Operand,
) -> TokenStream {
    match (op1, op2) {
        // OP r64, imm32
        // REX.W op /0 id
        // MI
        (Operand::Reg(r), Operand::Imm(i)) => {
            if i > 0xffff_ffff {
                panic!("'XXX r/m64, imm64' does not exists.");
            }
            let rex = rexw(Reg::Rax, r);
            let modrm = modrm_digit(Mode::Reg, digit, r);
            quote! {
                #rex
                jit.emitb(#opcode_imm);
                #modrm
                jit.emitl(#i as u32);
            }
        }
        // OP m64, imm32
        // REX.W op /0 id
        (Operand::Ind(r), Operand::Imm(i)) => {
            if i > 0xffff_ffff {
                panic!("'XXX r/m64, imm64' does not exists.");
            }
            let rex = rexw(Reg::Rax, r);
            let modrm = modrm_digit(Mode::Ind, digit, r);
            quote! {
                #rex
                jit.emitb(#opcode_imm);
                #modrm
                jit.emitl(#i as u32);
            }
        }
        // OP m64, imm32
        (Operand::IndDisp(r, disp), Operand::Imm(i)) => {
            if i > 0xffff_ffff {
                panic!("'XXX r/m64, imm64' does not exists.");
            }
            let rex = rexw(Reg::Rax, r);
            let modrm = {
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
            };
            quote! {
                #rex
                jit.emitb(#opcode_imm);
                #modrm
                jit.emitl(#i as u32);
            }
        }
        (Operand::Reg(r1), Operand::Reg(r2)) => enc_mr(opcode_rm_reg, r2, Mode::Reg, r1),
        _ => unimplemented!(),
    }
}

fn push_pop(opcode: u8, op: Operand) -> TokenStream {
    let mut ts = TokenStream::new();
    match op {
        // PUSH r64     POP 64
        // 50 +rd       58 +rd
        // O            O
        Operand::Reg(r) => {
            if (r as u8) > 7 {
                ts.extend(rex(Reg::Rax, r));
            }
            ts.extend(op_with_rd(opcode, r));
        }
        _ => unimplemented!(),
    }
    ts
}
