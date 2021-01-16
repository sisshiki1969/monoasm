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
                    let disp = imm_to_ts(disp, mode);
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
            dest => unimplemented!("call {:?}", dest),
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
            dest => unimplemented!("jmp {:?}", dest),
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

fn op_to_rm(op: Operand) -> (Mode, Reg, Option<Imm>) {
    match op {
        Operand::Reg(r) => (Mode::Reg, r, None),
        Operand::Ind(r, None) => (Mode::Ind, r, None),
        Operand::Ind(r, Some(d)) => (
            match d {
                Imm::Imm(i) => {
                    if std::i8::MIN as i32 <= i && i <= std::i8::MAX as i32 {
                        Mode::InD8
                    } else {
                        Mode::InD32
                    }
                }
                Imm::Expr(_) => Mode::InD32,
            },
            r,
            Some(d),
        ),
        rm_op => unreachable!("as_rm():{}", rm_op),
    }
}

fn imm_to_ts(imm: Option<Imm>, mode: Mode) -> TokenStream {
    match imm {
        Some(imm) => match imm {
            Imm::Imm(i) => match mode {
                Mode::InD8 => quote!( jit.emitb(#i as i8 as u8); ),
                Mode::InD32 => quote!( jit.emitl(#i as u32); ),
                _ => unreachable!(),
            },
            Imm::Expr(expr) => {
                quote!( jit.emitl((#expr) as u32); )
            }
        },
        None => quote!(),
    }
}

/// Encoding: Opcode + rd  
/// REX.W Op+ rd
fn enc_o(op: u8, reg: Reg) -> TokenStream {
    let mut ts = TokenStream::new();
    ts.extend(rexw(Reg::none(), reg, Reg::none()));
    ts.extend(op_with_rd(op, reg));
    ts
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
    let disp = imm_to_ts(disp, mode);
    quote! {
        #enc_mr
        #disp
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

/// ModRM
/// +-------+---+---+---+---+---+---+---+---+
/// |  bit  | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
/// +-------+---+---+---+---+---+---+---+---+
/// | field |  mod  |    reg    |    r/m    |
/// +-------+-------+-----------+-----------+
/// |  rex  |       |     r     |     b     |
/// +-------+-------+-----------+-----------+
fn modrm_digit(digit: u8, mode: Mode, rm: Reg) -> TokenStream {
    let modrm = (mode as u8) << 6 | (digit & 0b111) << 3 | (rm as u8) & 0b111;
    quote!( jit.emitb(#modrm); )
}

fn modrm(reg: Reg, mode: Mode, rm: Reg) -> TokenStream {
    let modrm = (mode as u8) << 6 | ((reg as u8) & 0b111) << 3 | (rm as u8) & 0b111;
    quote!( jit.emitb(#modrm); )
}

/// REX.W
///      bit
/// +---+---+------------------------------------------------+
/// | W | 3 | 1 = 64 bit operand size                        |
/// +---+---+------------------------------------------------+
/// | R | 2 | rex_r = ext of reg field of ModRM              |
/// +---+---+------------------------------------------------+
/// | X | 1 | rex_i = ext of index field of SIB              |
/// +---+---+------------------------------------------------+
/// | B | 0 | rex_b = ext of r/m(ModRM) or base(SIB)         |
/// |   |   |           or reg field of Op.                  |
/// +---+---+------------------------------------------------+
fn rexw(reg: Reg, base: Reg, index: Reg) -> TokenStream {
    let rex_prefix = 0x48
        | ((reg as u8) & 0b0000_1000) >> 1
        | ((index as u8) & 0b0000_1000) >> 2
        | ((base as u8) & 0b0000_1000) >> 3;
    quote!( jit.emitb(#rex_prefix); )
}

fn rex(reg: Reg, base: Reg, index: Reg) -> TokenStream {
    if base as u8 > 7 {
        let rex_prefix = 0x40
            | ((reg as u8) & 0b0000_1000) >> 1
            | ((index as u8) & 0b0000_1000) >> 2
            | ((base as u8) & 0b0000_1000) >> 3;
        quote!( jit.emitb(#rex_prefix); )
    } else {
        quote!()
    }
}

fn op_with_rd(op: u8, r: Reg) -> TokenStream {
    let val = op | ((r as u8) & 0b0111);
    quote!( jit.emitb(#val); )
}

/// Emit SIB
/// +-------+---+---+---+---+---+---+---+---+
/// |  bit  | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
/// +-------+---+---+---+---+---+---+---+---+
/// | field | scale |   index   |    base   |
/// +-------+-------+-----------+-----------+
/// |  rex  |       |     x     |     b     |
/// +-------+-------+-----------+-----------+
///
/// scale: 00|01|10|11
///  mul : na| 2| 4| 8
///
/// index: register number (with rex.x)
///
/// base: register number (with rex.b)
///     rex.b:0 base:101 => use RBP  mode:00/disp32 01/RBP+disp8 10/RBP+disp32
///     rex.b:1 base:101 => use R13  mode:00/disp32 01/R13+disp8 10/R13+disp32
///
fn sib(scale: u8, index: Reg, base: u8) -> TokenStream {
    assert!(scale < 4);
    assert!((index as u8) < 8);
    assert!(base < 8);
    let sib = scale << 6 | (index as u8) << 3 | base;
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
            let op_0 = enc_o(0xb8, r); // MOV r64, imm64
            let op_1 = enc_mi(0xc7, Operand::Reg(r)); // MOV r/m64, imm32
            quote! {
                let imm = (#i) as u64;
                if  imm <= 0xffff_ffff {
                    #op_1
                    jit.emitl(imm as u32);
                } else {
                    #op_0
                    jit.emitq(imm);
                }
            }
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
        (op1, Operand::Reg(r2)) => enc_mr(0x89, r2, op1),
        // MOV r64,m64
        // REX.W + 8B /r
        // RM
        (Operand::Reg(r1), op2) => enc_mr(0x8b, r1, op2),
        (op1, op2) => unimplemented!("movq {:?}, {:?}", op1, op2),
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
            let disp = imm_to_ts(disp, mode);
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
        (op1, Operand::Reg(r2)) => enc_mr(op_mr, r2, op1),
        // OP r64, m64
        // REX.W op_rm /r
        // RM
        (Operand::Reg(r1), op2) => enc_mr(op_rm, r1, op2),
        (op1, op2) => unimplemented!("XXX {:?}, {:?}", op1, op2),
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
