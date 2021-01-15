use proc_macro2::TokenStream;
use syn::Ident;

#[derive(Clone, Debug)]
pub struct Stmts {
    pub contents: Vec<Inst>,
}

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

    Call(Dest),
    Ret,
    Syscall,
}

#[derive(Clone, Debug)]
pub enum Operand {
    Imm(TokenStream),
    Reg(Reg),
    Ind(Reg),
    IndDisp(Reg, Imm),
}

impl std::fmt::Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::Imm(i) => write!(f, "Imm({})", i),
            Operand::Reg(r) => write!(f, "{:?}", r),
            Operand::Ind(r) => write!(f, "[{:?}]", r),
            Operand::IndDisp(r, d) => write!(f, "{}[{:?}]", d, r),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Dest {
    Reg(Reg),
    Rel(Ident),
}

#[derive(Clone, Debug)]
pub enum Imm {
    Imm(i32),
    Expr(TokenStream),
}

impl std::fmt::Display for Imm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Imm::Imm(i) => write!(f, "Imm({})", i),
            Imm::Expr(ts) => write!(f, "Expr({})", ts),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Reg {
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
    pub fn from_str(string: &str) -> Option<Reg> {
        let mut string = string.to_owned();
        string.make_ascii_lowercase();
        let reg = match string.as_str() {
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
            _ => return None,
        };
        Some(reg)
    }
}
