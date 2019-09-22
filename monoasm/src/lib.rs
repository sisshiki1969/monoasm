#![allow(dead_code)]
extern crate libc;
use std::mem;
use std::ops::{Index, IndexMut};

const PAGE_SIZE: usize = 4096;

#[derive(Copy, Clone, PartialEq)]
pub enum Or {
    Imm(u64),
    Reg(Reg),
    Ind(Reg),
    IndD8(Reg, i8),
    IndD32(Reg, i32),
}

#[derive(Copy, Clone, PartialEq)]
pub enum Dest {
    Reg(Reg),
    Rel(Label),
}

#[derive(Copy, Clone, PartialEq)]
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

#[derive(Copy, Clone, PartialEq)]
pub enum Mode {
    Ind = 0,   // (rax)
    InD8 = 1,  // (rax + disp8)
    InD32 = 2, // (rax + disp32)
    Reg = 3,   // rax
}

#[derive(Copy, Clone, PartialEq)]
pub struct Label(usize);

struct Reloc {
    loc: Option<usize>,
    disp: Vec<(u8, usize)>, //(size_in_bytes, pos_in_contents)
}

impl Reloc {
    fn new() -> Reloc {
        Reloc {
            loc: None,
            disp: vec![],
        }
    }
}

/// http://www.jonathanturner.org/2015/12/building-a-simple-jit-in-rust.html
pub struct JitMemory {
    contents: *mut u8,
    counter: usize,
    label_count: usize,
    reloc: Vec<Reloc>,
}

impl JitMemory {
    pub fn new() -> JitMemory {
        let contents: *mut u8;
        let size = 4096;
        unsafe {
            let mut page = mem::MaybeUninit::uninit().assume_init();
            libc::posix_memalign(&mut page, PAGE_SIZE, size);
            libc::mprotect(
                page,
                size,
                libc::PROT_EXEC | libc::PROT_READ | libc::PROT_WRITE,
            );
            contents = mem::transmute(page);
        }
        JitMemory {
            contents,
            counter: 0,
            label_count: 0,
            reloc: vec![],
        }
    }

    fn p(&self) {
        for i in 0..self.counter {
            print!("{:>02x} ", self[i]);
        }
        print!("\n");
    }

    pub fn get_mem_addr(&self) -> u64 {
        self.contents as u64
    }

    pub fn label(&mut self) -> Label {
        let label = Label(self.label_count);
        self.label_count += 1;
        self.reloc.push(Reloc::new());
        label
    }

    pub fn bind_label(&mut self, label: Label) {
        self.reloc[label.0].loc = Some(self.counter);
    }

    pub fn finalize(&mut self) -> (fn() -> i64) {
        let mut relocs: Vec<(usize, i32)> = vec![];
        for rel in &mut self.reloc {
            let pos = rel.loc.unwrap();
            for (size, dest) in &mut rel.disp {
                let disp = pos as i32 - *dest as i32 - *size as i32;
                relocs.push((*dest, disp));
            }
        }
        for (dest, disp) in relocs {
            self.write32(dest, disp);
        }
        unsafe { mem::transmute(self.contents) }
    }

    fn emitb(&mut self, val: u8) {
        let c = self.counter;
        self[c] = val;
        self.counter = c + 1;
    }

    fn emitb_with_rd(&mut self, val: u8, r: Reg) {
        let c = self.counter;
        self[c] = val | ((r as u8) & 0b0111);
        self.counter = c + 1;
    }

    fn emitw(&mut self, val: u16) {
        let c = self.counter;
        self[c] = val as u8;
        self[c + 1] = (val >> 8) as u8;
        self.counter = c + 2;
    }

    fn emitl(&mut self, val: u32) {
        let c = self.counter;
        self[c] = val as u8;
        self[c + 1] = (val >> 8) as u8;
        self[c + 2] = (val >> 16) as u8;
        self[c + 3] = (val >> 24) as u8;
        self.counter = c + 4;
    }

    fn write32(&mut self, loc: usize, val: i32) {
        let val = val as u32;
        self[loc] = val as u8;
        self[loc + 1] = (val >> 8) as u8;
        self[loc + 2] = (val >> 16) as u8;
        self[loc + 3] = (val >> 24) as u8;
    }

    fn emitq(&mut self, val: u64) {
        self.emitl(val as u32);
        self.emitl((val >> 32) as u32);
    }

    fn modrm_digit(&mut self, mode: Mode, digit: u8, rm: Reg) {
        let modrm = (mode as u8) << 6 | (digit & 0b111) << 3 | (rm as u8) & 0b111;
        self.emitb(modrm);
    }

    fn modrm(&mut self, mode: Mode, reg: Reg, rm: Reg) {
        let modrm = (mode as u8) << 6 | ((reg as u8) & 0b111) << 3 | (rm as u8) & 0b111;
        self.emitb(modrm);
    }

    fn rexw(&mut self, rex_r: Reg, rex_b: Reg) {
        let rex_prefix =
            0x48 | ((rex_r as u8) & 0b0000_1000) >> 1 | ((rex_b as u8) & 0b0000_1000) >> 3;
        self.emitb(rex_prefix);
    }
}

impl JitMemory {
    pub fn movq(&mut self, op1: Or, op2: Or) {
        match (op1, op2) {
            (Or::Imm(_), _) => panic!("Invalid op: moveq Imm, _"),
            // MOV r/m64, imm32
            (Or::Reg(r), Or::Imm(i)) if i <= 0xffff_ffff => {
                self.enc_m(0xc7, Mode::Reg, r);
                self.emitl(i as u32);
            }
            (Or::Ind(r), Or::Imm(i)) if i <= 0xffff_ffff => {
                self.enc_m(0xc7, Mode::Ind, r);
                self.emitl(i as u32);
            }
            // MOV r64, imm64
            (Or::Reg(r), Or::Imm(i)) => {
                self.enc_o(0xb8, r);
                self.emitq(i);
            }
            // MOV r/m64,r64
            (Or::Reg(r1), Or::Reg(r2)) => {
                self.enc_mr(0x89, Mode::Reg, r2, r1, 0);
            }
            (Or::Ind(r1), Or::Reg(r2)) => {
                self.enc_mr(0x89, Mode::Ind, r2, r1, 0);
            }
            (Or::IndD8(r1, disp), Or::Reg(r2)) => {
                self.enc_mr(0x89, Mode::InD8, r2, r1, disp as i32);
            }
            (Or::IndD32(r1, disp), Or::Reg(r2)) => {
                self.enc_mr(0x89, Mode::InD32, r2, r1, disp);
            }
            // MOV r64,m64
            (Or::Reg(r1), Or::Ind(r2)) => {
                self.enc_mr(0x8b, Mode::Ind, r1, r2, 0);
            }
            (Or::Reg(r1), Or::IndD8(r2, disp)) => {
                self.enc_mr(0x8b, Mode::InD8, r1, r2, disp as i32);
            }
            (Or::Reg(r1), Or::IndD32(r2, disp)) => {
                self.enc_mr(0x8b, Mode::InD32, r1, r2, disp);
            }
            _ => unimplemented!(),
        }
    }

    /// Encoding: Opcode + rd
    #[inline]
    fn enc_o(&mut self, op: u8, reg: Reg) {
        self.rexw(Reg::Rax, reg);
        self.emitb_with_rd(op, reg);
    }

    /// Encoding: M
    #[inline]
    fn enc_m(&mut self, op: u8, mode: Mode, reg_rm: Reg) {
        self.enc_mr(op, mode, Reg::Rax, reg_rm, 0);
    }

    /// Encoding: MR or RM
    fn enc_mr(&mut self, op: u8, mode: Mode, reg: Reg, reg_rm: Reg, disp: i32) {
        // TODO: If mode != Reg and r/m == 4(rsp/r12), use SIB.
        // TODO: If mode == Ind and r/m == 5, becomes [rip + disp32].
        self.rexw(reg, reg_rm);
        self.emitb(op);
        self.modrm(mode, reg, reg_rm);
        if mode == Mode::InD8 {
            self.emitb(disp as i8 as u8);
        } else if mode == Mode::InD32 {
            self.emitl(disp as u32);
        }
    }
}

impl JitMemory {
    pub fn call(&mut self, dest: Dest) {
        match dest {
            Dest::Reg(r) => {
                if r as u8 > 7 {
                    panic!("Can not CALL R8-R15");
                }
                self.emitb(0xff);
                self.modrm_digit(Mode::Reg, 2, r);
            }
            Dest::Rel(dest) => {
                self.emitb(0xe8);
                self.reloc[dest.0].disp.push((4, self.counter));
                self.emitl(0);
            }
        }
    }

    #[inline]
    pub fn ret(&mut self) {
        self.emitb(0xc3);
    }

    pub fn pushq(&mut self, r: Reg) {
        //self.emitb(0xff);
        //self.modrm_digit(Mode::Reg, 6, r);
        if (r as u8) > 7 {
            self.rexw(Reg::Rax, r);
        }
        self.emitb_with_rd(0x50, r);
    }

    pub fn popq(&mut self, r: Reg) {
        //self.emitb(0x8f);
        //self.modrm_digit(Mode::Reg, 0, r);
        if (r as u8) > 7 {
            self.rexw(Reg::Rax, r);
        }
        self.emitb_with_rd(0x58, r);
    }

    fn binary_op(&mut self, opcode_imm: u8, opcode_rm_reg: u8, digit: u8, op1: Or, op2: Or) {
        match (op1, op2) {
            // ADD r/m64, imm32
            (Or::Reg(r), Or::Imm(i))
            | (Or::Ind(r), Or::Imm(i))
            | (Or::IndD8(r, _), Or::Imm(i))
            | (Or::IndD32(r, _), Or::Imm(i)) => {
                if i > 0xffff_ffff {
                    panic!("'XXX r/m64, imm64' does not exists.");
                }
                self.rexw(Reg::Rax, r);
                self.emitb(opcode_imm);
                match op1 {
                    Or::Reg(r) => self.modrm_digit(Mode::Reg, digit, r),
                    Or::Ind(r) => self.modrm_digit(Mode::Ind, digit, r),
                    Or::IndD8(r, disp) => {
                        self.modrm_digit(Mode::InD8, digit, r);
                        self.emitb(disp as i8 as u8);
                    }
                    Or::IndD32(r, disp) => {
                        self.modrm_digit(Mode::InD32, digit, r);
                        self.emitl(disp as u32);
                    }
                    _ => unreachable!(),
                }
                self.emitl(i as u32);
            }
            (Or::Reg(r1), Or::Reg(r2)) => {
                self.enc_mr(opcode_rm_reg, Mode::Reg, r2, r1, 0);
            }
            _ => unimplemented!(),
        }
    }

    #[inline]
    pub fn addq(&mut self, op1: Or, op2: Or) {
        self.binary_op(0x81, 0x01, 0, op1, op2);
    }

    #[inline]
    pub fn orq(&mut self, op1: Or, op2: Or) {
        self.binary_op(0x81, 0x09, 1, op1, op2);
    }

    #[inline]
    pub fn adcq(&mut self, op1: Or, op2: Or) {
        self.binary_op(0x81, 0x11, 2, op1, op2);
    }

    #[inline]
    pub fn sbbq(&mut self, op1: Or, op2: Or) {
        self.binary_op(0x81, 0x19, 3, op1, op2);
    }

    #[inline]
    pub fn andq(&mut self, op1: Or, op2: Or) {
        self.binary_op(0x81, 0x21, 4, op1, op2);
    }

    #[inline]
    pub fn subq(&mut self, op1: Or, op2: Or) {
        self.binary_op(0x81, 0x29, 5, op1, op2);
    }

    #[inline]
    pub fn xorq(&mut self, op1: Or, op2: Or) {
        self.binary_op(0x81, 0x31, 6, op1, op2);
    }

    #[inline]
    pub fn cmpq(&mut self, op1: Or, op2: Or) {
        self.binary_op(0x81, 0x39, 7, op1, op2);
    }

    // IMUL r/m64: RDX:RAX <- RAX * r/m64
    pub fn imulq(&mut self, op: Or) {
        match op {
            Or::Reg(r) => {
                self.rexw(Reg::Rax, r);
                self.emitb(0xf7);
                self.modrm_digit(Mode::Reg, 5, r)
            }
            _ => unimplemented!(),
        }
    }

    // IMUL r32, r/m32: r32 <- r32 * r/m32
    pub fn imull(&mut self, op1: Or, op2: Or) {
        match (op1, op2) {
            (Or::Reg(r1), Or::Ind(r2)) => {
                self.emitb(0x0f);
                self.emitb(0xaf);
                self.modrm(Mode::Ind, r1, r2);
            }
            (Or::Reg(r1), Or::IndD8(r2, disp)) => {
                self.emitb(0x0f);
                self.emitb(0xaf);
                self.modrm(Mode::InD8, r1, r2);
                self.emitb(disp as u8);
            }
            _ => unimplemented!(),
        }
    }

    pub fn jmp(&mut self, dest: Label) {
        self.emitb(0xe9);
        self.reloc[dest.0].disp.push((4, self.counter));
        self.emitl(0);
    }

    pub fn jne(&mut self, dest: Label) {
        self.emitb(0x0f);
        self.emitb(0x85);
        self.reloc[dest.0].disp.push((4, self.counter));
        self.emitl(0);
    }

    pub fn syscall(&mut self) {
        self.emitb(0x0f);
        self.emitb(0x05);
    }
}

impl Index<usize> for JitMemory {
    type Output = u8;

    fn index(&self, index: usize) -> &u8 {
        unsafe { &*self.contents.offset(index as isize) }
    }
}

impl IndexMut<usize> for JitMemory {
    fn index_mut(&mut self, index: usize) -> &mut u8 {
        unsafe { &mut *self.contents.offset(index as isize) }
    }
}
