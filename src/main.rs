#![allow(dead_code)]
extern crate libc;
use std::mem;
use std::ops::{Index, IndexMut};

const PAGE_SIZE: usize = 4096;

#[derive(Copy, Clone)]
enum Or {
    Imm(u64),
    Reg(Reg),
    Ind(Reg),
}

#[derive(Copy, Clone)]
enum Reg {
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

#[derive(Copy, Clone)]
enum Mode {
    Ind = 0,   // (rax)
    InD8 = 1,  // (rax + disp8)
    InD32 = 2, // (rax + disp32)
    Reg = 3,   // rax
}

struct Label(usize);

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
struct JitMemory {
    contents: *mut u8,
    counter: usize,
    label_count: usize,
    reloc: Vec<Reloc>,
}

impl JitMemory {
    fn new() -> JitMemory {
        let contents: *mut u8;
        let size = 4096;
        unsafe {
            let mut page = mem::uninitialized();
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

    fn get_mem_addr(&self) -> u64 {
        self.contents as u64
    }

    fn label(&mut self) -> Label {
        let label = Label(self.label_count);
        self.label_count += 1;
        self.reloc.push(Reloc::new());
        label
    }

    fn finalize(&mut self) {
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
    fn movq(&mut self, op1: Or, op2: Or) {
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
                self.enc_mr(0x89, Mode::Reg, r2, r1);
            }
            (Or::Ind(r1), Or::Reg(r2)) => {
                self.enc_mr(0x89, Mode::Ind, r2, r1);
            }
            // MOV r64,m64
            (Or::Reg(r1), Or::Ind(r2)) => {
                self.enc_mr(0x8b, Mode::Ind, r1, r2);
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
        self.enc_mr(op, mode, Reg::Rax, reg_rm);
    }

    /// Encoding: MR or RM
    #[inline]
    fn enc_mr(&mut self, op: u8, mode: Mode, reg: Reg, reg_rm: Reg) {
        // TODO: If mode != Reg and r/m == 4(rsp/r12), use SIB.
        // TODO: If mode == Ind and r/m == 5, becomes [rip + disp32].
        self.rexw(reg, reg_rm);
        self.emitb(op);
        self.modrm(mode, reg, reg_rm);
    }
}

impl JitMemory {
    fn call(&mut self, op: Or) {
        match op {
            Or::Reg(r) => {
                self.emitb(0xff);
                self.modrm_digit(Mode::Reg, 2, r);
            }
            _ => unimplemented!(),
        }
    }

    fn ret(&mut self) {
        self.emitb(0xc3);
    }

    fn q_rm64_imm32(&mut self, opcode_imm: u8, digit: u8, op1: Or, op2: Or) {
        match (op1, op2) {
            // ADD r/m64, imm32
            (Or::Reg(r), Or::Imm(i)) | (Or::Ind(r), Or::Imm(i)) => {
                if i > 0xffff_ffff {
                    panic!("'XXX r/m64, imm64' does not exists.");
                }
                self.rexw(Reg::Rax, r);
                self.emitb(opcode_imm);
                match op1 {
                    Or::Reg(r) => self.modrm_digit(Mode::Reg, digit, r),
                    Or::Ind(r) => self.modrm_digit(Mode::Ind, digit, r),
                    _ => unimplemented!(),
                }
                self.emitl(i as u32);
            }
            _ => unimplemented!(),
        }
    }

    #[inline]
    fn addq(&mut self, op1: Or, op2: Or) {
        self.q_rm64_imm32(0x81, 0, op1, op2);
    }

    #[inline]
    fn orq(&mut self, op1: Or, op2: Or) {
        self.q_rm64_imm32(0x81, 1, op1, op2);
    }

    #[inline]
    fn adcq(&mut self, op1: Or, op2: Or) {
        self.q_rm64_imm32(0x81, 2, op1, op2);
    }

    #[inline]
    fn sbbq(&mut self, op1: Or, op2: Or) {
        self.q_rm64_imm32(0x81, 3, op1, op2);
    }

    #[inline]
    fn andq(&mut self, op1: Or, op2: Or) {
        self.q_rm64_imm32(0x81, 4, op1, op2);
    }

    #[inline]
    fn subq(&mut self, op1: Or, op2: Or) {
        self.q_rm64_imm32(0x81, 5, op1, op2);
    }

    #[inline]
    fn xorq(&mut self, op1: Or, op2: Or) {
        self.q_rm64_imm32(0x81, 6, op1, op2);
    }

    #[inline]
    fn cmpq(&mut self, op1: Or, op2: Or) {
        self.q_rm64_imm32(0x81, 7, op1, op2);
    }

    fn jmp(&mut self, dest: Or) {
        match dest {
            Or::Imm(disp) => {
                self.emitb(0xe9);
                self.emitl((disp as i64 - self.counter as i64 - 4) as u32);
            }
            _ => unimplemented!(),
        }
    }

    fn jne(&mut self, dest: Label) {
        self.emitb(0x0f);
        self.emitb(0x85);
        self.reloc[dest.0].disp.push((4, self.counter));
        self.emitl(0);
    }

    fn syscall(&mut self) {
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

fn jit() -> (fn() -> i64) {
    let hello = "Hello World! Are you angry?\n";
    let mut jit: JitMemory = JitMemory::new();

    jit.movq(Or::Reg(Reg::Rdi), Or::Imm(1));
    jit.movq(Or::Reg(Reg::R8), Or::Imm(jit.get_mem_addr() + 256));
    jit.movq(Or::Reg(Reg::Rax), Or::Imm(hello.as_ptr() as u64));
    jit.movq(Or::Ind(Reg::R8), Or::Reg(Reg::Rax));
    jit.movq(Or::Reg(Reg::Rsi), Or::Ind(Reg::R8));
    jit.movq(Or::Reg(Reg::Rdx), Or::Imm(hello.len() as u64));
    jit.movq(Or::Reg(Reg::Rax), Or::Imm(1));
    jit.syscall();

    jit.movq(Or::Reg(Reg::R15), Or::Imm(jit.contents as u64 + 256));
    jit.movq(Or::Ind(Reg::R15), Or::Imm(0x40));
    let label = jit.label();
    jit.reloc[label.0].loc = Some(jit.counter);
    let putchar_addr = libc::putchar as *const u8 as u64;
    jit.addq(Or::Ind(Reg::R15), Or::Imm(1));
    jit.movq(Or::Reg(Reg::Rax), Or::Imm(putchar_addr));
    jit.movq(Or::Reg(Reg::Rdi), Or::Ind(Reg::R15));
    jit.call(Or::Reg(Reg::Rax));

    jit.cmpq(Or::Ind(Reg::R15), Or::Imm(0x60));
    jit.jne(label);

    jit.movq(Or::Reg(Reg::Rax), Or::Imm(putchar_addr));
    jit.movq(Or::Reg(Reg::Rdi), Or::Imm('\n' as u64));
    jit.call(Or::Reg(Reg::Rax));

    jit.ret();
    jit.finalize();
    //jit.p();
    unsafe { mem::transmute(jit.contents) }
}

fn main() {
    let func = jit();
    let ret = func();
    println!("returned value:{}", ret);
}
