#![feature(proc_macro_hygiene)]
extern crate monoasm;
extern crate monoasm_macro;
use monoasm::JitMemory;
use monoasm_macro::monoasm;

fn jit() -> fn(()) -> u64 {
    let mut vm = VM::new();
    let l0 = vm.jit.label();
    let x_ofs = 8usize;
    // %0:r12
    // %1:r13
    // %2:r14
    // rbp-8: x
    vm.prologue(1);
    monoasm!(vm.jit,
        pushq r14;
        pushq r15;
    );
    // 5 -> %0
    vm.push_val(0, 5);
    // x <- %0
    vm.set_local(0, x_ofs);
    // loop:
    vm.jit.bind_label(l0);
    // x -> %0
    vm.get_local(0, x_ofs);
    // puts %0
    vm.puts_s0();
    // x -> %0
    vm.get_local(0, x_ofs);
    // 1 -> %1
    vm.push_val(1, 1);
    // %0 <- %0 - %1
    monoasm!(vm.jit,
        movq rax, r12;
        subq rax, r13;
        movq r12, rax;
    );
    // x <- %0
    vm.set_local(0, x_ofs);
    // x -> %0
    vm.get_local(0, x_ofs);
    // 0 -> %1
    vm.push_val(1, 0);
    monoasm!(vm.jit,
    // cmp %0 <- cmp(%0,%1)
        movq rax, r12;
        cmpq rax, r13;
    // jne loop <-%0
        //cmpq [rbp-8], 1;
        jne l0;

        popq r15;
        popq r14;
        movq rax, 0;
    );
    vm.epilogue();
    vm.jit.finalize()
}

struct VM {
    jit: JitMemory,
}

impl VM {
    fn new() -> Self {
        Self {
            jit: JitMemory::new(),
        }
    }

    fn prologue(&mut self, locals: usize) {
        monoasm!(self.jit,
            pushq rbp;
            movq rbp, rsp;
            subq rsp, ((locals + locals % 2)*8);
        );
    }

    fn epilogue(&mut self) {
        monoasm!(self.jit,
            movq rsp, rbp;
            popq rbp;
            ret;
        );
    }

    fn push_val(&mut self, reg: usize, val: i64) {
        match reg {
            0 => {
                monoasm!(self.jit,
                    movq r12, (val as u64);
                );
            }
            1 => {
                monoasm!(self.jit,
                    movq r13, (val as u64);
                );
            }
            2 => {
                monoasm!(self.jit,
                    movq r14, (val as u64);
                );
            }
            _ => unimplemented!(),
        };
    }

    fn set_local(&mut self, reg: usize, offset: usize) {
        match reg {
            0 => {
                monoasm!(self.jit,
                    movq [rbp-(offset as i64)], r12;
                );
            }
            _ => unimplemented!(),
        }
    }

    fn get_local(&mut self, reg: usize, offset: usize) {
        match reg {
            0 => {
                monoasm!(self.jit,
                    movq r12, [rbp-(offset as i64)];
                );
            }
            _ => unimplemented!(),
        }
    }

    fn puts_s0(&mut self) {
        monoasm!(self.jit,
        // puts <-%0
            movq rdi, r12;
            movq rax, (monoasm::test::PUTINT as u64);
            call rax;
        );
    }
}

fn main() {
    let func = jit();
    let ret = func(());
    println!("returned value:{}", ret);
}
