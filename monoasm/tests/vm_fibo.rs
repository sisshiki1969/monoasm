extern crate monoasm;
extern crate monoasm_macro;
use monoasm::*;
use monoasm_macro::monoasm;

fn jit() -> extern "C" fn(u64) -> u64 {
    let mut vm = VM::new();
    // Jump labels must be declared as local variable in advance.
    let fibo = vm.jit.label();
    let label0 = vm.jit.label();
    let label1 = vm.jit.label();
    let end = vm.jit.label();
    // Entry point
    vm.jit.bind_label(fibo);
    // Negative offset to rbp for local variable 'x'.
    let x_ofs = 8;
    // Working registers: %0:r12 %1:r13 %2:r14
    vm.prologue(1);
    monoasm! {&mut vm.jit,
        pushq r12; pushq r13; pushq r14; pushq r15;
        // Load argument to x.
        // You can use Rust expression in displacement, register designation.. etc.
        // Rust expression must be wrapped with ().
        movq [rbp-(x_ofs)], rdi;
    }
    // %0 <- x
    vm.get_local(0, x_ofs);
    // %1 <- 0
    vm.set_int(1, 0);
    // cmp %0, %1
    vm.cmp(0, 1);
    vm.jne(label0);
    monoasm! {&mut vm.jit,
        movq rax, 0;
        jmp end;
    label0:
    }
    // %1 <- 1
    vm.set_int(1, 1);
    // cmp %0, %1
    vm.cmp(0, 1);
    vm.jne(label1);
    monoasm! {&mut vm.jit,
        movq rax, 1;
        jmp end;
    label1:
    }
    // %0 <- x
    vm.get_local(0, x_ofs);
    // %1 <- 1
    vm.set_int(1, 1);
    // %0 <- %0 - %1
    vm.sub(0, 1);
    // %0 <- fibo(%0)
    vm.call_arg1(fibo, 0, 0);
    // %1 <- x
    vm.get_local(1, x_ofs);
    // %2 <- 2
    vm.set_int(2, 2);
    // %1 <- %1 - %2
    vm.sub(1, 2);
    // %1 <- fibo(%1)
    vm.call_arg1(fibo, 1, 1);
    // %0 <- %0 + %1
    vm.add(0, 1);
    monoasm! {&mut vm.jit,
        movq rax, R(12);
    }

    monoasm! {&mut vm.jit,
    end:
        popq r15; popq r14; popq r13; popq r12;
    }
    vm.epilogue();
    vm.jit.finalize();
    vm.jit.get_label_addr(fibo) as _
}

struct VM {
    jit: JitMemory,
}

// Virtual machine codes
#[allow(dead_code)]
impl VM {
    fn new() -> Self {
        Self {
            jit: JitMemory::new(),
        }
    }

    fn prologue(&mut self, locals: usize) {
        monoasm! {&mut self.jit,
            pushq rbp;
            movq rbp, rsp;
            subq rsp, ((locals + locals % 2)*8);
        }
    }

    fn epilogue(&mut self) {
        monoasm! {&mut self.jit,
            movq rsp, rbp;
            popq rbp;
            ret;
        }
    }

    fn set_int(&mut self, reg: u64, val: i64) {
        // You can use Rust expression for register designation.
        // ex. R(reg + 12)
        monoasm! {&mut self.jit,
            movq R(reg + 12), (val as u64);
        }
    }

    fn set_local(&mut self, reg: u64, offset: i64) {
        monoasm! {&mut self.jit,
            movq [rbp-(offset)], R(reg + 12);
        }
    }

    fn get_local(&mut self, reg: u64, offset: i64) {
        monoasm! {&mut self.jit,
            movq R(reg + 12), [rbp-(offset)];
        }
    }

    fn sub(&mut self, dest_reg: u64, src_reg: u64) {
        monoasm! {&mut self.jit,
            subq R(dest_reg + 12), R(src_reg + 12);
        }
    }

    fn add(&mut self, dest_reg: u64, src_reg: u64) {
        monoasm! {&mut self.jit,
            addq R(dest_reg + 12), R(src_reg + 12);
        }
    }

    fn cmp(&mut self, dest_reg: u64, src_reg: u64) {
        monoasm! {&mut self.jit,
            cmpq R(dest_reg + 12), R(src_reg + 12);
        }
    }

    fn jne(&mut self, dest: DestLabel) {
        monoasm!(&mut self.jit, jne dest;);
    }

    fn call_arg1(&mut self, dest: DestLabel, arg0_reg: u64, ret_reg: u64) {
        monoasm! {&mut self.jit,
            movq rdi, R(arg0_reg + 12);
            call dest;
            movq R(ret_reg + 12), rax;
        }
    }

    fn puts(&mut self, reg: u64) {
        monoasm! {&mut self.jit,
            movq rdi, R(reg + 12);
            movq rax, (monoasm::test::PUTINT as u64);
            call rax;
        }
    }
}

#[test]
fn vm_fibo() {
    let func = jit();
    let x = 35;
    let ret = func(x);
    eprintln!("fib( {} ) = {}", x, ret);
    assert_eq!(9227465, ret)
}
