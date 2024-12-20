# monoasm

monoasm is a x86-64 run-time code generator engine written in Rust.

This module generates x86-64 machine codes on memory and you can execute them as a Rust function.

You can write x64 assembly in your Rust code using intel-like syntax with the aid of procedual macro, which is a powerful feature of Rust 2018.
The assembly code is assembled in compile time, and embedded as a code generator for x64 assembly.

## operand syntax

|      addressing mode       |                       syntax                      |
| :------------------------: | :-----------------------------------------------: |
|      register direct       |         rax, R(_expr_), xmm0, xmm(_expr_)         |
|          indirect          |                 [rax], [R(_expr_)]                |
| indirect with displacement |   [rax + 100], [rax + (_expr_)], [rip + _label_]  |
|    scaled index indirect   | [rax + rdi], [rax + rdi * 4], [rax + rdi * 2 - 5] |

- You can write Rust expression in parenthesis as an immediate value.

```Rust
    let mut jit: JitMemory = JitMemory::new();
    let a = 100;

    monoasm!{ &mut jit,
        movq rax, (a * 10);
    };
```

- You can use _DestLabel_ for jump destination operand.

```Rust
    let mut jit: JitMemory = JitMemory::new();
    let label: DestLabel = jit.label();
    monoasm!( &mut jit,
        cmpq rax, 10;
        jne label;
    label:
        movq rax, 10;
        ret;
    );
```

## supported instructions

### general register operations

#### quad word operations

- movq
- lea

- addq
- orq
- adcq
- sbbq
- andq
- subq
- xorq
- cmpq
- xchgq

- negq

- imul
- idiv
- div
- cqo

- shlq
- shrq
- salq
- sarq
- rolq
- rorq

- testq

- lzcntq
- tzcntq
- popcntq

- seteq
- setne
- setgt
- setge
- setlt
- setle
- seta
- setae
- setb
- setbe

#### double word operations

- movl
- movsxl

- addl
- orl
- adcl
- sbbl
- andl
- subl
- xorl
- cmpl
- xchgl

#### word operations

- movw
- movzxw
- movsxw

- addw
- orw
- adcw
- sbbw
- andw
- subw
- xorw
- cmpw
- xchgw

#### byte operations

- movb
- movzxb
- movsxb

- addb
- orb
- adcb
- sbbb
- andb
- subb
- xorb
- cmpb
- xchgb

- testb

### conditional move operations

- cmovbq
- cmovaeq
- cmoveqq
- cmovneq
- cmovbeq
- cmovaq
- cmovsq
- cmovnsq
- cmovltq
- cmovgeq
- cmovleq
- cmovgtq

### floating point operations

- movsd
- addsd
- subsd
- mulsd
- divsd
- xorps
- cvtsi2sdq
- ucomisd

- sqrtpd
- sqrtsd

### stack operations

- pushq
- popq

### call/branch

- call
- ret
- jmp
- jne
- jeq
- jgt
- jge
- jlt
- jle
- ja
- jae
- jb
- jbe

### misc

- syscall
- leave
- int3

### constant data

- dq

```Rust
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
```

Most assemblies written using `monoasm!()` macros are compiled into x86 machine code at compile time, but some codes such as immediate values or register designation using Rust variables and functions, are resolved at run time.
Here is a Rust code generated by `monoasm!()` macros.

```Rust
jit . emitb(65u8) ; jit . emitb(84u8) ;
jit . emitb(65u8) ; jit . emitb(85u8) ;
jit . emitb(65u8) ; jit . emitb(86u8) ;
jit . emitb(65u8) ; jit . emitb(87u8) ;
jit . emitb(72u8) ; jit . emitb(137u8) ; jit . emitb(189u8) ;
jit . emitl((- (x_ofs)) as u32) ;
let imm = (0) as u64 ; if imm <= 0xffff_ffff {
    jit . emitb(72u8) ; jit . emitb(199u8) ; jit . emitb(192u8) ;
    jit . emitl(imm as u32) ;
} else {
    jit . emitb(72u8) ; jit . emitb(184u8) ; jit . emitq(imm) ;
}
jit . emitb(0xe9) ; jit . save_reloc(end, 4) ; jit . emitl(0) ;
jit . bind_label(label0) ;
let imm = (1) as u64 ; if imm <= 0xffff_ffff {
    jit . emitb(72u8) ; jit . emitb(199u8) ; jit . emitb(192u8) ;
    jit . emitl(imm as u32) ;
} else {
    jit . emitb(72u8) ; jit . emitb(184u8) ; jit . emitq(imm) ;
}
jit . emitb(0xe9) ; jit . save_reloc(end, 4) ; jit . emitl(0) ;
jit . bind_label(label1) ;
jit . emitb(76u8) ; jit . emitb(137u8) ; jit . emitb(224u8) ;
jit . bind_label(end) ;
jit . emitb(65u8) ; jit . emitb(95u8) ;
jit . emitb(65u8) ; jit . emitb(94u8) ;
jit . emitb(65u8) ; jit . emitb(93u8) ;
jit . emitb(65u8) ; jit . emitb(92u8) ;
jit . emitb(85u8) ;
jit . emitb(72u8) ; jit . emitb(137u8) ; jit . emitb(229u8) ;
let imm = ((locals + locals % 2) * 8) as u64 ;
if imm > 0xffff_ffff {
    panic ! ("'{} {}, imm64' does not exists.", "SUB", "Rsp") ;
}
jit . emitb(72u8) ; jit . emitb(129u8) ; jit . emitb(236u8) ;
jit . emitl(imm as u32) ;
jit . emitb(72u8) ; jit . emitb(137u8) ; jit . emitb(236u8) ;
jit . emitb(93u8) ;
jit . emitb(0xc3) ;
let imm = (val as u64) as u64 ;
let r = Reg :: from(reg + 12 as u64) ;
let rm_op = Or :: Reg(r) ;
if imm <= 0xffff_ffff {
    jit . enc_mi(0xc7, rm_op) ; jit . emitl(imm as u32) ;
} else {
    jit . enc_o(0xb8, r) ; jit . emitq(imm) ;
} ;
let r1 = Reg :: from((reg + 12) as u64) ;
jit . enc_mr(137u8, r1, Or :: IndD32(Reg :: from(5u64), (- (offset)) as i32)) ;
let r1 = Reg :: from((reg + 12) as u64) ;
jit . enc_mr(139u8, r1, Or :: IndD32(Reg :: from(5u64), (- (offset)) as i32)) ;
let r2 = Reg :: from((src_reg + 12) as u64) ;
jit . enc_mr(41u8, r2, Or :: Reg(Reg :: from((dest_reg + 12) as u64))) ;
let r2 = Reg :: from((src_reg + 12) as u64) ;
jit . enc_mr(1u8, r2, Or :: Reg(Reg :: from((dest_reg + 12) as u64))) ;
let r2 = Reg :: from((src_reg + 12) as u64) ;
jit . enc_mr(57u8, r2, Or :: Reg(Reg :: from((dest_reg + 12) as u64))) ;
jit . emitb(0x0f) ; jit . emitb(0x85) ; jit . save_reloc(dest, 4) ;
jit . emitl(0) ;
let r1 = Reg :: from((arg0_reg + 12) as u64) ;
jit . enc_mr(137u8, r1, Or :: Reg(Reg :: from(7u64))) ;
jit . emitb(0xe8) ; jit . save_reloc(dest, 4) ; jit . emitl(0) ;
let r1 = Reg :: from((ret_reg + 12) as u64) ;
jit . enc_mr(139u8, r1, Or :: Reg(Reg :: from(0u64))) ;
let r1 = Reg :: from((reg + 12) as u64) ;
jit . enc_mr(137u8, r1, Or :: Reg(Reg :: from(7u64))) ;
let imm = (monoasm :: test :: PUTINT as u64) as u64 ;
if imm <= 0xffff_ffff {
    jit . emitb(72u8) ; jit . emitb(199u8) ; jit . emitb(192u8) ;
    jit . emitl(imm as u32) ;
} else {
    jit . emitb(72u8) ; jit . emitb(184u8) ; jit . emitq(imm) ;
}
jit . emitb(0xff) ; jit . emitb(208u8) ;
```
