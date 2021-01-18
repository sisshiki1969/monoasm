#![feature(proc_macro_hygiene)]
extern crate monoasm;
extern crate monoasm_macro;
use monoasm::*;
use monoasm_inst::Reg;
use monoasm_macro::monoasm;

fn main() {
    let _ = construct_ast();
    let func = jit();
    let x = 35;
    let ret = func(x);
    println!("fib( {} ) = {}", x, ret);
    assert_eq!(9227465, ret)
}

fn jit() -> fn(u64) -> u64 {
    let mut vm = VM::new();
    // Jump labels must be declared as local variable in advance.
    let fibo = vm.jit.label();
    let label0 = vm.jit.label();
    let label1 = vm.jit.label();
    let end = vm.jit.label();
    vm.jit.bind_label(fibo);
    // Negative offset to rbp for local variable 'x'.
    let x_ofs = 8;
    // working registers: %0:r12 %1:r13 %2:r14
    vm.prologue(1);
    monoasm!(vm.jit,
        // load argument to x.
        movq [rbp-(x_ofs)], rdi;
    );
    // %0 <- x
    vm.get_local(0, x_ofs);
    // %1 <- 0
    vm.set_int(1, 0);
    // cmp %0, %1
    vm.jne(label0, 0, 1);
    // %0 <- 0
    vm.set_int(0, 0);
    // return %0
    vm.leave(end, 0);
    monoasm!(vm.jit,
    label0:
    );
    // %0 <- x
    vm.get_local(0, x_ofs);
    // %1 <- 1
    vm.set_int(1, 1);
    // cmp %0, %1
    vm.jne(label1, 0, 1);
    // %0 <- 1
    vm.set_int(0, 1);
    // return %0
    vm.leave(end, 0);
    monoasm!(vm.jit,
    label1:
    );
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
    // return %0
    vm.leave(end, 0);

    monoasm!(vm.jit,
    end:
    );
    vm.epilogue();
    vm.jit.finalize()
}

struct VM {
    jit: JitMemory,
}

#[allow(dead_code)]
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
            pushq r12; pushq r13; pushq r14; pushq r15;
        );
    }

    fn epilogue(&mut self) {
        monoasm!(self.jit,
            popq r15; popq r14; popq r13; popq r12;
            movq rsp, rbp;
            popq rbp;
            ret;
        );
    }

    fn set_int(&mut self, reg: u64, val: i64) {
        monoasm!(self.jit, movq R(reg + 12), (val as u64););
    }

    fn set_local(&mut self, reg: u64, offset: i64) {
        monoasm!(self.jit, movq [rbp-(offset)], R(reg + 12););
    }

    fn get_local(&mut self, reg: u64, offset: i64) {
        monoasm!(self.jit, movq R(reg + 12), [rbp-(offset)];);
    }

    fn sub(&mut self, dest_reg: u64, src_reg: u64) {
        monoasm!(self.jit, subq R(dest_reg + 12), R(src_reg + 12););
    }

    fn add(&mut self, dest_reg: u64, src_reg: u64) {
        monoasm!(self.jit, addq R(dest_reg + 12), R(src_reg + 12););
    }

    fn jne(&mut self, dest: DestLabel, dest_reg: u64, src_reg: u64) {
        monoasm!(self.jit,
            cmpq R(dest_reg + 12), R(src_reg + 12);
            jne dest;
        );
    }

    fn call_arg1(&mut self, dest: DestLabel, arg0_reg: u64, ret_reg: u64) {
        monoasm!(self.jit,
            movq rdi, R(arg0_reg + 12);
            call dest;
            movq R(ret_reg + 12), rax;
        );
    }

    fn leave(&mut self, end: DestLabel, reg: u64) {
        monoasm!(self.jit,
            movq rax, R(reg + 12);
            jmp end;
        );
    }

    fn puts(&mut self, reg: u64) {
        monoasm!(self.jit,
            movq rdi, R(reg + 12);
            movq rax, (monoasm::test::PUTINT as u64);
            call rax;
        );
    }
}

enum Node {
    Integer(i64), // push 1
    LocalVar(u8), // push 1
    If(If),       // pop2
    Return(Box<Node>),
    Add(Box<Node>, Box<Node>), // pop2, push1
    Sub(Box<Node>, Box<Node>), // pop2, push1
    Call(String, Vec<Node>),   // popn, push1
}

impl Node {
    fn add(lhs: Node, rhs: Node) -> Self {
        Node::Add(Box::new(lhs), Box::new(rhs))
    }

    fn sub(lhs: Node, rhs: Node) -> Self {
        Node::Sub(Box::new(lhs), Box::new(rhs))
    }

    fn if_(cond: Cmp, then: Node) -> Self {
        Node::If(If::new(cond, then))
    }

    fn call(func: &str, args: Vec<Node>) -> Self {
        Node::Call(func.to_string(), args)
    }

    fn ret(val: Node) -> Self {
        Node::Return(Box::new(val))
    }
}

struct If {
    cond: Cmp,
    then: Box<Node>,
}

impl If {
    fn new(cond: Cmp, then: Node) -> Self {
        Self {
            cond,
            then: Box::new(then),
        }
    }
}

struct Cmp {
    kind: CmpKind,
    lhs: Box<Node>,
    rhs: Box<Node>,
}

impl Cmp {
    fn new(kind: CmpKind, lhs: Node, rhs: Node) -> Self {
        Self {
            kind,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
}

enum CmpKind {
    Eq,
    Ne,
}

fn construct_ast() -> Vec<Node> {
    vec![
        Node::if_(
            Cmp::new(CmpKind::Eq, Node::LocalVar(0), Node::Integer(0)),
            Node::ret(Node::Integer(0)),
        ),
        Node::if_(
            Cmp::new(CmpKind::Eq, Node::LocalVar(0), Node::Integer(1)),
            Node::ret(Node::Integer(1)),
        ),
        Node::ret(Node::add(
            Node::call("fibo", vec![Node::sub(Node::LocalVar(0), Node::Integer(1))]),
            Node::call("fibo", vec![Node::sub(Node::LocalVar(0), Node::Integer(2))]),
        )),
    ]
}
