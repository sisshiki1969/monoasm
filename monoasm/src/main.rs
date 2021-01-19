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
    vm.get_local(x_ofs);
    // %1 <- 0
    vm.push_int(0);
    // cmp %0, %1
    vm.jne(label0);
    // %0 <- 0
    vm.push_int(0);
    // return %0
    vm.leave(end);
    monoasm!(vm.jit,
    label0:
    );
    // %0 <- x
    vm.get_local(x_ofs);
    // %1 <- 1
    vm.push_int(1);
    // cmp %0, %1
    vm.jne(label1);
    // %0 <- 1
    vm.push_int(1);
    // return %0
    vm.leave(end);
    monoasm!(vm.jit,
    label1:
    );
    // %0 <- x
    vm.get_local(x_ofs);
    // %1 <- 1
    vm.push_int(1);
    // %0 <- %0 - %1
    vm.sub();
    // %0 <- fibo(%0)
    vm.call_arg1(fibo);
    // %1 <- x
    vm.get_local(x_ofs);
    // %2 <- 2
    vm.push_int(2);
    // %1 <- %1 - %2
    vm.sub();
    // %1 <- fibo(%1)
    vm.call_arg1(fibo);
    // %0 <- %0 + %1
    vm.add();
    // return %0
    vm.leave(end);

    monoasm!(vm.jit,
    end:
    );
    vm.epilogue();
    vm.jit.finalize()
}

struct VM {
    jit: JitMemory,
    stack: u64,
}

#[allow(dead_code)]
impl VM {
    fn new() -> Self {
        Self {
            jit: JitMemory::new(),
            stack: 0,
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

    /// Push integer.
    /// stack +1
    fn push_int(&mut self, val: i64) {
        monoasm!(self.jit, movq R((self.stack) as u64 + 12), (val as u64););
        self.stack += 1;
    }

    /// Get local var(offset), and push it.
    /// stack +1
    fn get_local(&mut self, offset: i64) {
        monoasm!(self.jit, movq R(self.stack + 12), [rbp-(offset)];);
        self.stack += 1;
    }

    /// Pop a value, and set local var(offset) to the value.
    /// stack -1
    fn set_local(&mut self, offset: i64) {
        self.stack -= 1;
        monoasm!(self.jit, movq [rbp-(offset)], R(self.stack + 12););
    }

    /// Pop two values, and subtruct the former from the latter.
    /// Push the result.
    /// stack -1
    fn sub(&mut self) {
        self.stack -= 2;
        monoasm!(self.jit, subq R(self.stack + 12), R(self.stack + 13););
        self.stack += 1;
    }

    /// Pop two values, and add the former to the latter.
    /// Push the result.
    /// stack -1
    fn add(&mut self) {
        self.stack -= 2;
        monoasm!(self.jit, addq R(self.stack + 12), R(self.stack + 13););
        self.stack += 1;
    }

    /// Pop two values, and compare them.
    /// If the condition is met, jump to `dest`.
    /// stack -2
    fn jne(&mut self, dest: DestLabel) {
        self.stack -= 2;
        monoasm!(self.jit,
            cmpq R(self.stack + 12), R(self.stack + 13);
            jne dest;
        );
    }

    /// Pop one argument, and call `dest` with the arg.
    /// Push the returned value.
    /// stack +-0
    fn call_arg1(&mut self, dest: DestLabel) {
        self.stack -= 1;
        monoasm!(self.jit,
            movq rdi, R(self.stack + 12);
            call dest;
            movq R(self.stack + 12), rax;
        );
        self.stack += 1;
    }

    /// Pop a value, and return with it.
    /// stack -1
    fn leave(&mut self, end: DestLabel) {
        self.stack -= 1;
        monoasm!(self.jit,
            movq rax, R(self.stack + 12);
            jmp end;
        );
    }

    /// Pop a value, and print it as integer.
    /// stack -1
    fn puts(&mut self) {
        self.stack -= 1;
        monoasm!(self.jit,
            movq rdi, R(self.stack + 12);
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
