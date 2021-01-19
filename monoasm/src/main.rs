#![feature(proc_macro_hygiene)]
extern crate monoasm;
extern crate monoasm_macro;
use monoasm::*;
use monoasm_inst::Reg;
use monoasm_macro::monoasm;

fn main() {
    let ast = construct_ast();
    let mut codegen = Codegen::new();
    let func = codegen.gen_func(ast, 1);
    let x = 40;
    let ret = func(x);
    println!("fib( {} ) = {}", x, ret);
    assert_eq!(102334155, ret)
}

struct Codegen {
    jit: JitMemory,
    stack: u64,
    entry: DestLabel,
    exit: DestLabel,
}

impl Codegen {
    fn gen_func(&mut self, ast: Vec<Node>, arg_num: usize) -> fn(u64) -> u64 {
        self.stack = 0;
        self.entry = self.jit.label();
        self.exit = self.jit.label();
        self.jit.bind_label(self.entry);
        self.prologue(arg_num);
        match arg_num {
            0 => {}
            1 => {
                monoasm!(self.jit,
                    movq [rbp - 8], rdi;
                );
            }
            2 => {
                monoasm!(self.jit,
                    movq [rbp - 8], rdi;
                    movq [rbp - 16], rsi;
                );
            }
            3 => {
                monoasm!(self.jit,
                    movq [rbp - 8], rdi;
                    movq [rbp - 16], rsi;
                    movq [rbp - 24], rdx;
                );
            }
            _ => unreachable!(),
        };
        for node in ast {
            self.gen(node);
        }
        self.jit.bind_label(self.exit);
        self.epilogue();
        self.jit.finalize()
    }

    fn gen(&mut self, node: Node) {
        match node {
            Node::Integer(i) => self.push_int(i),
            Node::LocalVar(lvar) => self.get_local((lvar * 8) as i64 + 8),
            Node::Return(node) => {
                self.gen(*node);
                self.leave(self.exit)
            }
            Node::Add(lhs, rhs) => {
                self.gen(*lhs);
                self.gen(*rhs);
                self.add();
            }
            Node::Sub(lhs, rhs) => {
                self.gen(*lhs);
                self.gen(*rhs);
                self.sub();
            }
            Node::Call(_func, nodes) => {
                for node in nodes {
                    self.gen(node);
                }
                self.call_arg1(self.entry);
            }
            Node::If(If {
                cond: Cmp { kind, lhs, rhs },
                then,
            }) => {
                self.gen(*lhs);
                self.gen(*rhs);
                match kind {
                    CmpKind::Eq => {
                        let dest = self.jit.label();
                        self.jne(dest);
                        self.gen(*then);
                        self.jit.bind_label(dest);
                    }
                }
            }
        };
    }
}

#[allow(dead_code)]
impl Codegen {
    fn new() -> Self {
        let mut jit = JitMemory::new();
        let entry = jit.label();
        let exit = jit.label();
        jit.bind_label(entry);
        jit.bind_label(exit);
        Self {
            jit,
            stack: 0,
            entry,
            exit,
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
        monoasm!(self.jit, movq R(self.stack + 12), (val as u64););
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
    Integer(i64),  // push 1
    LocalVar(u64), // push 1
    If(If),        // pop2
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
