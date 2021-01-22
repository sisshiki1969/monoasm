use super::{Cmp, CmpKind, If, Node};
use monoasm::*;
use monoasm_macro::monoasm;
use std::collections::HashMap;

#[derive(Clone, Copy, Debug)]
pub struct FuncId(usize);

pub struct FuncInfo {
    entry: DestLabel,
    pub body: fn(u64) -> u64,
}

impl FuncInfo {
    fn new(entry: DestLabel, body: fn(u64) -> u64) -> Self {
        Self { entry, body }
    }
}

pub struct Codegen {
    jit: JitMemory,
    stack: u64,
    entry: DestLabel,
    exit: DestLabel,
    func_map: HashMap<String, FuncId>,
    func_reloc: HashMap<String, DestLabel>,
    funcs: Vec<FuncInfo>,
}

impl Codegen {
    fn add_func(&mut self, name: &str, info: FuncInfo) -> FuncId {
        let fid = FuncId(self.funcs.len());
        self.funcs.push(info);
        if self.func_map.insert(name.to_string(), fid).is_some() {
            panic!("Duplicate func name {}.", name)
        };
        fid
    }

    pub fn get_func(&self, id: FuncId) -> &FuncInfo {
        &self.funcs[id.0]
    }

    fn get_func_label(&mut self, func: String) -> DestLabel {
        match self.func_reloc.get(&func) {
            Some(dest) => *dest,
            None => {
                let label = self.jit.label();
                self.func_reloc.insert(func, label);
                label
            }
        }
    }

    pub fn resolve_func_labels(&mut self) {
        for (name, dest) in &self.func_reloc {
            let fid = self.func_map.get(name).expect("Undefined func name.");
            let entry = self.get_func(*fid).entry;
            let entry = self.jit.get_label_pos(entry);
            self.jit.bind_label_to_pos(*dest, entry);
        }
        self.jit.resolve_relocs();
    }

    pub fn gen_func(&mut self, name: &str, ast: Vec<Node>, arg_num: usize) -> FuncId {
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
        let func = FuncInfo::new(self.entry, self.jit.get_label_addr(self.entry));
        self.add_func(name, func)
    }

    fn gen(&mut self, node: Node) {
        match node {
            Node::Stmt(nodes) => {
                for n in nodes {
                    self.gen(n);
                }
            }
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
            Node::Call(func, nodes) => {
                for node in nodes {
                    self.gen(node);
                }
                let label = self.get_func_label(func);
                self.call_arg1(label);
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
    pub fn new() -> Self {
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
            func_map: HashMap::default(),
            func_reloc: HashMap::default(),
            funcs: vec![],
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
