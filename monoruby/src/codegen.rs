use super::ast::{CmpKind, Expr, Stmt};
use super::parse::script;
use monoasm::*;
use monoasm_macro::monoasm;
use std::collections::HashMap;

#[derive(Clone, Copy, Debug)]
pub struct FuncId(usize);

#[derive(Debug)]
struct FuncInfo {
    entry: DestLabel,
    arg_num: usize,
    pub body: extern "C" fn(u64) -> u64,
}

impl FuncInfo {
    fn new(entry: DestLabel, arg_num: usize, body: extern "C" fn(u64) -> u64) -> Self {
        Self {
            entry,
            arg_num,
            body,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct Context {
    stack: u64,
    entry: DestLabel,
    exit: DestLabel,
    lvars: HashMap<String, usize>,
    lvar_count: usize,
}

#[derive(Debug, Default)]
pub struct Codegen {
    jit: JitMemory,
    context: Context,
    func_map: HashMap<String, FuncId>,
    func_reloc: HashMap<String, DestLabel>,
    funcs: Vec<FuncInfo>,
}

impl Codegen {
    pub fn compile(&mut self, program: &str) -> extern "C" fn(u64) -> u64 {
        let ast = script(program);
        self.gen_func("main", ast, vec![]);
        self.resolve_func_labels();
        self.get_func_ptr("main")
    }

    pub fn get_func_ptr(&self, name: &str) -> extern "C" fn(u64) -> u64 {
        self.get_func_id(name).body
    }
}

impl Codegen {
    fn save_context(&mut self) -> Context {
        std::mem::take(&mut self.context)
    }

    fn restore_context(&mut self, mut ctx: Context) {
        self.context = std::mem::take(&mut ctx);
    }

    fn get_lvar(&mut self, name: String) -> usize {
        match self.context.lvars.get(&name) {
            None => {
                let ofs = self.context.lvar_count;
                self.context.lvar_count += 1;
                self.context.lvars.insert(name, ofs);
                ofs
            }
            Some(ofs) => *ofs,
        }
    }

    fn add_func(&mut self, name: &str, info: FuncInfo) -> FuncId {
        let fid = FuncId(self.funcs.len());
        self.funcs.push(info);
        if self.func_map.insert(name.to_string(), fid).is_some() {
            panic!("Duplicate func name {}.", name)
        };
        fid
    }

    fn get_func_id(&self, name: &str) -> &FuncInfo {
        let fid = self.func_map.get(name).unwrap();
        &self.funcs[fid.0]
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

    fn resolve_func_labels(&mut self) {
        for (name, dest) in &self.func_reloc {
            let fid = self.func_map.get(name).expect("Undefined func name.");
            let entry = self.funcs[fid.0].entry;
            let entry = self.jit.get_label_pos(entry);
            self.jit.bind_label_to_pos(*dest, entry);
        }
        self.jit.resolve_relocs();
    }

    fn gen_func(&mut self, name: &str, ast: Vec<Stmt>, args: Vec<String>) -> FuncId {
        let bypass = self.jit.label();
        monoasm!(self.jit,
            jmp bypass;
        );
        let arg_num = args.len();
        let saved_context = self.save_context();
        self.context.stack = 0;
        self.context.entry = self.jit.label();
        self.context.exit = self.jit.label();
        self.context.lvar_count = 0;
        for v in args {
            self.get_lvar(v);
        }
        self.jit.bind_label(self.context.entry);
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
            _ => unreachable!("the number of argument is too large."),
        };
        for node in ast {
            self.gen_stmt(node);
        }
        monoasm!(self.jit,
            movq rax, 7777;
        );
        self.jit.bind_label(self.context.exit);
        self.epilogue();
        self.jit.bind_label(bypass);
        let func = FuncInfo::new(
            self.context.entry,
            arg_num,
            self.jit.get_label_addr(self.context.entry),
        );
        self.restore_context(saved_context);
        self.add_func(name, func)
    }

    /// stack +1
    fn gen(&mut self, node: Expr) {
        match node {
            Expr::Integer(i) => self.push_int(i),
            Expr::LocalVar(name) => {
                let ofs = self.get_lvar(name);
                self.get_local(ofs);
            }
            Expr::Add(lhs, rhs) => {
                self.gen(*lhs);
                self.gen(*rhs);
                self.add();
            }
            Expr::Sub(lhs, rhs) => {
                self.gen(*lhs);
                self.gen(*rhs);
                self.sub();
            }
            Expr::Mul(lhs, rhs) => {
                self.gen(*lhs);
                self.gen(*rhs);
                self.mul();
            }
            Expr::Div(_lhs, _rhs) => unimplemented!(),
            Expr::Cmp(kind, lhs, rhs) => {
                self.gen(*lhs);
                self.gen(*rhs);
                self.cmp(kind);
            }
            Expr::Call(func, nodes) => {
                let arg_num = nodes.len();
                for node in nodes {
                    self.gen(node);
                }
                if func == "puts" {
                    assert!(arg_num == 1);
                    self.builtin(monoasm::test::PUTC, arg_num);
                } else if func == "panic" {
                    assert!(arg_num == 0);
                    self.builtin(monoasm::test::PANIC, arg_num);
                } else {
                    let label = self.get_func_label(func);
                    self.call_arg(label, arg_num);
                }
            }
            Expr::Assign(name, rhs) => {
                let ofs = self.get_lvar(name);
                self.gen(*rhs);
                self.set_local(ofs);
            }
            Expr::Nop => self.push_int(0),
        };
    }

    /// stack +-0
    fn gen_stmt(&mut self, node: Stmt) {
        match node {
            Stmt::Stmt(node) => {
                self.gen(*node);
                self.context.stack -= 1;
            }
            Stmt::IfStmt { cond, then } => {
                self.gen(*cond);
                let dest = self.jit.label();
                self.jmp_iff(dest);
                self.gen_stmt(*then);
                self.jit.bind_label(dest);
            }
            Stmt::IfCmpStmt {
                kind,
                lhs,
                rhs,
                then,
            } => {
                self.gen(*lhs);
                self.gen(*rhs);
                let dest = self.jit.label();
                self.cmp_jmp(kind, dest);
                self.gen_stmt(*then);
                self.jit.bind_label(dest);
            }
            Stmt::DefStmt(name, arg, body) => {
                self.gen_func(&name, body, arg);
            }
            Stmt::ReturnStmt(node) => {
                self.gen(*node);
                self.leave(self.context.exit)
            }
        };
    }
}

#[allow(dead_code)]
impl Codegen {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn exec_script(program: &str) -> extern "C" fn(u64) -> u64 {
        let mut codegen = Self::new();
        codegen.compile(program)
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
        assert!(self.context.stack <= 3);
        monoasm!(self.jit, movq R(self.context.stack + 12), (val as u64););
        self.context.stack += 1;
    }

    /// Get local var(offset), and push it.
    /// stack +1
    fn get_local(&mut self, offset: usize) {
        assert!(self.context.stack <= 3);
        let offset = (offset * 8) as i64 + 8;
        monoasm!(self.jit, movq R(self.context.stack + 12), [rbp-(offset)];);
        self.context.stack += 1;
    }

    /// Pop a value, and set local var(offset) to the value.
    /// stack +-1
    fn set_local(&mut self, offset: usize) {
        let offset = (offset * 8) as i64 + 8;
        self.context.stack -= 1;
        assert!(self.context.stack <= 3);
        monoasm!(self.jit, movq [rbp-(offset)], R(self.context.stack + 12););
        self.context.stack += 1;
    }

    /// Pop two values, and subtruct the former from the latter.
    /// Push the result.
    /// stack -1
    fn sub(&mut self) {
        self.context.stack -= 2;
        assert!(self.context.stack <= 3);
        monoasm!(self.jit, subq R(self.context.stack + 12), R(self.context.stack + 13););
        self.context.stack += 1;
    }

    /// Pop two values, and add the former to the latter.
    /// Push the result.
    /// stack -1
    fn add(&mut self) {
        self.context.stack -= 2;
        assert!(self.context.stack <= 3);
        monoasm!(self.jit, addq R(self.context.stack + 12), R(self.context.stack + 13););
        self.context.stack += 1;
    }

    /// Pop two values, and multiply the former to the latter.
    /// Push the result.
    /// stack -1
    fn mul(&mut self) {
        self.context.stack -= 2;
        assert!(self.context.stack <= 3);
        monoasm!(self.jit, imul R(self.context.stack + 12), R(self.context.stack + 13););
        self.context.stack += 1;
    }

    /// Pop two values, and compare the former to the latter.
    /// Push 1 if equal and 0 if else.
    /// stack -1
    fn cmp(&mut self, kind: CmpKind) {
        self.context.stack -= 2;
        let l0 = self.jit.label();
        let l1 = self.jit.label();
        assert!(self.context.stack <= 3);
        monoasm!(self.jit,
            cmpq R(self.context.stack + 12), R(self.context.stack + 13);
        );
        match kind {
            CmpKind::Eq => {
                monoasm!(self.jit,
                    jne l0;
                );
            }
            CmpKind::Ne => {
                monoasm!(self.jit,
                    jeq l0;
                );
            }
        }
        monoasm!(self.jit,
            movq R(self.context.stack + 12), 1;
            jmp l1;
        l0:
            movq R(self.context.stack + 12), 0;
        l1:
        );
        self.context.stack += 1;
    }

    /// Pop one values and jump to `dest` if the value is not 1.
    /// stack -1
    fn jmp_iff(&mut self, dest: DestLabel) {
        self.context.stack -= 1;
        assert!(self.context.stack <= 3);
        monoasm!(self.jit,
            cmpq R(self.context.stack + 12), 1;
            jne dest;
        );
    }

    fn cmp_jmp(&mut self, kind: CmpKind, dest: DestLabel) {
        self.context.stack -= 2;
        assert!(self.context.stack <= 3);
        monoasm!(self.jit,
            cmpq R(self.context.stack + 12), R(self.context.stack + 13);
        );
        match kind {
            CmpKind::Eq => {
                monoasm!(self.jit,
                    jne dest;
                );
            }
            CmpKind::Ne => {
                monoasm!(self.jit,
                    jeq dest;
                );
            }
        }
    }

    /// Pop one argument, and call `dest` with the arg.
    /// Push the returned value.
    /// stack +-0
    fn call_arg(&mut self, dest: DestLabel, arg_num: usize) {
        self.pop_args(arg_num);

        monoasm!(self.jit,
            call dest;
            movq R(self.context.stack + 12), rax;
        );
        self.context.stack += 1;
    }

    /// Pop a value, and return with it.
    /// stack -1
    fn leave(&mut self, end: DestLabel) {
        self.context.stack -= 1;
        assert!(self.context.stack <= 3);
        monoasm!(self.jit,
            movq rax, R(self.context.stack + 12);
            jmp end;
        );
    }

    /// Pop a value, and print it as integer.
    /// stack +-0
    fn builtin(&mut self, builtin: *const fn(), arg_num: usize) {
        self.pop_args(arg_num);
        monoasm!(self.jit,
            movq rax, (builtin as u64);
            call rax;
            movq R(self.context.stack + 12), rax;
        );
        self.context.stack += 1;
    }

    fn pop_args(&mut self, arg_num: usize) {
        self.context.stack -= arg_num as u64;
        assert!(self.context.stack <= 3);
        match arg_num {
            0 => {}
            1 => {
                monoasm!(self.jit,
                    movq rdi, R(self.context.stack + 12);
                );
            }
            2 => {
                monoasm!(self.jit,
                    movq rdi, R(self.context.stack + 12);
                    movq rsi, R(self.context.stack + 13);
                );
            }
            3 => {
                monoasm!(self.jit,
                    movq rdi, R(self.context.stack + 12);
                    movq rsi, R(self.context.stack + 13);
                    movq rdx, R(self.context.stack + 14);
                );
            }
            _ => unreachable!("the number of argument is too large."),
        };
    }
}

#[cfg(test)]
mod tests {
    extern crate test;
    use super::Codegen;
    use test::Bencher;

    fn fibo(x: u64) -> u64 {
        let program = "
        def fibo(x)
            if x == 0  
                return 0
            end
            if x == 1 then
                return 1
            end
            return fibo(x-1) + fibo(x-2)
        end
        return fibo(40)
        ";
        //eprintln!("{}", program);
        Codegen::exec_script(program)(x)
        //eprintln!("return value = {}", ret);
    }

    #[test]
    fn fibo_test() {
        let ret = fibo(40);
        eprintln!("fibo( {} ) = {}", 40, ret);
        assert_eq!(102334155, ret);
    }

    #[bench]
    fn bench_fibo(b: &mut Bencher) {
        let program = "
        def fibo(x)
            if x == 0 then return 0 end
            if x == 1 then return 1 end
            return fibo(x-1) + fibo(x-2)
        end
        return fibo(40)
        ";
        let main_func = Codegen::exec_script(program);
        b.iter(|| main_func(15));
    }

    fn factorial(x: u64) -> u64 {
        let program = "
        return fact(10)
        def fact(x)
            if x == 1 then return 1 end
            return x * fact(x-1)
        end
        ";
        //eprintln!("{}", program);
        Codegen::exec_script(program)(x)
        //eprintln!("return value = {}", ret);
    }

    #[test]
    fn fact_test() {
        let ret = factorial(10);
        eprintln!("factorial( {} ) = {}", 10, ret);
        assert_eq!(3628800, ret);
    }
}
