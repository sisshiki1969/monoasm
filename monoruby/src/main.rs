mod codegen;
use codegen::Codegen;
mod parse;
use parse::script;
mod ast;
use ast::*;

fn main() {
    let program = "
    def fibo(x)
        if x == 0 then return 0 end
        if x == 1 then return 1 end
        return fibo(x-1) + fibo(x-2)
    end
    return fibo(40)
        ";
    eprintln!("{}", program);
    let ast = script(program);
    let mut codegen = Codegen::new();
    codegen.gen_func("main", ast, 1);
    codegen.resolve_func_labels();
    let x = 40;
    let ret = (codegen.get_func_ptr("main"))(x);
    println!("return value = {}", ret);
    assert_eq!(102334155, ret);
}
