mod codegen;
use codegen::Codegen;
mod parse;
use parse::stmt;
mod ast;
use ast::*;

fn main() {
    let program = "
        fibo(x - 1) + fibo(x - 2)
        fibo(40)
        if x == 0 then return 0
        def fibo(x)
            if x == 0 then return 0
            if x == 1 then return 1
            fibo(x - 1) + fibo(x - 2)
        end
        ";
    eprintln!("{:?}", stmt(program));
    let ast = construct_ast();
    let mut codegen = Codegen::new();
    let fid = codegen.gen_func("fibo", ast, 1);
    codegen.resolve_func_labels();
    let x = 40;
    let ret = (codegen.get_func(fid).body)(x);
    println!("fib( {} ) = {}", x, ret);
    assert_eq!(102334155, ret);
}

mod test {
    use super::*;
    #[test]
    fn decimal_test() {
        assert_eq!(Node::Integer(100), decimal_number("100").unwrap().1);
        assert_eq!(Node::Integer(-100), decimal_number("-100").unwrap().1);
        assert_eq!(
            Node::sub(Node::Integer(100), Node::Integer(30)),
            add_expr("100 - 30").unwrap().1
        );
        assert_eq!(
            Node::add(Node::Integer(100), Node::Integer(30)),
            add_expr("100 + 30").unwrap().1
        );
    }
}
