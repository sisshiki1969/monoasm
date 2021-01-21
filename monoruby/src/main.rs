mod codegen;
use codegen::Codegen;

fn main() {
    let _program = "
        fibo(40)
        def fibo(x)
            if x == 0 then return 0
            if x == 1 then return 1
            fibo(x - 1) + fibo(x - 2)
        end
        ";
    let ast = construct_ast();
    let mut codegen = Codegen::new();
    let fid = codegen.gen_func("fibo", ast, 1);
    codegen.resolve_func_labels();
    let x = 40;
    let ret = (codegen.get_func(fid).body)(x);
    println!("fib( {} ) = {}", x, ret);
    assert_eq!(102334155, ret)
}

pub enum Node {
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

pub struct If {
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
