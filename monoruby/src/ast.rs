pub fn _construct_ast() -> Vec<Node> {
    vec![
        Node::if_(
            Node::eq(Node::LocalVar("x".to_string()), Node::Integer(0)),
            Node::ret(Node::Integer(0)),
        ),
        Node::if_(
            Node::eq(Node::LocalVar("x".to_string()), Node::Integer(1)),
            Node::ret(Node::Integer(1)),
        ),
        Node::ret(Node::add(
            Node::call(
                "fibo",
                vec![Node::sub(Node::LocalVar("x".to_string()), Node::Integer(1))],
            ),
            Node::call(
                "fibo",
                vec![Node::sub(Node::LocalVar("x".to_string()), Node::Integer(2))],
            ),
        )),
    ]
}

#[derive(Clone, Debug, PartialEq)]
pub enum Node {
    Stmt(Box<Node>),
    Integer(i64),                                // push 1
    LocalVar(String),                            // push 1
    Add(Box<Node>, Box<Node>),                   // pop2, push1
    Sub(Box<Node>, Box<Node>),                   // pop2, push1
    Cmp(CmpKind, Box<Node>, Box<Node>),          // pop2, push1
    Call(String, Vec<Node>),                     // popn, push1
    IfStmt { cond: Box<Node>, then: Box<Node> }, // pop2
    ReturnStmt(Box<Node>),
    DefStmt(String, String, Vec<Node>),
    Nop,
}

#[allow(dead_code)]
impl Node {
    pub fn stmt(node: Node) -> Self {
        Node::Stmt(Box::new(node))
    }

    pub fn localvar(name: &str) -> Self {
        Node::LocalVar(name.to_owned())
    }

    pub fn add(lhs: Node, rhs: Node) -> Self {
        Node::Add(Box::new(lhs), Box::new(rhs))
    }

    pub fn sub(lhs: Node, rhs: Node) -> Self {
        Node::Sub(Box::new(lhs), Box::new(rhs))
    }

    pub fn eq(lhs: Node, rhs: Node) -> Self {
        Node::Cmp(CmpKind::Eq, Box::new(lhs), Box::new(rhs))
    }

    pub fn if_(cond: Node, then: Node) -> Self {
        Node::IfStmt {
            cond: Box::new(cond),
            then: Box::new(then),
        }
    }

    pub fn call(func: &str, args: Vec<Node>) -> Self {
        Node::Call(func.to_string(), args)
    }

    pub fn ret(val: Node) -> Self {
        Node::ReturnStmt(Box::new(val))
    }

    pub fn def(name: &str, arg: &str, body: Vec<Node>) -> Self {
        Node::DefStmt(name.to_string(), arg.to_string(), body)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum CmpKind {
    Eq,
}
