pub fn construct_ast() -> Vec<Node> {
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

#[derive(Clone, Debug, PartialEq)]
pub enum Node {
    Stmt(Vec<Node>),
    Integer(i64),  // push 1
    LocalVar(u64), // push 1
    If(If),        // pop2
    Return(Box<Node>),
    Add(Box<Node>, Box<Node>), // pop2, push1
    Sub(Box<Node>, Box<Node>), // pop2, push1
    Call(String, Vec<Node>),   // popn, push1
}

impl Node {
    pub fn stmt(nodes: Vec<Node>) -> Self {
        Node::Stmt(nodes)
    }

    pub fn add(lhs: Node, rhs: Node) -> Self {
        Node::Add(Box::new(lhs), Box::new(rhs))
    }

    pub fn sub(lhs: Node, rhs: Node) -> Self {
        Node::Sub(Box::new(lhs), Box::new(rhs))
    }

    pub fn if_(cond: Cmp, then: Node) -> Self {
        Node::If(If::new(cond, then))
    }

    pub fn call(func: &str, args: Vec<Node>) -> Self {
        Node::Call(func.to_string(), args)
    }

    pub fn ret(val: Node) -> Self {
        Node::Return(Box::new(val))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct If {
    pub cond: Cmp,
    pub then: Box<Node>,
}

impl If {
    fn new(cond: Cmp, then: Node) -> Self {
        Self {
            cond,
            then: Box::new(then),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Cmp {
    pub kind: CmpKind,
    pub lhs: Box<Node>,
    pub rhs: Box<Node>,
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

#[derive(Clone, Debug, PartialEq)]
pub enum CmpKind {
    Eq,
}
