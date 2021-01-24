#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Integer(i64),                       // push 1
    LocalVar(String),                   // push 1
    Add(Box<Expr>, Box<Expr>),          // pop2, push1
    Sub(Box<Expr>, Box<Expr>),          // pop2, push1
    Cmp(CmpKind, Box<Expr>, Box<Expr>), // pop2, push1
    Call(String, Vec<Expr>),            // popn, push1
    Assign(String, Box<Expr>),          // pop1, push1
    Nop,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Stmt(Box<Expr>),
    IfStmt {
        cond: Box<Expr>,
        then: Box<Stmt>,
    }, // pop2
    IfCmpStmt {
        kind: CmpKind,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        then: Box<Stmt>,
    }, // pop2
    ReturnStmt(Box<Expr>),
    DefStmt(String, Vec<String>, Vec<Stmt>),
}

#[allow(dead_code)]
impl Expr {
    pub fn localvar(name: &str) -> Self {
        Expr::LocalVar(name.to_owned())
    }

    pub fn add(lhs: Expr, rhs: Expr) -> Self {
        Expr::Add(Box::new(lhs), Box::new(rhs))
    }

    pub fn sub(lhs: Expr, rhs: Expr) -> Self {
        Expr::Sub(Box::new(lhs), Box::new(rhs))
    }

    pub fn eq(lhs: Expr, rhs: Expr) -> Self {
        Expr::Cmp(CmpKind::Eq, Box::new(lhs), Box::new(rhs))
    }

    pub fn ne(lhs: Expr, rhs: Expr) -> Self {
        Expr::Cmp(CmpKind::Ne, Box::new(lhs), Box::new(rhs))
    }

    pub fn call(func: &str, args: Vec<Expr>) -> Self {
        Expr::Call(func.to_string(), args)
    }

    pub fn assign(name: impl Into<String>, rhs: Expr) -> Self {
        Expr::Assign(name.into(), Box::new(rhs))
    }
}

#[allow(dead_code)]
impl Stmt {
    pub fn stmt(node: Expr) -> Self {
        Self::Stmt(Box::new(node))
    }

    pub fn if_(cond: Expr, then: Stmt) -> Self {
        if let Expr::Cmp(kind, lhs, rhs) = cond {
            Self::IfCmpStmt {
                kind,
                lhs,
                rhs,
                then: Box::new(then),
            }
        } else {
            Self::IfStmt {
                cond: Box::new(cond),
                then: Box::new(then),
            }
        }
    }

    pub fn ret(val: Expr) -> Self {
        Self::ReturnStmt(Box::new(val))
    }

    pub fn def(name: &str, arg: Vec<String>, body: Vec<Stmt>) -> Self {
        Self::DefStmt(name.to_string(), arg, body)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum CmpKind {
    Eq,
    Ne,
}
