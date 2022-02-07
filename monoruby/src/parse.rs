use crate::ast::{Expr, Stmt};
use nom::combinator::{map, not, opt, recognize};
use nom::error::ParseError;
use nom::multi::{fold_many0, many0, many1, separated_list0};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use nom::{branch::alt, combinator::all_consuming};
use nom::{bytes::complete::tag, character::complete::newline};
use nom::{
    character::complete::{
        alpha1, alphanumeric1, char, digit1, multispace0, multispace1, one_of, space0, space1,
    },
    AsChar, InputTakeAtPosition,
};
use nom::{IResult, Parser};

pub fn script(s: &str) -> Vec<Stmt> {
    match all_consuming(terminated(
        separated_list0(many1(lineterm_ws), stmt),
        many0(lineterm_ws),
    ))(s)
    {
        Ok((_, nodes)) => nodes,
        Err(err) => panic!("{}", err),
    }
}

fn stmt(s: &str) -> IResult<&str, Stmt> {
    delimited(
        multispace0,
        alt((return_stmt, if_stmt, def_stmt, expr_stmt)),
        space0,
    )(s)
}

fn expr_stmt(s: &str) -> IResult<&str, Stmt> {
    map(expr, |expr| Stmt::stmt(expr))(s)
}

fn return_stmt(s: &str) -> IResult<&str, Stmt> {
    let (s, node) = preceded(tag("return"), opt(preceded(space1, expr)))(s)?;
    Ok((
        s,
        Stmt::ret(match node {
            Some(node) => node,
            None => Expr::Nop,
        }),
    ))
}

fn if_stmt(s: &str) -> IResult<&str, Stmt> {
    let (s, (_, _, cond, _, then, _)) = tuple((
        tag("if"),
        space1,
        expr,
        alt((multispace1, tag("then"))),
        stmt,
        pair(multispace0, tag("end")),
    ))(s)?;
    Ok((s, Stmt::if_(cond, then)))
}

fn def_stmt(s: &str) -> IResult<&str, Stmt> {
    let (s, (_, _, name, arg, body, _)) = tuple((
        tag("def"),
        space1,
        ident,
        delimited(char('('), method_params, char(')')),
        delimited(many1(lineterm_ws), many0(stmt), multispace0),
        tag("end"),
    ))(s)?;
    Ok((s, Stmt::def(name, arg, body)))
}

fn method_params(s: &str) -> IResult<&str, Vec<String>> {
    map(
        delimited(
            multispace0,
            separated_list0(tuple((multispace0, char(','), multispace0)), ident),
            multispace0,
        ),
        |x| x.iter().map(|x| x.to_string()).collect(),
    )(s)
}

fn expr(s: &str) -> IResult<&str, Expr> {
    delimited(space0, assign_expr, space0)(s)
}

fn assign_expr(s: &str) -> IResult<&str, Expr> {
    let (s, lhs) = eq_expr(s)?;
    fn mapper(_op: &str, lhs: Expr, rhs: Expr) -> Expr {
        match lhs {
            Expr::LocalVar(name) => Expr::assign(name, rhs),
            _ => unimplemented!(),
        }
    }
    binop_fold(lhs, eq_expr, tag("="), mapper)(s)
}

fn eq_expr(s: &str) -> IResult<&str, Expr> {
    let (s, lhs) = add_expr(s)?;
    fn mapper(op: &str, lhs: Expr, rhs: Expr) -> Expr {
        if op == "==" {
            Expr::eq(lhs, rhs)
        } else if op == "!=" {
            Expr::ne(lhs, rhs)
        } else {
            unimplemented!()
        }
    }
    binop_once(lhs, add_expr, alt((tag("=="), tag("!="))), mapper)(s)
}

fn add_expr(s: &str) -> IResult<&str, Expr> {
    let (s, lhs) = mul_expr(s)?;
    fn mapper(op: &str, lhs: Expr, rhs: Expr) -> Expr {
        if op == "-" {
            Expr::sub(lhs, rhs)
        } else if op == "+" {
            Expr::add(lhs, rhs)
        } else {
            unimplemented!()
        }
    }
    binop_fold(lhs, mul_expr, alt((tag("+"), tag("-"))), mapper)(s)
}

fn mul_expr(s: &str) -> IResult<&str, Expr> {
    let (s, lhs) = prim_expr(s)?;
    fn mapper(op: &str, lhs: Expr, rhs: Expr) -> Expr {
        if op == "*" {
            Expr::mul(lhs, rhs)
        } else if op == "/" {
            Expr::div(lhs, rhs)
        } else {
            unimplemented!()
        }
    }
    binop_fold(lhs, prim_expr, alt((tag("*"), tag("/"))), mapper)(s)
}

fn binop_once<'a, I, O, E: ParseError<I>, F, G, H>(
    lhs: O,
    base1: F,
    operator: G,
    mut mapper: H,
) -> impl FnMut(I) -> IResult<I, O, E>
where
    I: InputTakeAtPosition + Clone + PartialEq,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
    O: Clone,
    F: Parser<I, O, E>,
    G: Parser<I, I, E>,
    H: FnMut(I, O, O) -> O,
{
    map(
        opt(pair(
            preceded(space0, operator),
            preceded(multispace0, base1),
        )),
        move |rhs| match rhs {
            Some((op, rhs)) => mapper(op, lhs.clone(), rhs),
            None => lhs.clone(),
        },
    )
}

fn binop_fold<'a, E: ParseError<&'a str>, F, G, H>(
    lhs: Expr,
    base1: F,
    operator: G,
    mut mapper: H,
) -> impl FnMut(&'a str) -> IResult<&'a str, Expr, E>
where
    F: Parser<&'a str, Expr, E>,
    G: Parser<&'a str, &'a str, E>,
    H: FnMut(&'a str, Expr, Expr) -> Expr,
{
    fold_many0(
        pair(preceded(space0, operator), preceded(multispace0, base1)),
        move || lhs.clone(),
        move |lhs, (op, rhs)| mapper(op, lhs, rhs),
    )
}

fn prim_expr(s: &str) -> IResult<&str, Expr> {
    alt((paren_expr, method_call, local_var, decimal_number))(s)
}

fn method_call(s: &str) -> IResult<&str, Expr> {
    let (s, name0) = ident(s)?;
    let (s, args) = delimited(char('('), method_args, char(')'))(s)?;
    let node = Expr::call(name0, args);
    Ok((s, node))
}

fn method_args(s: &str) -> IResult<&str, Vec<Expr>> {
    delimited(
        multispace0,
        separated_list0(tuple((multispace0, char(','), multispace0)), expr),
        multispace0,
    )(s)
}

fn paren_expr(s: &str) -> IResult<&str, Expr> {
    delimited(
        char('('),
        delimited(multispace0, expr, multispace0),
        char(')'),
    )(s)
}

fn local_var(s: &str) -> IResult<&str, Expr> {
    let (s, name) = ident(s)?;
    Ok((s, Expr::localvar(name)))
}

fn ident(s: &str) -> IResult<&str, &str> {
    not(alt((tag("end"), tag("def"), tag("if"))))(s)?;
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(s)
}

fn decimal_number(s: &str) -> IResult<&str, Expr> {
    let (s, n) = recognize(tuple((opt(one_of("+-")), digit1)))(s)?;
    let num = n.parse::<i64>().unwrap();
    Ok((s, Expr::Integer(num)))
}

fn lineterm_ws(s: &str) -> IResult<&str, char> {
    delimited(space0, alt((newline, char(';'))), space0)(s)
}

#[cfg(test)]
mod test {
    use super::super::ast::*;
    use super::*;
    #[test]
    fn prim_test() {
        assert_eq!(Expr::localvar("_f1"), expr("_f1").unwrap().1);
        assert_eq!(Expr::Integer(100), expr("100").unwrap().1);
        assert_eq!(Expr::Integer(-100), expr("-100").unwrap().1);
        assert_eq!(
            Expr::call("func", vec![Expr::Integer(100)]),
            expr("func(100)").unwrap().1
        );
    }

    #[test]
    fn op_test() {
        assert_eq!(
            Expr::sub(Expr::Integer(100), Expr::Integer(30)),
            expr("100 - 30").unwrap().1
        );
        assert_eq!(
            Expr::add(Expr::Integer(100), Expr::Integer(30)),
            expr("100 + 30").unwrap().1
        );
        assert_eq!(
            Expr::sub(
                Expr::add(Expr::Integer(100), Expr::Integer(30)),
                Expr::Integer(50)
            ),
            expr("100 + 30 - 50").unwrap().1
        );
        assert_eq!(
            Expr::sub(
                Expr::Integer(100),
                Expr::mul(Expr::Integer(5), Expr::Integer(30))
            ),
            expr("100 - 5* 30 ").unwrap().1
        );
        assert_eq!(
            Expr::div(
                Expr::add(Expr::Integer(100), Expr::Integer(50)),
                Expr::Integer(30),
            ),
            expr(" ( 100 + 50) /30 ").unwrap().1
        );
    }

    #[test]
    fn paren_test() {
        assert_eq!(
            Expr::sub(
                Expr::Integer(100),
                Expr::add(Expr::Integer(30), Expr::Integer(47))
            ),
            expr("100 -( 30+ 47  )").unwrap().1
        );
        assert_eq!(
            Expr::sub(
                Expr::Integer(100),
                Expr::add(Expr::Integer(30), Expr::Integer(47))
            ),
            expr("100-  (30 +47)").unwrap().1
        );
        /*assert_eq!(
            Expr::add(Expr::Integer(100), Expr::Integer(30)),
            expr("100 + 30").unwrap().1
        );
        assert_eq!(
            Expr::sub(
                Expr::add(Expr::Integer(100), Expr::Integer(30)),
                Expr::Integer(50)
            ),
            expr("100 + 30 - 50").unwrap().1
        );*/
    }

    #[test]
    fn return_test() {
        assert_eq!(Stmt::ret(Expr::Nop), return_stmt("return").unwrap().1);
        assert_eq!(
            Stmt::ret(Expr::Integer(100)),
            return_stmt("return 100").unwrap().1
        );
        assert_eq!(
            Stmt::ret(Expr::add(Expr::localvar("x"), Expr::Integer(100))),
            return_stmt("return x+100").unwrap().1
        );
    }

    #[test]
    fn if_test() {
        assert_eq!(
            Stmt::if_(
                Expr::eq(Expr::localvar("x"), Expr::Integer(0)),
                Stmt::stmt(Expr::Integer(3))
            ),
            if_stmt("if x == 0 then 3 end").unwrap().1
        );
        assert_eq!(
            Stmt::if_(
                Expr::eq(Expr::localvar("x"), Expr::Integer(1)),
                Stmt::ret(Expr::Integer(1))
            ),
            if_stmt("if x == 1 then return 1 end").unwrap().1
        );
    }
}
