use super::Node;
use nom::character::complete::{
    alpha1, alphanumeric1, char, digit1, multispace0, one_of, space0, space1,
};
use nom::combinator::{map, not, opt, recognize};
use nom::error::ParseError;
use nom::multi::{many0, many1, separated_list0};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use nom::{branch::alt, combinator::all_consuming};
use nom::{bytes::complete::tag, character::complete::newline};
use nom::{IResult, Parser};

pub fn script(s: &str) -> Vec<Node> {
    match all_consuming(terminated(
        separated_list0(many1(lineterm_ws), stmt),
        many0(lineterm_ws),
    ))(s)
    {
        Ok((_, nodes)) => nodes,
        Err(err) => panic!("{}", err),
    }
}

fn stmt(s: &str) -> IResult<&str, Node> {
    delimited(
        multispace0,
        alt((return_stmt, if_stmt, def_stmt, expr_stmt)),
        space0,
    )(s)
}

fn expr_stmt(s: &str) -> IResult<&str, Node> {
    map(expr, |expr| Node::stmt(expr))(s)
}

fn return_stmt(s: &str) -> IResult<&str, Node> {
    let (s, node) = preceded(tag("return"), opt(preceded(space1, expr)))(s)?;
    Ok((
        s,
        Node::ret(match node {
            Some(node) => node,
            None => Node::Nop,
        }),
    ))
}

fn if_stmt(s: &str) -> IResult<&str, Node> {
    let (s, (_, _, cond, _, then, _)) =
        tuple((tag("if"), space1, expr, tag("then"), stmt, tag("end")))(s)?;
    Ok((s, Node::if_(cond, then)))
}

fn def_stmt(s: &str) -> IResult<&str, Node> {
    let (s, (_, _, name, arg, body, _)) = tuple((
        tag("def"),
        space1,
        ident,
        delimited(
            char('('),
            delimited(multispace0, ident, multispace0),
            char(')'),
        ),
        delimited(many1(lineterm_ws), many0(stmt), multispace0),
        tag("end"),
    ))(s)?;
    Ok((s, Node::def(name, arg, body)))
}

fn expr(s: &str) -> IResult<&str, Node> {
    delimited(space0, eq_expr, space0)(s)
}

fn eq_expr(s: &str) -> IResult<&str, Node> {
    fn mapper(op: &str, lhs: Node, rhs: Node) -> Node {
        if op == "==" {
            Node::eq(lhs, rhs)
        } else {
            unimplemented!()
        }
    }
    binop_helper(add_expr, add_expr, tag("=="), mapper)(s)
}

fn add_expr(s: &str) -> IResult<&str, Node> {
    fn mapper(op: &str, lhs: Node, rhs: Node) -> Node {
        if op == "-" {
            Node::sub(lhs, rhs)
        } else if op == "+" {
            Node::add(lhs, rhs)
        } else {
            unimplemented!()
        }
    }
    binop_helper(prim_expr, prim_expr, alt((tag("+"), tag("-"))), mapper)(s)
}

fn binop_helper<'a, E: ParseError<&'a str>, F, G, H>(
    base0: F,
    base1: F,
    operator: G,
    mut mapper: H,
) -> impl FnMut(&'a str) -> IResult<&'a str, Node, E>
where
    F: Parser<&'a str, Node, E>,
    G: Parser<&'a str, &'a str, E>,
    H: FnMut(&'a str, Node, Node) -> Node,
{
    map(
        pair(
            base0,
            opt(pair(
                preceded(space0, operator),
                preceded(multispace0, base1),
            )),
        ),
        move |(lhs, rhs)| match rhs {
            None => lhs,
            Some((op, rhs)) => mapper(op, lhs, rhs),
        },
    )
}

fn prim_expr(s: &str) -> IResult<&str, Node> {
    alt((method_call, local_var, decimal_number))(s)
}

fn method_call(s: &str) -> IResult<&str, Node> {
    let (s, name0) = ident(s)?;
    let (s, arg) = delimited(
        char('('),
        delimited(multispace0, expr, multispace0),
        char(')'),
    )(s)?;
    let node = Node::call(name0, vec![arg]);
    Ok((s, node))
}

fn local_var(s: &str) -> IResult<&str, Node> {
    let (s, name) = ident(s)?;
    Ok((s, Node::localvar(name)))
}

fn ident(s: &str) -> IResult<&str, &str> {
    not(alt((tag("end"), tag("def"), tag("if"))))(s)?;
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(s)
}

fn decimal_number(s: &str) -> IResult<&str, Node> {
    let (s, n) = recognize(tuple((opt(one_of("+-")), digit1)))(s)?;
    let num = n.parse::<i64>().unwrap();
    Ok((s, Node::Integer(num)))
}

fn lineterm_ws(s: &str) -> IResult<&str, char> {
    delimited(space0, alt((newline, char(';'))), space0)(s)
}

#[allow(unused_imports)]
mod test {
    use super::super::ast::*;
    use super::*;
    #[test]
    fn expr_test() {
        assert_eq!(Node::localvar("_f1"), expr("_f1").unwrap().1);
        assert_eq!(Node::Integer(100), expr("100").unwrap().1);
        assert_eq!(Node::Integer(-100), expr("-100").unwrap().1);
        assert_eq!(
            Node::call("func", vec![Node::Integer(100)]),
            expr("func(100)").unwrap().1
        );
        assert_eq!(
            Node::sub(Node::Integer(100), Node::Integer(30)),
            expr("100 - 30").unwrap().1
        );
        assert_eq!(
            Node::add(Node::Integer(100), Node::Integer(30)),
            expr("100 + 30").unwrap().1
        );
    }

    #[test]
    fn return_test() {
        assert_eq!(Node::ret(Node::Nop), return_stmt("return").unwrap().1);
        assert_eq!(
            Node::ret(Node::Integer(100)),
            return_stmt("return 100").unwrap().1
        );
        assert_eq!(
            Node::ret(Node::add(Node::localvar("x"), Node::Integer(100))),
            return_stmt("return x+100").unwrap().1
        );
    }

    #[test]
    fn if_test() {
        assert_eq!(
            Node::if_(
                Node::eq(Node::localvar("x"), Node::Integer(0)),
                Node::stmt(Node::Integer(3))
            ),
            if_stmt("if x == 0 then 3 end").unwrap().1
        );
        assert_eq!(
            Node::if_(
                Node::eq(Node::localvar("x"), Node::Integer(1)),
                Node::ret(Node::Integer(1))
            ),
            if_stmt("if x == 1 then return 1 end").unwrap().1
        );
    }
}
