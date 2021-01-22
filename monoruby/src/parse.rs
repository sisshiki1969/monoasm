use super::Node;
use nom::branch::{alt, permutation};
use nom::character::{
    complete::{alpha1, char, digit1, multispace0, newline, one_of},
    streaming::alphanumeric0,
};
use nom::combinator::{not, opt};
use nom::multi::separated_list0;
use nom::sequence::delimited;
use nom::IResult;

pub fn stmt(s: &str) -> IResult<&str, Node> {
    let (s, node) = separated_list0(newline, expr)(s)?;
    Ok((s, Node::stmt(node)))
}

fn expr(s: &str) -> IResult<&str, Node> {
    add_expr(s)
}

fn add_expr(s: &str) -> IResult<&str, Node> {
    fn add(s: &str) -> IResult<&str, Node> {
        let (s, (lhs, _, op, _, rhs)) =
            permutation((call_expr, multispace0, one_of("+-"), multispace0, call_expr))(s)?;
        let node = if op == '-' {
            Node::sub(lhs, rhs)
        } else if op == '+' {
            Node::add(lhs, rhs)
        } else {
            unimplemented!()
        };
        Ok((s, node))
    }
    alt((add, call_expr))(s)
}

fn call_expr(s: &str) -> IResult<&str, Node> {
    fn method(s: &str) -> IResult<&str, Node> {
        let (s, _) = multispace0(s)?;
        let (s, name0) = alpha1(s)?;
        let (s, name1) = alphanumeric0(s)?;
        let (s, arg) = delimited(
            char('('),
            delimited(multispace0, expr, multispace0),
            char(')'),
        )(s)?;
        let node = Node::call(&format!("{}{}", name0, name1), vec![arg]);
        Ok((s, node))
    }
    alt((prim_expr, method))(s)
}

fn prim_expr(s: &str) -> IResult<&str, Node> {
    delimited(multispace0, alt((decimal_number, local_var)), multispace0)(s)
}

fn local_var(s: &str) -> IResult<&str, Node> {
    let (s, name0) = alpha1(s)?;
    let (s, name1) = alphanumeric0(s)?;
    not(char('('))(s)?;
    let node = Node::LocalVar(0);
    Ok((s, node))
}

fn decimal_number(s: &str) -> IResult<&str, Node> {
    let (s, (sign, n1)) = permutation((opt(one_of("+-")), digit1))(s)?;
    let num = (match sign {
        Some(c) => c.to_string(),
        None => String::new(),
    } + n1)
        .parse::<i64>()
        .unwrap();
    Ok((s, Node::Integer(num)))
}
