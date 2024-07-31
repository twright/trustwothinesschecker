use winnow::token::literal;
use winnow::PResult;
use winnow::Parser;
use winnow::combinator::*;

use winnow::ascii::dec_int as integer;
// Be sure to exclude actual tokens from the ident parser
use winnow::ascii::alphanumeric1 as ident;
use winnow::ascii::space0 as whitespace;

use crate::ast::*;

pub fn aexpr(s: &mut &str) -> PResult<AExpr> {
    return aminus.parse_next(s);
}

fn anum(s: &mut &str) -> PResult<AExpr> {
    return integer.map(AExpr::ANum).parse_next(s);
}

fn avar(s: &mut &str) -> PResult<AExpr> {
    return ident.map(
        |w: &str| AExpr::AVar(w.to_string())
    ).parse_next(s);
}

fn aatom(s: &mut &str) -> PResult<AExpr> {
    return delimited(
        whitespace,
        alt((
            anum, avar, paren_aexpr
        )),
        whitespace
    ).parse_next(s);
}

fn aplus_raw(s: &mut &str) -> PResult<AExpr> {
    return seq!((
        amult,
        literal('+'),
        amult,
    )).map(
        |(a1, _, a2)|
        AExpr::APlus(Box::new(a1), Box::new(a2))
    ).parse_next(s);
}

fn aplus(s: &mut &str) -> PResult<AExpr> {
    return delimited(
        whitespace,
        alt((
            aplus_raw, amult
        )),
        whitespace
    ).parse_next(s);
}

fn amult_raw(s: &mut &str) -> PResult<AExpr> {
    return seq!((
        aatom,
        literal('*'),
        aatom,
    )).map(
        |(a1, _, a2)|
        AExpr::AMult(Box::new(a1), Box::new(a2))
    ).parse_next(s);
}

fn amult(s: &mut &str) -> PResult<AExpr> {
    return delimited(whitespace, alt((
        amult_raw, paren_aexpr, anum
    )), whitespace).parse_next(s);
}

fn aminus_raw(s: &mut &str) -> PResult<AExpr> {
    return seq!((
        aplus,
        literal('-'),
        aplus,
    )).map(
        |(a1, _, a2)|
        AExpr::AMinus(Box::new(a1), Box::new(a2))
    ).parse_next(s);
}

fn aminus(s: &mut &str) -> PResult<AExpr> {
    return delimited(whitespace, alt((
        aminus_raw, aplus
    )), whitespace).parse_next(s);
}

fn paren_aexpr(s: &mut &str) -> PResult<AExpr> {
    return delimited(
        '(',
        aexpr,
        ')',
    ).parse_next(s);
}