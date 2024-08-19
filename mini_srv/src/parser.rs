use winnow::token::literal;
use winnow::PResult;
use winnow::Parser;
use winnow::combinator::*;

use winnow::ascii::dec_int as integer;
// Be sure to exclude actual tokens from the ident parser
use winnow::ascii::alphanumeric1 as ident;
use winnow::ascii::alphanumeric0 as string;
use winnow::ascii::space0 as whitespace;

use crate::ast::*;

pub fn aexpr(s: &mut &str) -> PResult<SExpr<VarName>> {
    return aminus.parse_next(s);
}

fn anum(s: &mut &str) -> PResult<SExpr<VarName>> {
    return integer.map(SExpr::Num).parse_next(s);
}

fn avar(s: &mut &str) -> PResult<SExpr<VarName>> {
    return ident.map(
        |w: &str| SExpr::Var(VarName(w.into()))
    ).parse_next(s);
}

fn aatom(s: &mut &str) -> PResult<SExpr<VarName>> {
    return delimited(
        whitespace,
        alt((
            anum, avar, paren_aexpr
        )),
        whitespace
    ).parse_next(s);
}

fn aplus_raw(s: &mut &str) -> PResult<SExpr<VarName>> {
    return separated_foldr1(
        amult,
        "+", 
        |x, _, y| SExpr::Plus(Box::new(x), Box::new(y)
    )).parse_next(s);
}

fn aplus(s: &mut &str) -> PResult<SExpr<VarName>> {
    return delimited(
        whitespace,
        alt((
            aplus_raw, amult
        )),
        whitespace
    ).parse_next(s);
}

fn amult_raw(s: &mut &str) -> PResult<SExpr<VarName>> {
    return separated_foldr1(
        aatom,
        "*",
        |x, _, y|
        SExpr::Mult(Box::new(x), Box::new(y))
    ).parse_next(s);
}

fn amult(s: &mut &str) -> PResult<SExpr<VarName>> {
    return delimited(whitespace, alt((
        amult_raw, paren_aexpr, anum
    )), whitespace).parse_next(s);
}

fn aminus_raw(s: &mut &str) -> PResult<SExpr<VarName>> {
    return separated_foldl1(
        aplus,
        "-",
        |x, _, y| SExpr::Minus(Box::new(x), Box::new(y))
    ).parse_next(s)
}

fn aminus(s: &mut &str) -> PResult<SExpr<VarName>> {
    return delimited(whitespace, alt((
        aminus_raw, aplus
    )), whitespace).parse_next(s);
}

fn paren_aexpr(s: &mut &str) -> PResult<SExpr<VarName>> {
    return delimited(
        '(',
        aexpr,
        ')',
    ).parse_next(s);
}

fn btrue(s: &mut &str) -> PResult<BExpr<VarName>> {
    return delimited(
        whitespace,
        literal("true"),
        whitespace,
    ).map(|_| BExpr::Val(true)).parse_next(s);
}

fn bfalse(s: &mut &str) -> PResult<BExpr<VarName>> {
    return delimited(
        whitespace,
        literal("false"),
        whitespace,
    ).map(|_| BExpr::Val(false)).parse_next(s);
}

fn beq(s: &mut &str) -> PResult<BExpr<VarName>> {
    return seq!((
        _: whitespace,
        aexpr,
        _: literal('='),
        aexpr,
        _: whitespace,
    ))
    .map(|(a1, a2)| BExpr::Eq(Box::new(a1), Box::new(a2)))
    .parse_next(s);
}

fn ble(s: &mut &str) -> PResult<BExpr<VarName>> {
    return seq!((
        _: whitespace,
        aexpr,
        _: literal("<="),
        aexpr,
        _: whitespace,
    ))
    .map(|(a1, a2)| BExpr::Le(Box::new(a1), Box::new(a2)))
    .parse_next(s);
}

fn bnot(s: &mut &str) -> PResult<BExpr<VarName>> {
    return seq!((
        _: whitespace,
        literal('!'),
        bexpr,
        _: whitespace,
    ))
    .map(|(_, b)| BExpr::Not(Box::new(b)))
    .parse_next(s);
}

fn band_raw(s: &mut &str) -> PResult<BExpr<VarName>> {
    return separated_foldr1(
        bexpr,
        "&&",
        |b1, _, b2|
        BExpr::And(Box::new(b1), Box::new(b2))
    ).parse_next(s);
}

fn band(s: &mut &str) -> PResult<BExpr<VarName>> {
    return delimited(
        whitespace,
        band_raw, 
        whitespace
    ).parse_next(s);
}

fn bor_raw(s: &mut &str) -> PResult<BExpr<VarName>> {
    return separated_foldr1(
        bexpr,
        "||",
        |b1, _, b2|
        BExpr::Or(Box::new(b1), Box::new(b2))
    ).parse_next(s);
}

fn bor(s: &mut &str) -> PResult<BExpr<VarName>> {
    return delimited(
        whitespace,
        bor_raw, 
        whitespace
    ).parse_next(s);
}

fn bexpr(s: &mut &str) -> PResult<BExpr<VarName>> {
    return delimited(
        whitespace,
        alt((band, bor, beq, ble, bnot, btrue, bfalse)),
        whitespace
    ).parse_next(s);
}

fn sif(s: &mut &str) -> PResult<SExpr<VarName>> {
    return seq!((
        _: whitespace,
        _: "if",
        bexpr,
        _: "then",
        sexpr,
        _: "else",
        sexpr,
        _: whitespace,
    ))
    .map(|(b, s1, s2)|
         SExpr::If(Box::new(b), Box::new(s1), Box::new(s2)))
    .parse_next(s);
}

fn sindex(s: &mut &str) -> PResult<SExpr<VarName>> {
    return seq!((
        _: whitespace,
        sexpr,
        _: whitespace,
        _: "[",
        _: whitespace,
        integer,
        _: whitespace,
        _: ",",
        integer,
        _: "]",
        _: whitespace,
    ))
    .map(|(s, i,c)|
         SExpr::Index(Box::new(s), i, c))
    .parse_next(s);
}

// fn seq(s: &mut &str) -> PResult<SExpr<VarName>> {
//     return seq!((
//         _: whitespace,
//         sexpr,
//         _: literal(":="),
//         sexpr,
//         _: whitespace,
//     ))
//     .map(|(a1, a2)| SExpr::SEq(Box::new(a1), Box::new(a2)))
//     .parse_next(s);
// }

fn paren_sexpr(s: &mut &str) -> PResult<SExpr<VarName>> {
    return delimited(
        '(',
        sexpr,
        ')',
    ).parse_next(s);
}

pub fn sexpr(s: &mut &str) -> PResult<SExpr<VarName>> {
    return alt((
        sif, sindex, paren_sexpr, aexpr
    )).parse_next(s);
}