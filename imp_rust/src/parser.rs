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
    return separated_foldr1(
        amult,
        "+", 
        |x, _, y| AExpr::APlus(Box::new(x), Box::new(y)
    )).parse_next(s);
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
    return separated_foldr1(
        aatom,
        "*",
        |x, _, y| AExpr::AMult(Box::new(x), Box::new(y))
    ).parse_next(s);
}

fn amult(s: &mut &str) -> PResult<AExpr> {
    return delimited(whitespace, alt((
        amult_raw, paren_aexpr, anum
    )), whitespace).parse_next(s);
}

fn aminus_raw(s: &mut &str) -> PResult<AExpr> {
    return separated_foldl1(
        aplus,
        "-",
        |x, _, y| AExpr::AMinus(Box::new(x), Box::new(y))
    ).parse_next(s)
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

fn btrue(s: &mut &str) -> PResult<BExpr> {
    return delimited(
        whitespace,
        literal("true"),
        whitespace,
    ).map(|_| BExpr::BTrue).parse_next(s);
}

fn bfalse(s: &mut &str) -> PResult<BExpr> {
    return delimited(
        whitespace,
        literal("false"),
        whitespace,
    ).map(|_| BExpr::BFalse).parse_next(s);
}

fn beq(s: &mut &str) -> PResult<BExpr> {
    return seq!((
        _: whitespace,
        aexpr,
        _: literal('='),
        aexpr,
        _: whitespace,
    ))
    .map(|(a1, a2)| BExpr::BEq(Box::new(a1), Box::new(a2)))
    .parse_next(s);
}

fn ble(s: &mut &str) -> PResult<BExpr> {
    return seq!((
        _: whitespace,
        aexpr,
        _: literal('<'),
        aexpr,
        _: whitespace,
    ))
    .map(|(a1, a2)| BExpr::BLe(Box::new(a1), Box::new(a2)))
    .parse_next(s);
}

fn bnot(s: &mut &str) -> PResult<BExpr> {
    return seq!((
        _: whitespace,
        literal('!'),
        bexpr,
        _: whitespace,
    ))
    .map(|(_, b)| BExpr::BNot(Box::new(b)))
    .parse_next(s);
}

fn band_raw(s: &mut &str) -> PResult<BExpr> {
    return separated_foldr1(
        bexpr,
        "&&",
        |b1, _, b2| BExpr::BAnd(Box::new(b1), Box::new(b2))
    ).parse_next(s);
}

fn band(s: &mut &str) -> PResult<BExpr> {
    return delimited(
        whitespace,
        band_raw, 
        whitespace
    ).parse_next(s);
}

fn bexpr(s: &mut &str) -> PResult<BExpr> {
    return delimited(
        whitespace,
        alt((band, beq, ble, bnot, btrue, bfalse)),
        whitespace
    ).parse_next(s);
}

fn cass(s: &mut &str) -> PResult<Com> {
    return seq!((
        _: whitespace,
        _: "let",
        _: whitespace,
        ident,
        _: "=",
        aexpr,
        _: whitespace,
    ))
    .map(|(x, a)| Com::CAss(x.to_string(), Box::new(a)))
    .parse_next(s);
}

fn cupd(s: &mut &str) -> PResult<Com> {
    return seq!((
        _: whitespace,
        _: "let",
        _: whitespace,
        ident,
        _: "=",
        aexpr,
        _: whitespace,
    ))
    .map(|(x, a)| Com::CAss(x.to_string(), Box::new(a)))
    .parse_next(s);
}

fn cblock(s: &mut &str) -> PResult<Com> {
    return seq!((
        _: whitespace,
        _: "{",
        cseq,
        _: "}",
        _: whitespace,
    ))
    .map(|(c,)| Com::CBlock(Box::new(c)))
    .parse_next(s);
}

fn cstm(s: &mut &str) -> PResult<Com> {
    return alt((
        cass, cupd, cblock, cif, cwhile
    )).parse_next(s);
}

fn cif(s: &mut &str) -> PResult<Com> {
    return seq!((
        _: whitespace,
        _: "if",
        bexpr,
        _: "then",
        cstm,
        _: "else",
        cstm,
        _: whitespace,
    ))
    .map(|(b, c1, c2)| Com::CIf(Box::new(b), Box::new(c1), Box::new(c2)))
    .parse_next(s);
}

fn cwhile(s: &mut &str) -> PResult<Com> {
    return seq!((
        _: whitespace,
        _: "while",
        bexpr,
        _: "do",
        cstm,
        _: whitespace,
    ))
    .map(|(b, c)| Com::CWhile(Box::new(b), Box::new(c)))
    .parse_next(s);
}

fn cseq(s: &mut &str) -> PResult<Com> {
    return seq!((
        _: whitespace,
        separated_foldl1(
            cstm,
            ";",
            |c1, _, c2| Com::CSeq(Box::new(c1), Box::new(c2))
        ),
        _: opt(";"),
        _: whitespace,    
    ))
    .map(|(c,)| c)
    .parse_next(s);
}