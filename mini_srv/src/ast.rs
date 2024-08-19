use std::fmt::Display;
use std::rc::{Rc, Weak};

use winnow::stream::Stream;

#[derive(Clone, Eq, PartialEq)]
pub enum StreamData {
    Int(i64),
    Str(Box<str>),
    Bool(bool),
    Unit,
}

impl Display for StreamData {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            StreamData::Int(n) => write!(f, "{}", n),
            StreamData::Bool(b) => write!(f, "{}", if *b { "true" } else { "false" }),
            StreamData::Str(s) => write!(f, "{}", s.to_string()),
            StreamData::Unit => write!(f, "unit"),
        }
    }
}

// Could also do this with async steams
// trait InputStream = Iterator<Item = StreamData>;

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct VarName(pub Box<str>);

#[derive(Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct IndexedVarName(pub Box<str>, pub i64);

#[derive(Clone, PartialEq, Eq)]
pub enum BExpr<VarT> {
    Val(bool),
    Eq(Box<SExpr<VarT>>, Box<SExpr<VarT>>),
    Le(Box<SExpr<VarT>>, Box<SExpr<VarT>>),
    Not(Box<BExpr<VarT>>),
    And(Box<BExpr<VarT>>, Box<BExpr<VarT>>),
    Or(Box<BExpr<VarT>>, Box<BExpr<VarT>>),
}

#[derive(Clone, PartialEq, Eq)]
pub enum SExpr<VarT> {
    // if-then-else
    If(Box<BExpr<VarT>>, Box<SExpr<VarT>>, Box<SExpr<VarT>>),

    // Stream indexing
    Index(
        // Inner SExpr e
        Box<SExpr<VarT>>,
        // Index i
        i64,
        // Default c
        i64,
    ),

    // Arithmetic Stream expression
    Num(i64),
    Plus(Box<SExpr<VarT>>, Box<SExpr<VarT>>),
    Minus(Box<SExpr<VarT>>, Box<SExpr<VarT>>),
    Mult(Box<SExpr<VarT>>, Box<SExpr<VarT>>),
    Var(VarT),
}

pub fn sexpr_constant<VarT>(s: &SExpr<VarT>) -> bool {
    matches!(s, SExpr::Num(_))
}

impl Display for VarName {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl Display for IndexedVarName {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let IndexedVarName(name, index) = self;
        write!(f, "{}[{}]", name, index)
    }
}

impl<VarT: Display> Display for SExpr<VarT> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            SExpr::If(b, e1, e2) => write!(f, "if {} then {} else {}", b, e1, e2),
            SExpr::Index(s, i, c) => write!(f, "{}[{},{}]", s, i, c),
            SExpr::Num(n) => write!(f, "{}", n),
            SExpr::Plus(e1, e2) => write!(f, "({} + {})", e1, e2),
            SExpr::Minus(e1, e2) => write!(f, "({} - {})", e1, e2),
            SExpr::Mult(e1, e2) => write!(f, "({} * {})", e1, e2),
            SExpr::Var(v) => write!(f, "{}", v),
        }
    }
}

impl<VarT: Display> Display for BExpr<VarT> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BExpr::Val(b) => write!(f, "{}", if *b { "true" } else { "false" }),
            BExpr::Eq(e1, e2) => write!(f, "({} == {})", e1, e2),
            BExpr::Le(e1, e2) => write!(f, "({} <= {})", e1, e2),
            BExpr::Not(b) => write!(f, "!{}", b),
            BExpr::And(b1, b2) => write!(f, "({} && {})", b1, b2),
            BExpr::Or(b1, b2) => write!(f, "({} || {})", b1, b2),
        }
    }
}
