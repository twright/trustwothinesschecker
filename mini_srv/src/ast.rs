use std::{collections::BTreeMap, fmt::Display};

use crate::core::{IndexedVarName, Monitor, StreamData, StreamExpr, VarName};

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
        isize,
        // Default c
        StreamData,
    ),

    // Arithmetic Stream expression
    Val(StreamData),
    Plus(Box<SExpr<VarT>>, Box<SExpr<VarT>>),
    Minus(Box<SExpr<VarT>>, Box<SExpr<VarT>>),
    Mult(Box<SExpr<VarT>>, Box<SExpr<VarT>>),
    Var(VarT),

    // Eval
    Eval(Box<SExpr<VarT>>),
}

impl StreamExpr for SExpr<VarName> {
    fn var(var: &VarName) -> Self {
        SExpr::Var(var.clone())
    }
}

pub struct LOLAMonitor {
    pub input_vars: Vec<VarName>,
    pub output_vars: Vec<VarName>,
    pub exprs: BTreeMap<VarName, SExpr<VarName>>,
}

impl Monitor<SExpr<VarName>> for LOLAMonitor {
    fn input_vars(&self) -> Vec<VarName> {
        self.input_vars.clone()
    }

    fn output_vars(&self) -> Vec<VarName> {
        self.output_vars.clone()
    }

    fn var_expr(&self, var: &VarName) -> Option<SExpr<VarName>> {
        Some(self.exprs.get(var)?.clone())
    }
}

impl VarName {
    pub fn to_indexed(&self, i: usize) -> IndexedVarName {
        IndexedVarName(self.0.clone(), i)
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
            SExpr::Val(n) => write!(f, "{}", n),
            SExpr::Plus(e1, e2) => write!(f, "({} + {})", e1, e2),
            SExpr::Minus(e1, e2) => write!(f, "({} - {})", e1, e2),
            SExpr::Mult(e1, e2) => write!(f, "({} * {})", e1, e2),
            SExpr::Var(v) => write!(f, "{}", v),
            SExpr::Eval(e) => write!(f, "eval({})", e),
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

// Trait for indexing a variable producing a new SExpr
pub trait IndexableVar {
    fn index(&self, i: isize, c: &StreamData) -> SExpr<Self>
    where
        Self: Sized;
}

impl IndexableVar for VarName {
    // For unindexed variables, indexing just produces the same expression
    fn index(&self, i: isize, c: &StreamData) -> SExpr<VarName> {
        SExpr::Index(Box::new(SExpr::Var(self.clone())), i, c.clone())
    }
}

impl IndexableVar for IndexedVarName {
    // For indexed variables, we can actually attempt to change the index on the underlying variable
    fn index(&self, i: isize, c: &StreamData) -> SExpr<IndexedVarName> {
        use SExpr::*;
        match self {
            // If the shifted index is positive, we can just shift the index
            // attached to the variable
            IndexedVarName(name, j) if i.wrapping_add_unsigned(*j) >= 0 => Var(IndexedVarName(
                name.clone(),
                i.wrapping_add_unsigned(*j) as usize,
            )),
            // If not the indexed variable is replaced with the default value
            IndexedVarName(_, _) => Val(c.clone()),
        }
    }
}
