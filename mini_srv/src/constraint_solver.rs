use std::fmt::Display;

use crate::ast::*;

pub type SExprConstraint<VarT> = (VarT, SExpr<VarT>);

pub struct SExprConstraintStore<VarT> {
    pub resolved: Vec<SExprConstraint<VarT>>,
    pub unresolved: Vec<SExprConstraint<VarT>>,
}

impl<VarT: Display> Display for SExprConstraintStore<VarT> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Resolved: [\n")?;
        for (var, sexpr) in self.resolved.iter() {
            write!(f, "  {} -> {},\n", var, sexpr)?;
        }
        write!(f, "]\nUnresolved: [\n")?;
        for (var, sexpr) in self.unresolved.iter() {
            write!(f, "  {} -> {},\n", var, sexpr)?;
        }
        write!(f, "]\n")
    }
}

impl<VarT: Eq> PartialEq for SExprConstraintStore<VarT> {
    fn eq(&self, other: &Self) -> bool {
        self.resolved == other.resolved && self.unresolved == other.unresolved
    }
}
impl<VarT: Eq> Eq for SExprConstraintStore<VarT> {}

impl<VarT: Clone> Clone for SExprConstraintStore<VarT> {
    fn clone(&self) -> Self {
        SExprConstraintStore {
            resolved: self.resolved.clone(),
            unresolved: self.unresolved.clone(),
        }
    }
}

fn constraint_resolved<VarT>((_, s): &SExprConstraint<VarT>) -> bool {
    sexpr_constant(s)
}

fn constraint_store_sort_resolved<VarT: Clone + Eq>(cs: &mut SExprConstraintStore<VarT>) {
    let new_resolved: Vec<SExprConstraint<VarT>> = cs
        .unresolved
        .iter()
        .filter(|c| constraint_resolved(c))
        .cloned()
        .collect();
    cs.unresolved = cs
        .unresolved
        .iter()
        .filter(|c| !new_resolved.contains(c))
        .cloned()
        .collect();
    cs.resolved.extend(new_resolved);
}

fn constraint_store_sorted_resolved<VarT: Clone + Eq>(
    cs: SExprConstraintStore<VarT>,
) -> SExprConstraintStore<VarT> {
    let new_resolved: Vec<SExprConstraint<VarT>> = cs
        .unresolved
        .iter()
        .filter(|c| constraint_resolved(c))
        .cloned()
        .collect();
    let new_unresolved: Vec<SExprConstraint<VarT>> = cs
        .unresolved
        .iter()
        .filter(|c| !new_resolved.contains(c))
        .cloned()
        .collect();
    SExprConstraintStore {
        resolved: new_resolved,
        unresolved: new_unresolved,
    }
}

fn constraint_store_match<VarT: Clone + Eq>(
    v: &VarT,
    cs: &SExprConstraintStore<VarT>,
) -> Option<SExpr<VarT>> {
    for (var, sexpr) in cs.resolved.iter() {
        if *v == *var {
            return Some(sexpr.clone());
        }
    }

    for (var, sexpr) in cs.unresolved.iter() {
        if *v == *var {
            return Some(sexpr.clone());
        }
    }

    return None;
}

fn to_indexed_bexpr(b: &BExpr<VarName>) -> BExpr<IndexedVarName> {
    use BExpr::*;
    match b {
        Val(b) => Val(*b),
        Eq(a, c) => Eq(
            Box::new(to_indexed_expr(a, 0)),
            Box::new(to_indexed_expr(c, 0)),
        ),
        Le(a, c) => Le(
            Box::new(to_indexed_expr(a, 0)),
            Box::new(to_indexed_expr(c, 0)),
        ),
        Not(b) => Not(Box::new(to_indexed_bexpr(b))),
        And(a, b) => And(Box::new(to_indexed_bexpr(a)), Box::new(to_indexed_bexpr(b))),
        Or(a, b) => Or(Box::new(to_indexed_bexpr(a)), Box::new(to_indexed_bexpr(b))),
    }
}

fn to_indexed_expr(s: &SExpr<VarName>, current_index: i64) -> SExpr<IndexedVarName> {
    use SExpr::*;
    match s {
        Num(n) => Num(*n),
        Plus(a, b) => Plus(
            Box::new(to_indexed_expr(a, current_index)),
            Box::new(to_indexed_expr(b, current_index)),
        ),
        Minus(a, b) => Minus(
            Box::new(to_indexed_expr(a, current_index)),
            Box::new(to_indexed_expr(b, current_index)),
        ),
        Mult(a, b) => Mult(
            Box::new(to_indexed_expr(a, current_index)),
            Box::new(to_indexed_expr(b, current_index)),
        ),
        Var(VarName(v)) => Var(IndexedVarName(v.clone(), current_index)),
        Index(s, i, c) => Index(Box::new(to_indexed_expr(s, current_index)), *i, *c),
        If(b, e1, e2) => If(
            Box::new(to_indexed_bexpr(b)),
            Box::new(to_indexed_expr(s, current_index)),
            Box::new(to_indexed_expr(s, current_index)),
        ),
    }
}

fn to_indexed_constraints(
    cs: &SExprConstraintStore<VarName>,
    current_index: i64,
) -> SExprConstraintStore<IndexedVarName> {
    let resolved: Vec<SExprConstraint<IndexedVarName>> = cs
        .resolved
        .iter()
        .map(|(v, s)| match v {
            VarName(u) => (
                IndexedVarName(u.clone(), current_index),
                to_indexed_expr(s, current_index),
            ),
        })
        .collect();
    let unresolved: Vec<SExprConstraint<IndexedVarName>> = cs
        .unresolved
        .iter()
        .map(|(v, s)| match v {
            VarName(u) => (
                IndexedVarName(u.clone(), current_index),
                to_indexed_expr(s, current_index),
            ),
        })
        .collect();
    SExprConstraintStore {
        resolved,
        unresolved,
    }
}

// Trait for indexing a variable producing a new SExpr
pub trait IndexableVar {
    fn index(&self, i: i64, c: i64) -> SExpr<Self>
    where
        Self: Sized;
}

impl IndexableVar for VarName {
    // For unindexed variables, indexing just produces the same expression
    fn index(&self, i: i64, c: i64) -> SExpr<VarName> {
        SExpr::Index(Box::new(SExpr::Var(self.clone())), i, c)
    }
}

impl IndexableVar for IndexedVarName {
    // For indexed variables, we can actually attempt to change the index on the underlying variable
    fn index(&self, i: i64, c: i64) -> SExpr<IndexedVarName> {
        use SExpr::*;
        match self {
            // If the shifted index is positive, we can just shift the index
            // attached to the variable
            IndexedVarName(name, j) if j + i >= 0 => Var(IndexedVarName(name.clone(), j + i)),
            // If not the indexed variable is replaced with the default value
            IndexedVarName(_, _) => Num(c),
        }
    }
}

pub trait PartialEvaluable<VarT: Eq + Clone + IndexableVar> {
    fn partial_eval(&self, cs: &SExprConstraintStore<VarT>) -> Self;
}

impl<VarT: Eq + Clone + IndexableVar> PartialEvaluable<VarT> for SExpr<VarT> {
    fn partial_eval(&self, cs: &SExprConstraintStore<VarT>) -> Self {
        use SExpr::*;
        match self {
            Num(n) => Num(*n),
            Plus(a, b) => {
                let a_s = a.partial_eval(cs);
                let b_s = b.partial_eval(cs);
                match (a_s, b_s) {
                    (Num(n1), Num(n2)) => Num(n1 + n2),
                    (Num(n1), b) => Plus(Box::new(b), Box::new(Num(n1))).partial_eval(cs),
                    (Plus(a1, b1), c1) => {
                        Plus(a1, Box::new(Plus(b1, Box::new(c1)))).partial_eval(cs)
                    }
                    // Explicitly match the variables to avoid use after move
                    (a_ss, b_ss) => Plus(Box::new(a_ss), Box::new(b_ss)),
                }
            }
            Minus(a, b) => {
                let a_s = a.partial_eval(cs);
                let b_s = b.partial_eval(cs);
                match (a_s, b_s) {
                    (Num(n1), Num(n2)) => Num(n1 - n2),
                    (a_ss, b_ss) => Minus(Box::new(a_ss), Box::new(b_ss)),
                }
            }
            Mult(a, b) => {
                let a_s = a.partial_eval(cs);
                let b_s = b.partial_eval(cs);
                match (a_s, b_s) {
                    (Num(n1), Num(n2)) => Num(n1 * n2),
                    (Num(n1), b) => Mult(Box::new(b.clone()), Box::new(Num(n1))).partial_eval(cs),
                    (Mult(a1, b1), c1) => {
                        Mult(a1, Box::new(Mult(b1, Box::new(c1.clone())))).partial_eval(cs)
                    }
                    (a_ss, b_ss) => Mult(Box::new(a_ss), Box::new(b_ss)),
                }
            }
            If(b, e1, e2) => {
                let b_s = b.partial_eval(cs);
                let e1_s = e1.partial_eval(cs);
                let e2_s = e2.partial_eval(cs);
                match b_s {
                    BExpr::Val(true) => e1_s,
                    BExpr::Val(false) => e2_s,
                    _ if e1_s == e2_s => e1_s,
                    _ => If(Box::new(b_s), Box::new(e1_s), Box::new(e2_s)),
                }
            }
            Index(s, i, c) => {
                let s_s = s.partial_eval(cs);
                match s_s {
                    Var(var) => var.index(*i, *c),
                    Num(n) => Num(n),
                    _ => Index(Box::new(s_s), *i, *c),
                }
            }
            Var(var) => match constraint_store_match(var, &cs) {
                Some(sexpr) => sexpr,
                None => Var(var.clone()),
            },
        }
    }
}

impl<VarT: Eq + Clone + IndexableVar> PartialEvaluable<VarT> for BExpr<VarT> {
    fn partial_eval(&self, cs: &SExprConstraintStore<VarT>) -> Self {
        use BExpr::*;
        match self {
            Val(bool) => Val(*bool),
            Eq(a, b) => {
                let a_s = a.partial_eval(cs);
                let b_s = b.partial_eval(cs);
                match (a_s, b_s) {
                    (SExpr::Num(x1), SExpr::Num(x2)) => Val(x1 == x2),
                    (a_ss, b_ss) => Eq(Box::new(a_ss), Box::new(b_ss)),
                }
            }
            Le(a, b) => {
                let a_s = a.partial_eval(cs);
                let b_s = b.partial_eval(cs);
                match (a_s, b_s) {
                    (SExpr::Num(x1), SExpr::Num(x2)) => Val(x1 <= x2),
                    (a_ss, b_ss) => Le(Box::new(a_ss), Box::new(b_ss)),
                }
            }
            Not(b) => {
                let b_s = b.partial_eval(cs);
                match b_s {
                    Val(b1) => Val(!b1),
                    _ => Not(Box::new(b_s)),
                }
            }
            And(a, b) => {
                let a_s = a.partial_eval(cs);
                let b_s = b.partial_eval(cs);
                match (a_s, b_s) {
                    (Val(b1), Val(b2)) => Val(b1 && b2),
                    (Val(a1), b1) => And(Box::new(b1), Box::new(Val(a1))).partial_eval(cs),
                    (And(a1, b1), c1) => And(a1, Box::new(And(b1, Box::new(c1)))).partial_eval(cs),
                    (a_ss, b_ss) => And(Box::new(a_ss), Box::new(b_ss)),
                }
            }
            Or(a, b) => {
                let a_s = a.partial_eval(cs);
                let b_s = b.partial_eval(cs);
                match (a_s, b_s) {
                    (Val(b1), Val(b2)) => Val(b1 || b2),
                    (Val(a1), b1) => Or(Box::new(b1), Box::new(Val(a1))).partial_eval(cs),
                    (Or(a1, b1), c1) => Or(a1, Box::new(Or(b1, Box::new(c1)))).partial_eval(cs),
                    (a_ss, b_ss) => Or(Box::new(a_ss), Box::new(b_ss)),
                }
            }
        }
    }
}

fn solve_step<VarT: Eq + Clone + IndexableVar>(
    cs: &SExprConstraintStore<VarT>,
) -> SExprConstraintStore<VarT> {
    let unresolved = cs
        .unresolved
        .iter()
        .map(|(v, s)| (v.clone(), s.partial_eval(cs)))
        .collect();
    let resolved = cs
        .resolved
        .iter()
        .map(|(v, s)| (v.clone(), s.partial_eval(cs)))
        .collect();
    let mut cs = SExprConstraintStore {
        unresolved: unresolved,
        resolved: resolved,
    };
    constraint_store_sort_resolved(&mut cs);
    cs
}

pub fn solve_constraints<VarT: Eq + Clone + IndexableVar>(mut cs: &mut SExprConstraintStore<VarT>) {
    loop {
        let cs_new = solve_step(cs);

        if cs_new == *cs {
            return;
        }
        *cs = cs_new;
    }
}

fn add_constraints<VarT: Clone>(
    mut cs: &mut SExprConstraintStore<VarT>,
    cs_new: &SExprConstraintStore<VarT>,
) {
    cs.resolved.extend(cs_new.resolved.clone());
    cs.unresolved.extend(cs_new.unresolved.clone());
}
