use std::collections::HashMap;

mod ast;
mod parser;
use ast::*;
mod constraint_solver;
use constraint_solver::*;
mod monitor;
use monitor::*;

fn main() {
    let mut cs: SExprConstraintStore<IndexedVarName> = SExprConstraintStore {
        resolved: Vec::new(),
        unresolved: Vec::new(),
    };
    let expr = SExpr::Plus(Box::new(SExpr::Num(1)), Box::new(SExpr::Num(2)));
    println!("{}", expr.partial_eval(&cs));
    let mut cs1 = SExprConstraintStore {
        resolved: vec![
            (IndexedVarName("x".into(), 0), SExpr::Num(1)),
            (IndexedVarName("y".into(), 0), SExpr::Num(2)),
        ],
        unresolved: vec![(
            IndexedVarName("z".into(), 0),
            SExpr::Plus(
                Box::new(SExpr::Var(IndexedVarName("x".into(), 0))),
                Box::new(SExpr::Var(IndexedVarName("y".into(), 0))),
            ),
        )],
    };
    println!("{}", cs1);
    solve_constraints(&mut cs1);
    println!("{}", cs1);
    let mut cs2 = SExprConstraintStore {
        resolved: vec![
            (IndexedVarName("x".into(), 0), SExpr::Num(1)),
            (IndexedVarName("x".into(), 1), SExpr::Num(2)),
        ],
        unresolved: vec![(
            IndexedVarName("y".into(), 2),
            SExpr::Plus(
                Box::new(SExpr::Index(
                    Box::new(SExpr::Var(IndexedVarName("x".into(), 2))),
                    -1,
                    0,
                )),
                Box::new(SExpr::Index(
                    Box::new(SExpr::Var(IndexedVarName("x".into(), 2))),
                    -2,
                    0,
                )),
            ),
        )],
    };
    println!("{}", cs2);
    solve_constraints(&mut cs2);
    println!("{}", cs2);
}
