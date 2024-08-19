use std::collections::{BTreeMap, HashMap};

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

    let cs3 = SExprConstraintStore {
        resolved: vec![],
        unresolved: vec![(
            VarName("z".into()),
            SExpr::Plus(
                Box::new(SExpr::Var(VarName("x".into()))),
                Box::new(SExpr::Var(VarName("y".into()))),
            ),
        )],
    };
    let inputs1 =
        BTreeMap::from_iter(vec![(VarName("x".into()), 1), (VarName("y".into()), 2)].into_iter());
    let inputs2 =
        BTreeMap::from_iter(vec![(VarName("x".into()), 3), (VarName("y".into()), 4)].into_iter());
    let mut monitor = ConstraintBasedMonitor::new(
        vec![VarName("x".into()), VarName("y".into())],
        vec![VarName("z".into())],
        cs3,
    );
    for (cs, i) in monitor.iter_constraints().zip(0..3) {
        println!("Step {}:\n{}", i, cs);
    }
    monitor.publish_inputs(&inputs1);
    for (cs, i) in monitor.iter_constraints().zip(0..3) {
        println!("Step {}:\n{}", i, cs);
    }
    monitor.publish_inputs(&inputs2);
    for (cs, i) in monitor.iter_constraints().zip(0..3) {
        println!("Step {}:\n{}", i, cs);
    }

    for (i, x) in monitor.iter_outputs().enumerate() {
        println!("z[{}] = {:?}", i, x[&VarName("z".into())]);
    }
}
