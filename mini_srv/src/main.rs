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
    let expr = SExpr::Plus(
        Box::new(SExpr::Val(StreamData::Int(1))),
        Box::new(SExpr::Val(StreamData::Int((2)))),
    );
    println!("{}", expr.partial_eval(&cs, 0));
    let mut cs1 = SExprConstraintStore {
        resolved: vec![
            (IndexedVarName("x".into(), 0), StreamData::Int(1)),
            (IndexedVarName("y".into(), 0), StreamData::Int(2)),
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
    cs1.solve(0);
    println!("{}", cs1);
    let mut cs2 = SExprConstraintStore {
        resolved: vec![
            (IndexedVarName("x".into(), 0), StreamData::Int(1)),
            (IndexedVarName("x".into(), 1), StreamData::Int(2)),
        ],
        unresolved: vec![(
            IndexedVarName("y".into(), 2),
            SExpr::Plus(
                Box::new(SExpr::Index(
                    Box::new(SExpr::Var(IndexedVarName("x".into(), 2))),
                    -1,
                    StreamData::Int(0),
                )),
                Box::new(SExpr::Index(
                    Box::new(SExpr::Var(IndexedVarName("x".into(), 2))),
                    -2,
                    StreamData::Int(0),
                )),
            ),
        )],
    };
    println!("{}", cs2);
    cs2.solve(0);
    println!("{}", cs2);

    let cs3 = SExprConstraintStore {
        resolved: vec![],
        unresolved: vec![(
            VarName("z".into()),
            SExpr::Plus(
                Box::new(SExpr::Var(VarName("x".into()))),
                Box::new(SExpr::Var(VarName("y".into()))),
            ),
        ),
        (
            VarName("w".into()),
            SExpr::Eval(Box::new(SExpr::Var(VarName("z".into())))),
        ),
        ],
    };
    let binding = StreamData::Str("x + y".to_string());
    let inputs1 =
        BTreeMap::from_iter(vec![(VarName("x".into()), &StreamData::Int(1)), (VarName("y".into()), &StreamData::Int(2)), (VarName("s".into()), &binding), ].into_iter());
    let binding = StreamData::Str("x - y".to_string());
    let inputs2 =
        BTreeMap::from_iter(vec![(VarName("x".into()), &StreamData::Int(3)), (VarName("y".into()), &StreamData::Int(4)), (VarName("s".into()), &binding)].into_iter());
    let mut monitor = ConstraintBasedMonitor::new(
        vec![VarName("x".into()), VarName("y".into()), VarName("s".into())],
        vec![VarName("z".into()), VarName("w".into())],
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

    for (i, x) in monitor.iter_outputs().enumerate() {
        println!("w[{}] = {:?}", i, x[&VarName("z".into())]);
    }
}
