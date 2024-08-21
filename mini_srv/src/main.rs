use std::{
    collections::{BTreeMap, HashMap},
    pin::Pin,
};

mod ast;
mod parser;
use ast::*;
mod constraint_solver;
use constraint_solver::*;
mod monitor;
use futures::{stream, StreamExt};
use monitor::*;

#[tokio::main]
async fn main() {
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
        unresolved: vec![
            (
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
    let mut input_streams: BTreeMap<
        _,
        Pin<Box<dyn futures::Stream<Item = ast::StreamData> + std::marker::Send>>,
    > = BTreeMap::new();
    input_streams.insert(
        VarName("x".into()),
        Box::pin(stream::iter(
            vec![StreamData::Int(1), StreamData::Int(3)].into_iter(),
        )) as Pin<Box<dyn futures::Stream<Item = ast::StreamData> + std::marker::Send>>,
    );
    input_streams.insert(
        VarName("y".into()),
        Box::pin(stream::iter(
            vec![StreamData::Int(2), StreamData::Int(4)].into_iter(),
        )) as Pin<Box<dyn futures::Stream<Item = ast::StreamData> + std::marker::Send>>,
    );
    input_streams.insert(
        VarName("s".into()),
        Box::pin(stream::iter(
            vec![
                StreamData::Str("x+y".to_string()),
                StreamData::Str("x+y".to_string()),
            ]
            .into_iter(),
        )) as Pin<Box<dyn futures::Stream<Item = ast::StreamData> + std::marker::Send>>,
    );
    let mut input_streams2: BTreeMap<
        _,
        Pin<Box<dyn futures::Stream<Item = ast::StreamData> + std::marker::Send>>,
    > = BTreeMap::new();
    input_streams2.insert(
        VarName("x".into()),
        Box::pin(stream::iter(
            vec![StreamData::Int(1), StreamData::Int(3)].into_iter(),
        )) as Pin<Box<dyn futures::Stream<Item = ast::StreamData> + std::marker::Send>>,
    );
    input_streams2.insert(
        VarName("y".into()),
        Box::pin(stream::iter(
            vec![StreamData::Int(2), StreamData::Int(4)].into_iter(),
        )) as Pin<Box<dyn futures::Stream<Item = ast::StreamData> + std::marker::Send>>,
    );
    input_streams.insert(
        VarName("s".into()),
        Box::pin(stream::iter(
            vec![
                StreamData::Str("x+y".to_string()),
                StreamData::Str("x+y".to_string()),
            ]
            .into_iter(),
        )) as Pin<Box<dyn futures::Stream<Item = ast::StreamData> + std::marker::Send>>,
    );
    let binding = ValStreamCollection(input_streams);
    let mut monitor = ConstraintBasedMonitor::new(
        vec![
            VarName("x".into()),
            VarName("y".into()),
            VarName("s".into()),
        ],
        vec![VarName("z".into()), VarName("w".into())],
        cs3.clone(),
        binding,
    );
    let mut constraints = monitor.monitor_constraints().enumerate();
    while let Some((i, cs)) = constraints.next().await {
        println!("Step {}:\n{}", i, cs);
    }
    let binding = ValStreamCollection(input_streams2);
    let mut monitor2 = ConstraintBasedMonitor::new(
        vec![
            VarName("x".into()),
            VarName("y".into()),
            VarName("s".into()),
        ],
        vec![VarName("z".into()), VarName("w".into())],
        cs3.clone(),
        binding,
    );
    let mut outputs = monitor2.monitor_outputs().enumerate();
    while let Some((i, output)) = outputs.next().await {
        println!("z[{}]: {}", i, output.get(&VarName("z".into())).unwrap());
        println!("w[{}]: {}", i, output.get(&VarName("w".into())).unwrap());
    }
}
