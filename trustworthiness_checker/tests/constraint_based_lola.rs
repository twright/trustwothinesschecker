// Test untimed monitoring of LOLA specifications with the async runtime

use futures::stream::StreamExt;
use std::collections::BTreeMap;
use trustworthiness_checker::constraint_based_runtime::ConstraintBasedMonitor;
use trustworthiness_checker::lola_specification;
use trustworthiness_checker::{
    ConcreteStreamData, Monitor, VarName,
};
mod lola_fixtures;
use lola_fixtures::*;

#[tokio::test]
async fn test_simple_add_monitor() {
    let input_streams = input_streams1();
    let spec = lola_specification(&mut spec_simple_add_monitor()).unwrap();
    let mut monitor = ConstraintBasedMonitor::new(spec, input_streams);
    let outputs: Vec<(usize, BTreeMap<VarName, ConcreteStreamData>)> =
        monitor.monitor_outputs().enumerate().collect().await;
    assert_eq!(
        outputs,
        vec![
            (
                0,
                vec![(VarName("z".into()), ConcreteStreamData::Int(3))]
                    .into_iter()
                    .collect(),
            ),
            (
                1,
                vec![(VarName("z".into()), ConcreteStreamData::Int(7))]
                    .into_iter()
                    .collect(),
            ),
        ]
    );
}

#[ignore = "currently we can't handle recursive constraints in the solver as need a way to handle the inner indexes"]
#[tokio::test]
async fn test_count_monitor() {
    let input_streams = input_streams1();
    let spec = lola_specification(&mut spec_count_monitor()).unwrap();
    let mut monitor = ConstraintBasedMonitor::new(spec, input_streams);
    let outputs: Vec<(usize, BTreeMap<VarName, ConcreteStreamData>)> = monitor
        .monitor_outputs()
        .take(5)
        .enumerate()
        .collect()
        .await;
    assert_eq!(
        outputs,
        vec![
            (
                0,
                vec![(VarName("x".into()), ConcreteStreamData::Int(1))]
                    .into_iter()
                    .collect(),
            ),
            (
                1,
                vec![(VarName("x".into()), ConcreteStreamData::Int(2))]
                    .into_iter()
                    .collect(),
            ),
            (
                2,
                vec![(VarName("x".into()), ConcreteStreamData::Int(3))]
                    .into_iter()
                    .collect(),
            ),
            (
                3,
                vec![(VarName("x".into()), ConcreteStreamData::Int(4))]
                    .into_iter()
                    .collect(),
            ),
            (
                4,
                vec![(VarName("x".into()), ConcreteStreamData::Int(5))]
                    .into_iter()
                    .collect(),
            ),
        ]
    );
}

#[ignore = "currently we can't handle recursive constraints in the solver as need a way to handle the inner indexes"]
#[tokio::test]
async fn test_eval_monitor() {
    let input_streams = input_streams2();
    let spec = lola_specification(&mut spec_eval_monitor()).unwrap();
    let mut monitor = ConstraintBasedMonitor::new(spec, input_streams);
    let outputs: Vec<(usize, BTreeMap<VarName, ConcreteStreamData>)> = monitor
        .monitor_outputs()
        .enumerate()
        .collect()
        .await;
    assert_eq!(
        outputs,
        vec![
            (
                0,
                vec![
                    (VarName("z".into()), ConcreteStreamData::Int(3)),
                    (VarName("w".into()), ConcreteStreamData::Int(3))
                ]
                .into_iter()
                .collect(),
            ),
            (
                1,
                vec![
                    (VarName("z".into()), ConcreteStreamData::Int(7)),
                    (VarName("w".into()), ConcreteStreamData::Int(7))
                ]
                .into_iter()
                .collect(),
            ),
        ]
    );
}
