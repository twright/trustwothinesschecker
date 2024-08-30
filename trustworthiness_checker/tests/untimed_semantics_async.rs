// Test untimed monitoring of LOLA specifications with the async runtime

use futures::stream;
use futures::stream::{BoxStream, StreamExt};
use std::{collections::BTreeMap, pin::Pin};
use trustworthiness_checker::UntimedLolaSemantics;
use trustworthiness_checker::{
    ast::{LOLASpecification, SExpr},
    async_runtime::AsyncMonitorRunner,
    Monitor, StreamData, VarName,
};
mod lola_fixtures;
use lola_fixtures::*;

#[tokio::test]
async fn test_simple_add_monitor() {
    let input_streams = input_streams1();
    let spec = spec_simple_add_monitor();
    let mut async_monitor =
        AsyncMonitorRunner::<_, UntimedLolaSemantics, _>::new(spec, input_streams);
    let outputs: Vec<(usize, BTreeMap<VarName, StreamData>)> = async_monitor
        .monitor_outputs()
        .take(2)
        .enumerate()
        .collect()
        .await;
    assert_eq!(
        outputs,
        vec![
            (
                0,
                vec![(VarName("z".into()), StreamData::Int(3))]
                    .into_iter()
                    .collect(),
            ),
            (
                1,
                vec![(VarName("z".into()), StreamData::Int(7))]
                    .into_iter()
                    .collect(),
            ),
        ]
    );
}

#[tokio::test]
async fn test_count_monitor() {
    let input_streams: BTreeMap<VarName, BoxStream<'static, StreamData>> = BTreeMap::new();
    let spec = spec_count_monitor();
    let mut async_monitor =
        AsyncMonitorRunner::<_, UntimedLolaSemantics, _>::new(spec, input_streams);
    let outputs: Vec<(usize, BTreeMap<VarName, StreamData>)> = async_monitor
        .monitor_outputs()
        .take(4)
        .enumerate()
        .collect()
        .await;
    assert_eq!(
        outputs,
        vec![
            (
                0,
                vec![(VarName("x".into()), StreamData::Int(1))]
                    .into_iter()
                    .collect(),
            ),
            (
                1,
                vec![(VarName("x".into()), StreamData::Int(2))]
                    .into_iter()
                    .collect(),
            ),
            (
                2,
                vec![(VarName("x".into()), StreamData::Int(3))]
                    .into_iter()
                    .collect(),
            ),
            (
                3,
                vec![(VarName("x".into()), StreamData::Int(4))]
                    .into_iter()
                    .collect(),
            ),
        ]
    );
}

#[ignore = "will currently deadlock due to need to provide streams into the eval operator"]
#[tokio::test]
async fn test_eval_monitor() {
    let input_streams = input_streams2();
    let spec = spec_eval_monitor();
    let mut async_monitor =
        AsyncMonitorRunner::<_, UntimedLolaSemantics, _>::new(spec, input_streams);
    let outputs: Vec<(usize, BTreeMap<VarName, StreamData>)> =
        async_monitor.monitor_outputs().take(2).enumerate().collect().await;
    assert_eq!(
        outputs,
        vec![
            (
                0,
                vec![
                    (VarName("z".into()), StreamData::Int(3)),
                    (VarName("w".into()), StreamData::Int(3))
                ]
                .into_iter()
                .collect(),
            ),
            (
                1,
                vec![
                    (VarName("z".into()), StreamData::Int(7)),
                    (VarName("w".into()), StreamData::Int(7))
                ]
                .into_iter()
                .collect(),
            ),
        ]
    );
}
