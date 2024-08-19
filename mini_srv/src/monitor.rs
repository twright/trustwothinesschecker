use core::time;
use std::collections::BTreeMap;
use std::iter::zip;
use std::marker::PhantomData;
use std::mem;

use crate::ast::*;
use crate::constraint_solver::*;

// Temporarily focus on inputs which are streams of integers
#[derive(Clone, Debug)]
struct InputStreamVec(Vec<i64>);

pub trait InputStream {
    fn publish(&mut self, data: i64);

    fn iter(&self) -> Box<dyn Iterator<Item = i64> + '_>;
}

impl InputStream for InputStreamVec {
    fn publish(&mut self, data: i64) {
        self.0.push(data);
    }

    fn iter(&self) -> Box<dyn Iterator<Item = i64> + '_> {
        Box::new(self.0.iter().copied())
    }
}

pub struct InputStreamTree(BTreeMap<VarName, InputStreamVec>);

pub trait InputStreamCollection {
    fn publish_inputs(&mut self, input: &BTreeMap<VarName, i64>);

    fn iter(&self) -> Box<dyn Iterator<Item = BTreeMap<VarName, i64>>>;
}

struct InputStreamIter {
    keys: Vec<VarName>,
    iters: Vec<Box<dyn Iterator<Item = i64>>>,
}

impl Iterator for InputStreamIter {
    type Item = BTreeMap<VarName, i64>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut res = BTreeMap::new();

        for (k, v) in zip(&self.keys, &mut self.iters) {
            match v.next() {
                Some(val) => {
                    res.insert(k.clone(), val);
                }
                None => {
                    return None;
                }
            }
        }

        Some(res)
    }
}

impl InputStreamCollection for InputStreamTree {
    fn publish_inputs(&mut self, input: &BTreeMap<VarName, i64>) {
        for (k, v) in input {
            self.0
                .entry(k.clone())
                .or_insert(InputStreamVec(Vec::new()))
                .publish(*v);
        }
    }

    fn iter(&self) -> Box<dyn Iterator<Item = BTreeMap<VarName, i64>>> {
        let mut iters: Vec<Box<dyn Iterator<Item = i64>>> = Vec::new();
        for v in self.0.values() {
            iters.push(Box::new(v.0.clone().into_iter()));
        }
        Box::new(InputStreamIter {
            keys: self.0.keys().cloned().collect(),
            iters,
        })
    }
}

fn inputs_to_constraint_stream(
    inputs: &dyn InputStreamCollection,
) -> Box<dyn Iterator<Item = SExprConstraintStore<IndexedVarName>>> {
    Box::new(inputs.iter().zip(0..).map(|(input, time)| {
        SExprConstraintStore {
            resolved: input
                .iter()
                .map(
                    |(k, v)|
                    (   
                        IndexedVarName(k.0.clone(), time),
                        SExpr::Num(*v)
                    )
                )
                .collect(),
            unresolved: Vec::new(),
        }
    }))
}
