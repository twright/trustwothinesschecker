use core::time;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::iter::zip;
use std::marker::PhantomData;
use std::mem;
use std::rc::Rc;
use std::rc::Weak;

use crate::ast::*;
use crate::constraint_solver::*;

// Temporarily focus on inputs which are streams of integers
#[derive(Clone, Debug)]
struct ValStreamVec(Vec<i64>);

pub trait ValStream {
    fn publish(&mut self, data: i64);

    fn iter(&self) -> Box<dyn Iterator<Item = i64> + '_>;
}

impl ValStream for ValStreamVec {
    fn publish(&mut self, data: i64) {
        self.0.push(data);
    }

    fn iter(&self) -> Box<dyn Iterator<Item = i64> + '_> {
        Box::new(self.0.iter().copied())
    }
}

pub struct ValStreamTree(BTreeMap<VarName, ValStreamVec>);

pub trait ValStreamCollection {
    fn publish_inputs(&mut self, input: &BTreeMap<VarName, i64>);

    fn iter(&self) -> Box<dyn Iterator<Item = BTreeMap<VarName, i64>>>;
}

struct ValStreamIter {
    keys: Vec<VarName>,
    iters: Vec<Box<dyn Iterator<Item = i64>>>,
}

impl Iterator for ValStreamIter {
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

impl ValStreamCollection for ValStreamTree {
    fn publish_inputs(&mut self, input: &BTreeMap<VarName, i64>) {
        for (k, v) in input {
            self.0
                .entry(k.clone())
                .or_insert(ValStreamVec(Vec::new()))
                .publish(*v);
        }
    }

    fn iter(&self) -> Box<dyn Iterator<Item = BTreeMap<VarName, i64>>> {
        let mut iters: Vec<Box<dyn Iterator<Item = i64>>> = Vec::new();
        for v in self.0.values() {
            iters.push(Box::new(v.0.clone().into_iter()));
        }
        Box::new(ValStreamIter {
            keys: self.0.keys().cloned().collect(),
            iters,
        })
    }
}

fn inputs_to_constraint_stream(
    inputs: &dyn ValStreamCollection,
) -> impl Iterator<Item = SExprConstraintStore<IndexedVarName>> {
    Box::new(inputs.iter().enumerate().map(|(time, input)| {
        SExprConstraintStore {
            resolved: input
                .iter()
                .map(|(k, v)| {
                    (
                        IndexedVarName(k.0.clone(), time.try_into().unwrap()),
                        SExpr::Num(*v),
                    )
                })
                .collect(),
            unresolved: Vec::new(),
        }
    }))
}

pub struct ConstraintBasedMonitor {
    input_streams: ValStreamTree,
    constraints: SExprConstraintStore<VarName>,
    input_vars: Vec<VarName>,
    output_vars: Vec<VarName>,
    input_index: usize,
}

impl ConstraintBasedMonitor {
    pub fn new(
        input_vars: Vec<VarName>,
        output_vars: Vec<VarName>,
        constraints: SExprConstraintStore<VarName>,
    ) -> Self {
        ConstraintBasedMonitor {
            input_streams: ValStreamTree(BTreeMap::new()),
            constraints,
            input_vars,
            output_vars,
            input_index: 0,
        }
    }

    pub fn publish_inputs(&mut self, input: &BTreeMap<VarName, i64>) {
        for k in input.keys() {
            if !self.input_vars.contains(k) {
                panic!("Unexpected input variable");
            }
        }
        self.input_index += 1;
        self.input_streams.publish_inputs(input);
    }

    pub fn iter_constraints(
        &self,
    ) -> impl Iterator<Item = SExprConstraintStore<IndexedVarName>> + '_ {
        MonitorConstraintIter::new(self, &self.input_streams)
    }

    pub fn iter_outputs(&self) -> impl Iterator<Item = BTreeMap<VarName, i64>> + '_ {
        self.iter_constraints().enumerate().map(move |(i, cs)| {
            let mut res = BTreeMap::new();
            for k in &self.output_vars {
                match cs[IndexedVarName(k.0.clone(), i.try_into().unwrap())] {
                    SExpr::Num(val) => {
                        res.insert(k.clone(), val);
                    }
                    _ => panic!("Expected solved constraint"),
                }
            }
            res
        })
    }
}

struct MonitorConstraintIter<'a> {
    monitor: &'a ConstraintBasedMonitor,
    constraints: SExprConstraintStore<IndexedVarName>,
    input_stream_iter: Box<dyn Iterator<Item = SExprConstraintStore<IndexedVarName>>>,
    index: usize,
}

impl<'a> MonitorConstraintIter<'a> {
    fn new(monitor: &'a ConstraintBasedMonitor, input_streams: &ValStreamTree) -> Self {
        MonitorConstraintIter {
            monitor,
            constraints: SExprConstraintStore {
                resolved: Vec::new(),
                unresolved: Vec::new(),
            },
            input_stream_iter: Box::new(inputs_to_constraint_stream(input_streams)),
            index: 0,
        }
    }
}

impl<'a> Iterator for MonitorConstraintIter<'a> {
    type Item = SExprConstraintStore<IndexedVarName>;

    fn next(&mut self) -> Option<Self::Item> {
        let new_constraints = self.input_stream_iter.next();

        match new_constraints {
            Some(new_constraints) => {
                // Add the new contraints to the constraint store
                add_constraints(&mut self.constraints, &new_constraints);

                // Add new output equations to the constraint store
                add_constraints(
                    &mut self.constraints,
                    &to_indexed_constraints(
                        &self.monitor.constraints,
                        // Potential panic from mismatch between indexes and
                        // input stream lengths
                        self.index.try_into().unwrap(),
                    ),
                );

                // Solve the extended constraint system
                solve_constraints(&mut self.constraints);

                // Increment the output index
                self.index += 1;

                Some(self.constraints.clone())
            }
            None => {
                return None;
            }
        }
    }
}
