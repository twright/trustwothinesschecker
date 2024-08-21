use std::collections::BTreeMap;
use std::iter::zip;

use crate::ast::*;
use crate::constraint_solver::*;

// Temporarily focus on inputs which are streams of integers
#[derive(Clone, Debug)]
struct ValStreamVec(Vec<StreamData>);

pub trait ValStream {
    fn publish(&mut self, data: &StreamData);

    fn iter(&self) -> Box<dyn Iterator<Item = &'_ StreamData> + '_>;
}

impl ValStream for ValStreamVec {
    fn publish(&mut self, data: &StreamData) {
        self.0.push(data.clone());
    }

    fn iter(&self) -> Box<dyn Iterator<Item = &'_ StreamData> + '_> {
        Box::new(self.0.iter())
    }
}

pub struct ValStreamTree(BTreeMap<VarName, ValStreamVec>);

pub trait ValStreamCollection {
    fn publish_inputs(&mut self, input: &BTreeMap<VarName, &StreamData>);

    fn iter(&self) -> impl Iterator<Item = BTreeMap<VarName, &'_ StreamData>> + '_;
}

struct ValStreamIter<'a> {
    keys: Vec<VarName>,
    iters: Vec<Box<dyn Iterator<Item = &'a StreamData> + 'a>>,
}

impl<'a> Iterator for ValStreamIter<'a> {
    type Item = BTreeMap<VarName, &'a StreamData>;

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
    fn publish_inputs(&mut self, input: &BTreeMap<VarName, &StreamData>) {
        for (k, v) in input {
            self.0
                .entry(k.clone())
                .or_insert(ValStreamVec(Vec::new()))
                .publish(v.clone());
        }
    }

    fn iter(&self) -> impl Iterator<Item = BTreeMap<VarName, &'_ StreamData>> + '_ {
        let mut iters: Vec<Box<dyn Iterator<Item = &'_ StreamData> + '_>> = Vec::new();
        for v in self.0.values() {
            iters.push(Box::new(v.0.iter()));
        }
        ValStreamIter::<'_> {
            keys: self.0.keys().cloned().collect(),
            iters,
        }
    }
}

fn inputs_to_constraint_stream(
    inputs: &ValStreamTree,
) -> Box<dyn Iterator<Item = SExprConstraintStore<IndexedVarName>> + '_> {
    Box::new(inputs.iter().enumerate().map(|(time, input)| {
        SExprConstraintStore {
            resolved: input
                .iter()
                .map(|(k, v)| (k.clone().to_indexed(time.try_into().unwrap()), (*v).clone()))
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

    pub fn publish_inputs(&mut self, input: &BTreeMap<VarName, &StreamData>) {
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

    pub fn iter_outputs(&self) -> impl Iterator<Item = BTreeMap<VarName, StreamData>> + '_ {
        self.iter_constraints().enumerate().map(|(i, cs)| {
            let mut res = BTreeMap::new();
            for k in &self.output_vars {
                res.insert(
                    k.clone(),
                    cs.resolved
                        .iter()
                        .find_map(|(v, s)| {
                            if *v == k.to_indexed(i.try_into().unwrap()) {
                                Some(s.clone())
                            } else {
                                None
                            }
                        })
                        .unwrap_or_else(|| StreamData::Unknown),
                );
            }
            res
        })
    }
}

struct MonitorConstraintIter<'a> {
    monitor: &'a ConstraintBasedMonitor,
    constraints: SExprConstraintStore<IndexedVarName>,
    input_stream_iter: Box<dyn Iterator<Item = SExprConstraintStore<IndexedVarName>> + 'a>,
    index: usize,
}

impl<'a> MonitorConstraintIter<'a> {
    fn new(monitor: &'a ConstraintBasedMonitor, input_streams: &'a ValStreamTree) -> Self {
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
                self.constraints.extend(new_constraints);

                // Add new output equations to the constraint store
                self.constraints.extend(to_indexed_constraints(
                    &self.monitor.constraints,
                    // Potential panic from usize to isize conversion
                    self.index.try_into().unwrap(),
                ));

                // Solve the extended constraint system
                self.constraints.solve(self.index);

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
