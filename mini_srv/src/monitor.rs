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
) -> Box<dyn Iterator<Item = SExprConstraintStore<IndexedVarName>>> {
    Box::new(inputs.iter().zip(0..).map(|(input, time)| {
        SExprConstraintStore {
            resolved: input
                .iter()
                .map(|(k, v)| (IndexedVarName(k.0.clone(), time), SExpr::Num(*v)))
                .collect(),
            unresolved: Vec::new(),
        }
    }))
}

struct CombinedConstraintIter<'a> {
    iter: Box<dyn Iterator<Item = SExprConstraintStore<IndexedVarName>> + 'a>,
    child_values: Vec<Vec<i64>>,
    keys: Vec<VarName>,
    index: usize,
}

fn combined_iter_explore(iter: &mut CombinedConstraintIter, target_index: usize) {
    for i in iter.index..target_index {
        let new_values = iter.iter.next();

        match new_values {
            Some(new_values) => {
                iter.child_values.push(
                    iter.keys
                        .iter()
                        .map(|k| {
                            match new_values[IndexedVarName(k.0.clone(), i.try_into().unwrap())] {
                                SExpr::Num(val) => val,
                                _ => panic!("Expected solved constraint"),
                            }
                        })
                        .collect(),
                );
                iter.index += 1;
            }
            None => {
                return;
            }
        }
    }
}

struct SingleConstraintIter<'a> {
    parent: Rc<RefCell<CombinedConstraintIter<'a>>>,
    index: usize,
    child_key: VarName,
}

impl<'a> Iterator for SingleConstraintIter<'a> {
    type Item = i64;

    fn next(&mut self) -> Option<Self::Item> {
        let mut parent = self.parent.borrow_mut();
        let child_index = parent
            .keys
            .iter()
            .position(|x| x == &self.child_key)
            .unwrap();
        combined_iter_explore(&mut parent, self.index);

        match parent.child_values.get(self.index) {
            Some(child_values) => {
                let res = child_values[child_index];
                self.index += 1;
                Some(res)
            }
            None => None,
        }
    }
}

fn constraint_stream_to_outputs<'a>(
    output_var_names: &Vec<VarName>,
    constraints: Box<dyn Iterator<Item = SExprConstraintStore<IndexedVarName>> + 'a>,
) -> BTreeMap<VarName, Box<dyn Iterator<Item = i64> + 'a>> {
    let mut res: BTreeMap<VarName, Box<dyn Iterator<Item = i64>>> = BTreeMap::new();

    let combined_iter = Rc::new(RefCell::new(CombinedConstraintIter {
        iter: Box::new(constraints),
        child_values: Vec::new(),
        keys: output_var_names.clone(),
        index: 0,
    }));

    for k in output_var_names {
        let output_iter = SingleConstraintIter {
            parent: combined_iter.clone(),
            index: 0,
            child_key: k.clone(),
        };
        res.insert(k.clone(), Box::new(output_iter));
    }

    res
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
        self.input_streams.publish_inputs(input);
    }

    pub fn iter_constraints(
        &self,
    ) -> impl Iterator<Item = SExprConstraintStore<IndexedVarName>> + '_ {
        MonitorConstraintIter::new(self, &self.input_streams)
    }

    pub fn iter_outputs(&self) -> impl Iterator<Item = BTreeMap<VarName, i64>> + '_ {
        self.iter_constraints().enumerate().map(|(i, cs)| {
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

    pub fn iter_output_iters(&self) -> BTreeMap<VarName, Box<dyn Iterator<Item = i64> + '_>> {
        constraint_stream_to_outputs(&self.output_vars, Box::new(self.iter_constraints()))
    }
}

pub struct MonitorConstraintIter<'a> {
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
            input_stream_iter: inputs_to_constraint_stream(input_streams),
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
