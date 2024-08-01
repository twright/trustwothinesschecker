use std::collections::HashMap;
use std::collections::VecDeque;

use winnow::combinator::Take;

use crate::ast::*;

pub type VarStackImp = VecDeque<HashMap<String, ProgData>>;

pub trait VarStack {
    fn empty() -> Self;
    fn lookup(&self, x: &String) -> ProgData;
    fn set(&mut self, x: String, n: ProgData);
    fn assign_new(&mut self, x: String, n: ProgData);
    fn grow(&mut self);
    fn pop(&mut self);
}

impl VarStack for VarStackImp {
    fn empty() -> Self {
        return VecDeque::from(vec![HashMap::new()]);
    }

    fn lookup(&self, x: &String) -> ProgData {
        for map in self.iter().rev() {
            if map.contains_key(x) {
                return map[x].clone();
            }
        }
        panic!("Variable not found");
    }

    fn set(&mut self, x: String, n: ProgData) {
        for map in self.iter_mut() {
            if map.contains_key(&x) {
                map.entry(x).and_modify(|e| *e = n);
                return;
            }
        }

        self[0].entry(x).or_insert(n);
        return;
    }

    fn assign_new(&mut self, x: String, n: ProgData) {
        self[0].entry(x).or_insert(n);
        return;
    }

    fn grow(&mut self) {
        self.push_front(HashMap::new());
    }

    fn pop(&mut self) {
        self.pop_front();
    }
}

pub trait Evaluatable<T> {
    fn eval(&self, vs: &mut VarStackImp) ->
        T;
}

impl Evaluatable<ProgData> for AExpr {
    fn eval(&self, vs: &mut VarStackImp) -> ProgData {
        match self {
            AExpr::ANum(n) => {
                ProgData::TInt(*n)
            },
            AExpr::APlus(a1, a2) => {
                match (a1.eval(vs), a2.eval(vs)) {
                    (ProgData::TInt(n1), ProgData::TInt(n2)) => {
                        ProgData::TInt(n1 + n2)
                    },
                    _ => panic!("Type error in APlus")
                }
            },
            AExpr::AMinus(a1, a2) => {
                match (a1.eval(vs), a2.eval(vs)) {
                    (ProgData::TInt(n1), ProgData::TInt(n2)) => {
                        ProgData::TInt(n1 - n2)
                    },
                    _ => panic!("Type error in AMinus")
                    
                }
            },
            AExpr::AMult(a1, a2) => {
                match (a1.eval(vs), a2.eval(vs)) {
                    (ProgData::TInt(n1), ProgData::TInt(n2)) => {
                        ProgData::TInt(n1 * n2)
                    },
                    _ => panic!("Type error in AMult")
                    
                }
            },
            AExpr::AVar(x) => {
                vs.lookup(x)
            },
            AExpr::ACall(f, args) => {
                match vs.lookup(f) {
                    ProgData::TFun(params, body) => {
                        let mut new_vs = VarStackImp::empty();
                        for (param, arg) in params.iter().zip(args.iter()) {
                            new_vs.assign_new(
                                param.to_string(),
                                arg.eval(vs)
                            );
                        }
                        body.eval(&mut new_vs)
                    },
                    _ => panic!("Type error in ACall")
                }
            },
            AExpr::AFun(params, body) => {
                ProgData::TFun(params.clone(), body.clone())
            },
        }
    }
}

impl Evaluatable<bool> for BExpr {
    fn eval(&self, vs: &mut VarStackImp) -> bool {
        match self {
            BExpr::BTrue => true,
            BExpr::BFalse => false,
            BExpr::BEq(a1, a2) => {
                match (a1.eval(vs), a2.eval(vs)) {
                    (ProgData::TInt(n1), ProgData::TInt(n2)) => {
                        n1 == n2
                    },
                    _ => panic!("Type error in BEq")
                }
            },
            BExpr::BLe(a1, a2) => {
                match (a1.eval(vs), a2.eval(vs)) {
                    (ProgData::TInt(n1), ProgData::TInt(n2)) => {
                        n1 <= n2
                    },
                    _ => panic!("Type error in BLe")
                }
            },
            BExpr::BNot(b) => {
                !b.eval(vs)
            },
            BExpr::BAnd(b1, b2) => {
                b1.eval(vs) && b2.eval(vs)
            },
        }
    }
}

impl Evaluatable<ProgData> for Com {
    fn eval(&self, vs: &mut VarStackImp) -> ProgData {
        match self {
            Com::CAss(x, a) => {
                let n = a.eval(vs);
                vs.assign_new(x.clone(), n);
                return ProgData::TUnit;
            },
            Com::CSeq(c1, c2) => {
                c1.eval(vs);
                return c2.eval(vs);
            },
            Com::CIf(b, c1, c2) => {
                if b.eval(vs) {
                    c1.eval(vs)
                } else {
                    c2.eval(vs)
                }
            },
            Com::CWhile(b, c) => {
                let mut res = ProgData::TUnit;
                if b.eval(vs) {
                    res = c.eval(vs);
                    let res1 = self.eval(vs);
                    match res1 {
                        ProgData::TUnit => (),
                        _ => res = res1
                    }
                }
                return res;
            },
            Com::CBlock(c) => {
                vs.grow();
                let res = c.eval(vs);
                vs.pop();
                return res;
            },
            Com::CUpd(x, a) => {
                let n = a.eval(vs);
                vs.set(x.clone(), n);
                return ProgData::TUnit;
            },
            Com::CRetVal(a) => {
                return a.eval(vs);
            },
        }
    }
}
