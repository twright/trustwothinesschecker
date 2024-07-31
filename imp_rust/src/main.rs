use std::collections::HashMap;
use std::collections::VecDeque;

enum AExpr {
    ANum(i32),
    APlus(Box<AExpr>, Box<AExpr>),
    AMinus(Box<AExpr>, Box<AExpr>),
    AMult(Box<AExpr>, Box<AExpr>),
    AVar(String),
}

enum BExpr {
    BTrue,
    BFalse,
    BEq(Box<AExpr>, Box<AExpr>),
    BLe(Box<AExpr>, Box<AExpr>),
    BNot(Box<BExpr>),
    BAnd(Box<BExpr>, Box<BExpr>),
}

enum Com {
    CAss(String, Box<AExpr>),
    CSeq(Box<Com>, Box<Com>),
    CIf(Box<BExpr>, Box<Com>, Box<Com>),
    CWhile(Box<BExpr>, Box<Com>),
}

type VarStackImp = VecDeque<HashMap<String, i32>>;

trait VarStack {
    fn empty() -> Self;
    fn lookup(&self, x: &String) -> i32;
    fn set(&mut self, x: String, n: i32);
    fn tail(&self) -> Self;
    fn pop(&mut self);
}

impl VarStack for VarStackImp {
    fn empty() -> Self {
        return VecDeque::from(vec![HashMap::new()]);
    }

    fn lookup(&self, x: &String) -> i32 {
        for map in self.iter().rev() {
            if map.contains_key(x) {
                return map[x];
            }
        }
        panic!("Variable not found");
    }

    fn set(&mut self, x: String, n: i32) {
        for map in self.iter_mut() {
            if map.contains_key(&x) {
                map.entry(x).and_modify(|e| *e = n);
                return;
            }
        }

        self[0].entry(x).or_insert(n);
        return;
    }

    fn tail(&self) -> Self {
        let mut vs = self.clone();
        vs.pop_front();
        return vs;
    }

    fn pop(&mut self) {
        self.pop_front();
    }
}

trait Evaluatable<T> {
    fn eval(&self, vs: &mut VarStackImp) ->
        T;
}

impl Evaluatable<i32> for AExpr {
    fn eval(&self, vs: &mut VarStackImp) ->
        i32 {
        match self {
            AExpr::ANum(n) => *n,
            AExpr::APlus(a1, a2) => {
                a1.eval(vs) + a2.eval(vs)
            },
            AExpr::AMinus(a1, a2) => {
                a1.eval(vs) - a2.eval(vs)
            },
            AExpr::AMult(a1, a2) => {
                a1.eval(vs) * a2.eval(vs)
            },
            AExpr::AVar(x) => {
                vs.lookup(x)
            },
        }
    }
}

impl Evaluatable<bool> for BExpr {
    fn eval(&self, vs: &mut VarStackImp) ->
        bool {
        match self {
            BExpr::BTrue => true,
            BExpr::BFalse => false,
            BExpr::BEq(a1, a2) => {
                a1.eval(vs) == a2.eval(vs)
            },
            BExpr::BLe(a1, a2) => {
                a1.eval(vs) <= a2.eval(vs)
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

impl Evaluatable<()> for Com {
    fn eval(&self, vs: &mut VarStackImp) ->
        () {
        match self {
            Com::CAss(x, a) => {
                let n = a.eval(vs);
                vs.set(x.clone(), n);
            },
            Com::CSeq(c1, c2) => {
                c1.eval(vs);
                c2.eval(vs);
            },
            Com::CIf(b, c1, c2) => {
                if b.eval(vs) {
                    c1.eval(vs)
                } else {
                    c2.eval(vs)
                }
            },
            Com::CWhile(b, c) => {
                if b.eval(vs) {
                    c.eval(vs);
                    self.eval(vs);
                }
            },
        }
    }
}

fn main() {
    let plus_expr = AExpr::APlus(
        Box::new(AExpr::ANum(1)),
        Box::new(AExpr::AMult(
            Box::new(AExpr::ANum(2)),
            Box::new(AExpr::ANum(3)),
        )),
    );
    let plus_expr2 = AExpr::APlus(
        Box::new(AExpr::ANum(1)),
        Box::new(AExpr::AMult(
            Box::new(AExpr::AVar("x".to_string())),
            Box::new(AExpr::ANum(3)),
        )),
    );
    let mut stack2 = VarStackImp::from(vec![HashMap::from([("x".to_string(), 2)])]);
    println!("1 + 2 * 3 = {}",
        plus_expr.eval(&mut VarStackImp::empty()));
    println!("1 + x * 3 = {}",
        plus_expr2.eval(&mut stack2));
}
