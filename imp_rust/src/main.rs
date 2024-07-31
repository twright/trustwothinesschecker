use std::collections::HashMap;
use std::collections::VecDeque;

enum AExpr {
    ANum(i32),
    APlus(Box<AExpr>, Box<AExpr>),
    AMinus(Box<AExpr>, Box<AExpr>),
    AMult(Box<AExpr>, Box<AExpr>),
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
    fn get(&self, x: &String) -> i32;
    fn set(&mut self, x: String, n: i32);
    fn tail(&self) -> Self;
    fn pop(&mut self);
}

impl VarStack for VarStackImp {
    fn empty() -> Self {
        return VecDeque::from(vec![HashMap::new()]);
    }

    fn get(&self, x: &String) -> i32 {
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
        (T, VarStackImp);
}

impl Evaluatable<i32> for AExpr {
    fn eval(&self, vs: &mut VarStackImp) ->
        (i32, VarStackImp) {
        match self {
            AExpr::ANum(n) => (*n, vs.clone()),
            AExpr::APlus(a1, a2) => {
                let (n1, mut vs1) = a1.eval(vs);
                let (n2, vs2) = a2.eval(&mut vs1);
                (n1 + n2, vs2)
            },
            AExpr::AMinus(a1, a2) => {
                let (n1, mut vs1) = a1.eval(vs);
                let (n2, vs2) = a2.eval(&mut vs1);
                (n1 - n2, vs2)
            },
            AExpr::AMult(a1, a2) => {
                let (n1, mut vs1) = a1.eval(vs);
                let (n2, vs2) = a2.eval(&mut vs1);
                (n1 * n2, vs2)
            },
        }
    }
}

impl Evaluatable<bool> for BExpr {
    fn eval(&self, vs: &mut VarStackImp) ->
        (bool, VarStackImp) {
        match self {
            BExpr::BTrue => (true, vs.clone()),
            BExpr::BFalse => (false, vs.clone()),
            BExpr::BEq(a1, a2) => {
                let (n1, mut vs1) = a1.eval(vs);
                let (n2, vs2) = a2.eval(&mut vs1);
                (n1 == n2, vs2)
            },
            BExpr::BLe(a1, a2) => {
                let (n1, mut vs1) = a1.eval(vs);
                let (n2, vs2) = a2.eval(&mut vs1);
                (n1 <= n2, vs2)
            },
            BExpr::BNot(b) => {
                let (b1, vs1) = b.eval(vs);
                (!b1, vs1)
            },
            BExpr::BAnd(b1, b2) => {
                let (b1, mut vs1) = b1.eval(vs);
                let (b2, vs2) = b2.eval(&mut vs1);
                (b1 && b2, vs2)
            },
        }
    }
    
}

impl Evaluatable<()> for Com {
    fn eval(&self, vs: &mut VarStackImp) ->
        ((), VarStackImp) {
        match self {
            Com::CAss(x, a) => {
                let (n, mut vs1) = a.eval(vs);
                vs1.set(x.clone(), n);
                ((), vs1)
            },
            Com::CSeq(c1, c2) => {
                let (_, mut vs1) = c1.eval(vs);
                c2.eval(&mut vs1)
            },
            Com::CIf(b, c1, c2) => {
                let (b1, mut vs1) = b.eval(vs);
                if b1 {
                    c1.eval(&mut vs1)
                } else {
                    c2.eval(&mut vs1)
                }
            },
            Com::CWhile(b, c) => {
                let (b1, mut vs1) = b.eval(vs);
                if b1 {
                    let (_, mut vs2) = c.eval(&mut vs1);
                    c.eval(&mut vs2)
                } else {
                    ((), vs1)
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
    println!("1 + 2 * 3 = {}",
        plus_expr.eval(&mut VarStackImp::empty()).0);
}
