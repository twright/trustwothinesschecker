use std::fmt::Display;

#[derive(Clone)]
pub enum ProgData {
    TInt(i32),
    TFun(Vec<Box<str>>, Box<Com>),
    TUnit,
}

impl Display for ProgData {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ProgData::TInt(n) => write!(f, "{}", n),
            ProgData::TFun(_, _) => write!(f, "function"),
            ProgData::TUnit => write!(f, "unit"),
        }
    }
}

#[derive(Clone)]
pub enum AExpr {
    ANum(i32),
    AFun(Vec<Box<str>>, Box<Com>),
    APlus(Box<AExpr>, Box<AExpr>),
    AMinus(Box<AExpr>, Box<AExpr>),
    AMult(Box<AExpr>, Box<AExpr>),
    AVar(String),
    ACall(String, Vec<AExpr>),
}

#[derive(Clone)]
pub enum BExpr {
    BTrue,
    BFalse,
    BEq(Box<AExpr>, Box<AExpr>),
    BLe(Box<AExpr>, Box<AExpr>),
    BNot(Box<BExpr>),
    BAnd(Box<BExpr>, Box<BExpr>),
}

#[derive(Clone)]
pub enum Com {
    CAss(String, Box<AExpr>),
    CUpd(String, Box<AExpr>),
    CBlock(Box<Com>),
    CSeq(Box<Com>, Box<Com>),
    CIf(Box<BExpr>, Box<Com>, Box<Com>),
    CWhile(Box<BExpr>, Box<Com>),
    CRetVal(Box<AExpr>),
}