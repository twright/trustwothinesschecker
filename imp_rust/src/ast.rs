pub enum AExpr {
    ANum(i32),
    APlus(Box<AExpr>, Box<AExpr>),
    AMinus(Box<AExpr>, Box<AExpr>),
    AMult(Box<AExpr>, Box<AExpr>),
    AVar(String),
}

pub enum BExpr {
    BTrue,
    BFalse,
    BEq(Box<AExpr>, Box<AExpr>),
    BLe(Box<AExpr>, Box<AExpr>),
    BNot(Box<BExpr>),
    BAnd(Box<BExpr>, Box<BExpr>),
}

pub enum Com {
    CAss(String, Box<AExpr>),
    CSeq(Box<Com>, Box<Com>),
    CIf(Box<BExpr>, Box<Com>, Box<Com>),
    CWhile(Box<BExpr>, Box<Com>),
}