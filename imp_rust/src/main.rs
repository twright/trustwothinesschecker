use std::collections::HashMap;

mod parser;
mod ast;
use ast::*;
mod interpreter;
use interpreter::*;

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
    println!("1 + 2 * 3 = {}",
        plus_expr.eval(&mut VarStackImp::empty()));
    let mut stack2 = VarStackImp::from(vec![HashMap::from([("x".to_string(), 2)])]);
    println!("1 + x * 3 = {}",
        plus_expr2.eval(&mut stack2));

    let parsed_expr = parser::aexpr(&mut "1 + (2*3)".as_ref());
    match parsed_expr {
        Ok(ast) => {
            println!("Parsed ast");
            println!("Evaluated: {}", ast.eval(&mut VarStackImp::empty()));
        },
        Err(e) => {
            println!("Error parsing expression: {:?}", e);
        }
    }

    let parsed_expr2 = parser::aexpr(&mut "1 + (x*3)".as_ref());
    match parsed_expr2 {
        Ok(ast) => {
            println!("Parsed ast");
            println!("Evaluated: {}", ast.eval(&mut stack2));
        },
        Err(e) => {
            println!("Error parsing expression: {:?}", e);
        }
    }
}
