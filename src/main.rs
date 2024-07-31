mod evaluator;
mod parser;
use std::io::{read_to_string, stdin};

use evaluator::*;
use parser::*;
fn main() {
    let mut eval = Evaluator::new(Expr::Numeric(0.0));
    let mut str = String::new();
    loop {
        stdin().read_line(&mut str).unwrap();
        let ast = gen_ast(&*str);
        match ast {
            Ok(e) => println!("{:?}", eval.evaluate(Some(e), 0.0)),
            Err(e) => println!("{e:#?}"),
        }
        str.clear();
    }
}
