mod parser;
use parser::*;
fn main() {
    let sin = "sin(x) = x ^ (1/2)";
    let f = "faq(x) = x * x * sin(x*3)"; //function
    let y = "y = faq(u)"; //var
    let faq = gen_ast(f).unwrap();
    let sin = gen_ast(sin).unwrap();
    let y = gen_ast(y).unwrap();
    let mut eval = Evaluator::new(y);
    eval.save_fn(faq);
    eval.save_fn(sin);
    println!("{:#?}", eval.evaluate(None, 12.0));
}
