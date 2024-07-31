use std::collections::HashMap;

use crate::{gen_ast, Error, Expr, MathOp};
pub struct Evaluator {
    ast: Expr,
    funcs: HashMap<String, Box<Expr>>,
    vars: HashMap<String, f64>,
}

impl Evaluator {
    const ERROR_NUM: f64 = -1.234567890;
    pub fn new(ast: Expr) -> Self {
        Self {
            ast,
            funcs: HashMap::new(),
            vars: HashMap::new(),
        }
    }
    pub fn eval_asts(asts: Vec<Expr>) -> f64 {
        let mut evaluator = Self::new(Expr::Numeric(0.0));
        let mut r = Self::ERROR_NUM;
        for ast in asts {
            r = evaluator.evaluate(Some(ast), 0.0);
        }
        r
    }
    pub fn eval_from_txt(content: &str) -> Result<f64, Error> {
        let mut vec = Vec::with_capacity(12);
        for c in content.split(';') {
            vec.push(gen_ast(c)?);
        }
        Ok(Self::eval_asts(vec))
    }
    fn save_func(&mut self, fname: String, body: Box<Expr>) -> f64 {
        self.funcs.insert(fname, body);
        0.0
    }
    pub fn save_fn(&mut self, f: Expr) {
        match f {
            Expr::Func(fname, _, expr) => {
                self.save_func(fname, expr);
            }
            _ => {}
        }
    }
    pub fn save_fns(&mut self, fns: Vec<Expr>) {
        for f in fns {
            match f {
                Expr::Func(fname, _, expr) => {
                    self.save_func(fname, expr);
                }
                _ => {}
            };
        }
    }
    pub fn save_var(&mut self, varname: String, expr: Expr) -> f64 {
        let evalue = self.eval(expr, 0.0);
        self.vars.insert(varname, evalue);
        evalue
    }
    pub fn evaluate(&mut self, ast: Option<Expr>, identifier_num: f64) -> f64 {
        match ast.unwrap_or(self.ast.clone()) {
            Expr::Func(fname, _, body) => self.save_func(fname, body),
            Expr::Def(_, expr) => self.eval(*expr, identifier_num),
            Expr::VarDeclaration(varname, expr) => self.save_var(varname, *expr),
            a => self.eval(a, identifier_num),
        }
    }
    fn eval_func(&mut self, fname: String, param: Expr, identifier_num: f64) -> f64 {
        let param_value = self.eval(param, identifier_num);
        match &*fname {
            "sin" => param_value.sin(),
            "sinh" => param_value.sinh(),
            "asin" => param_value.asin(),
            "asinh" => param_value.asinh(),

            "cos" => param_value.cos(),
            "cosh" => param_value.cosh(),
            "acos" => param_value.acos(),
            "acosh" => param_value.acosh(),

            "tan" => param_value.tan(),
            "tanh" => param_value.tanh(),
            "atan" => param_value.atan(),
            "atanh" => param_value.atanh(),

            "log10" => param_value.log10(),
            "log2" => param_value.log2(),
            "ln" => param_value.ln(),

            "floor" => param_value.floor(),
            "ceil" => param_value.ceil(),

            "sqrt" => param_value.sqrt(),
            "cbrt" => param_value.cbrt(),

            "2pow" => param_value.exp2(),
            "exp" => param_value.exp(),

            "abs" => param_value.abs(),
            "sign" => param_value.signum(),

            //"gamma" => param_value.gamma(),
            //"fac" => (param_value + 1.0).gamma(),
            "inverse" => param_value.recip(),
            "trunc" => param_value.trunc(),
            "fract" => param_value.fract(),

            "radians" => param_value.to_radians(),
            "degrees" => param_value.to_degrees(),
            _ => {
                if let Some(fexpr) = &mut self.funcs.get(&fname) {
                    self.eval(*fexpr.clone(), param_value)
                } else {
                    Self::ERROR_NUM
                }
            }
        }
    }
    fn eval(&mut self, ast: Expr, identifier_num: f64) -> f64 {
        match ast {
            Expr::Func(_, _, _) | Expr::VarDeclaration(_, _) => {
                panic!("Didnt expect to receive functions and var definitions")
            }
            Expr::FunCall(fname, pexpr) => self.eval_func(fname, *pexpr, identifier_num),
            Expr::Def(_, expr) => self.eval(*expr, identifier_num),
            Expr::Identifier(name) => *self.vars.get(&name).unwrap_or(&identifier_num),
            Expr::Numeric(n) => n,
            Expr::UnaryNeg => -self.evaluate(None, identifier_num),
            Expr::BinExpr { lhs, rhs, operator } => {
                let lhs_val = self.evaluate(Some(*lhs), identifier_num);
                let rhs_val = self.evaluate(Some(*rhs), identifier_num);
                match operator {
                    MathOp::ADD => lhs_val + rhs_val,
                    MathOp::SUB => lhs_val - rhs_val,
                    MathOp::MUL => lhs_val * rhs_val,
                    MathOp::DIV => lhs_val / rhs_val,
                    MathOp::POW => lhs_val.powf(rhs_val),
                    MathOp::REM => lhs_val % rhs_val,
                }
            }
        }
    }
}
