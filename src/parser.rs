use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MathOp {
    ADD,
    SUB,
    MUL,
    DIV,
    REM,
    POW,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MathToken {
    LeftParen,
    RightParen,
    Eq,
    Alpha(String),
    Num(String),
    Operator(MathOp),
    EOF,
}
fn check_for_multiplication(current: &u8, next: &u8) -> bool {
    (current.is_ascii_digit() && next.is_ascii_alphabetic())
        || (next.is_ascii_digit() && current.is_ascii_alphabetic())
}
pub fn tokenize(content: &str) -> Vec<MathToken> {
    let mut vec = Vec::with_capacity(content.len());
    let bytes = content.as_bytes();
    let mut i = 0;
    while let Some(byte) = bytes.get(i) {
        vec.push(match byte {
            b'(' => MathToken::LeftParen,
            b')' => MathToken::RightParen,
            b'=' => MathToken::Eq,
            b' ' => {
                i += 1;
                continue;
            }
            b'+' => MathToken::Operator(MathOp::ADD),
            b'-' => MathToken::Operator(MathOp::SUB),
            b'*' => MathToken::Operator(MathOp::MUL),
            b'/' => MathToken::Operator(MathOp::DIV),
            b'^' => MathToken::Operator(MathOp::POW),
            b'%' => MathToken::Operator(MathOp::REM),
            _ => {
                if byte.is_ascii_digit() {
                    let mut digit = String::with_capacity(12);
                    while let Some(byte) = bytes.get(i) {
                        let mut pc = 0;
                        if byte.is_ascii_digit() || (*byte == b'.' && pc == 0) {
                            pc += if *byte == b'.' { 1 } else { 0 };
                            digit.push(*byte as char);
                            i += 1;
                        } else {
                            break;
                        };
                    }
                    i -= 1;
                    MathToken::Num(digit)
                } else if byte.is_ascii_alphabetic() {
                    let mut str = String::with_capacity(12);
                    while let Some(byte) = bytes.get(i) {
                        if byte.is_ascii_alphabetic() {
                            str.push(*byte as char);
                            i += 1;
                        } else {
                            break;
                        };
                    }
                    i -= 1;
                    MathToken::Alpha(str)
                } else {
                    i += 1;
                    continue;
                }
            }
        });
        i += 1;
    }
    vec.push(MathToken::EOF);
    vec
}
pub struct Parser {
    tokens: Vec<MathToken>,
    idx: usize,
}

#[derive(Debug, Clone)]
pub enum Expr {
    BinExpr {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        operator: MathOp,
    },
    Func(String, String, Box<Expr>), //Func name, Func param, func expr,
    FunCall(String, Box<Expr>),      //Func name, parameter
    Def(String, Box<Expr>),
    Identifier(String),
    Numeric(f64),
}
impl Parser {
    pub fn new(tokens: Vec<MathToken>) -> Self {
        Self { tokens, idx: 0 }
    }
    fn peak(&self) -> Option<MathToken> {
        self.tokens.get(self.idx).cloned()
    }
    fn eat(&mut self) -> Option<MathToken> {
        let token = self.peak();
        self.idx += 1;
        token
    }

    fn eat_n(&mut self, n: usize) -> Option<MathToken> {
        self.idx += n;
        self.peak()
    }
    fn next(&self) -> Option<MathToken> {
        self.tokens.get(self.idx + 1).cloned()
    }
    pub fn expect(&mut self, tk: MathToken) -> Result<MathToken, String> {
        if let Some(token) = self.eat() {
            if token == tk {
                Ok(token)
            } else {
                Err(format!("Expected {tk:?} instead got {token:?}"))
            }
        } else {
            Err("Not able to eat".to_string())
        }
    }
    pub fn parse(&mut self) -> Result<Expr, String> {
        if let Some(MathToken::Eq) = self.next() {
            if let Some(MathToken::Alpha(varname)) = self.peak() {
                self.eat_n(2);
                Ok(Expr::Def(varname, Box::new(self.parse_expr())))
            } else {
                Err("Wtf, expected x = expr".to_string())
            }
        } else {
            self.parse_func()
        }
    }
    fn parse_func(&mut self) -> Result<Expr, String> {
        if let Some(token) = self.eat() {
            match token {
                MathToken::Alpha(fname) => {
                    self.eat(); //eat l paren
                    if let Some(tk) = self.eat() {
                        match tk {
                            MathToken::Alpha(pname) => {
                                self.eat();
                                if let Err(e) = self.expect(MathToken::Eq) {
                                    Err(e)
                                } else {
                                    Ok(Expr::Func(fname, pname, Box::new(self.parse_expr())))
                                }
                            }
                            _ => Err("Expected alphanumeric name for parameter".to_string()),
                        }
                    } else {
                        Err("not expected to end parsing".to_string())
                    }
                }
                _ => Err("Expected alphanumeric name for func name".to_string()),
            }
        } else {
            Err("No tokens to parse".to_string())
        }
    }
    fn parse_expr(&mut self) -> Expr {
        self.parse_additive()
    }
    fn parse_pow(&mut self) -> Expr {
        let mut left = self.parse_primary();
        while let Some(MathToken::Operator(MathOp::POW)) = self.peak() {
            self.eat();
            let right = self.parse_primary();
            left = Expr::BinExpr {
                lhs: Box::new(left),
                rhs: Box::new(right),
                operator: MathOp::POW,
            }
        }
        left
    }
    fn parse_multiplicative(&mut self) -> Expr {
        let mut left = self.parse_pow();
        while let Some(MathToken::Operator(t)) = self.peak() {
            let op = match t {
                MathOp::MUL | MathOp::DIV | MathOp::REM => t,
                _ => break,
            };
            self.eat();
            let right = self.parse_pow();
            left = Expr::BinExpr {
                lhs: Box::new(left),
                rhs: Box::new(right),
                operator: op,
            };
        }
        left
    }
    fn parse_additive(&mut self) -> Expr {
        let mut left = self.parse_multiplicative();
        while let Some(MathToken::Operator(t)) = self.peak() {
            let op = match t {
                MathOp::ADD | MathOp::SUB => t,
                _ => break,
            };
            self.eat();
            let right = self.parse_multiplicative();
            left = Expr::BinExpr {
                lhs: Box::new(left),
                rhs: Box::new(right),
                operator: op,
            };
        }
        left
    }
    fn parse_primary(&mut self) -> Expr {
        if let Some(token) = self.peak() {
            match token {
                MathToken::LeftParen => {
                    //left paren for expr
                    self.eat();
                    let expr = self.parse_additive();
                    self.expect(MathToken::RightParen).unwrap();
                    expr
                }
                MathToken::Alpha(name) => {
                    if let Some(tk) = self.next() {
                        match tk {
                            MathToken::Num(next) => {
                                self.eat_n(2);
                                Expr::BinExpr {
                                    lhs: Box::new(Expr::Identifier(name)),
                                    rhs: Box::new(Expr::Numeric(next.parse::<f64>().unwrap())),
                                    operator: MathOp::MUL,
                                }
                            }
                            MathToken::LeftParen => {
                                self.eat_n(2);
                                Expr::FunCall(name, Box::new(self.parse_expr()))
                            }
                            _ => {
                                self.eat();
                                Expr::Identifier(name)
                            }
                        }
                    } else {
                        self.eat();
                        Expr::Identifier(name)
                    }
                }
                MathToken::Num(num) => {
                    if let Some(MathToken::Alpha(next)) = self.next() {
                        self.eat_n(2);
                        Expr::BinExpr {
                            lhs: Box::new(Expr::Numeric(num.parse::<f64>().unwrap())),
                            rhs: Box::new(Expr::Identifier(next)),
                            operator: MathOp::MUL,
                        }
                    } else {
                        if let Ok(val) = num.parse::<f64>() {
                            self.eat();
                            Expr::Numeric(val)
                        } else {
                            panic!("Invalid number {num}");
                        }
                    }
                }
                _ => panic!("Unexpected token {:?}", token),
            }
        } else {
            panic!("Could not peak");
        }
    }
}

pub struct Evaluator {
    ast: Expr,
    funcs: HashMap<String, Box<Expr>>,
    vars: HashMap<String, Box<Expr>>,
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
    fn save_func(&mut self, fname: String, body: Box<Expr>) -> f64 {
        self.funcs.insert(fname, body);
        0.0
    }
    pub fn save_fn(&mut self, f: Expr) {
        match f {
            Expr::Func(fname, _, expr) => {
                self.save_func(fname, expr);
            }
            _ => panic!("err"),
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
    pub fn evaluate(&mut self, ast: Option<Expr>, identifier_num: f64) -> f64 {
        match ast.unwrap_or(self.ast.clone()) {
            Expr::Func(fname, _, body) => self.save_func(fname, body),
            Expr::Def(_varname, expr) => self.eval(*expr, identifier_num),
            a => self.eval(a, identifier_num),
        }
    }
    fn eval_func(&mut self, fname: String, param: Expr, identifier_num: f64) -> f64 {
        if let Some(fexpr) = &mut self.funcs.get(&fname) {
            let fexpr = *fexpr.clone();
            let param_value = self.eval(param, identifier_num);
            self.eval(fexpr, param_value)
        } else {
            Self::ERROR_NUM
        }
    }
    fn eval_bin(&mut self, ast: Expr, identifier_num: f64) -> f64 {
        if let Expr::BinExpr { lhs, rhs, operator } = ast {
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
        } else {
            panic!("Wtf it should wait for binary expression")
        }
    }
    fn eval(&mut self, ast: Expr, identifier_num: f64) -> f64 {
        match ast {
            Expr::Func(_, _, _) => panic!("Didnt expect to receive functions and var definitions"),
            Expr::FunCall(fname, pexpr) => self.eval_func(fname, *pexpr, identifier_num),
            Expr::Def(_, expr) => self.eval(*expr, identifier_num),
            Expr::Identifier(_) => identifier_num,
            Expr::Numeric(n) => n,
            Expr::BinExpr {
                lhs: _,
                rhs: _,
                operator: _,
            } => self.eval_bin(ast, identifier_num),
        }
    }
}

pub fn gen_ast(content: &str) -> Result<Expr, String> {
    Parser::new(tokenize(content)).parse()
}
