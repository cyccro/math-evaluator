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
    SemiColon,
    VarDecl,
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
            b';' => MathToken::SemiColon,
            _ => {
                if byte.is_ascii_digit() {
                    let mut digit = String::with_capacity(12);
                    while let Some(byte) = bytes.get(i) {
                        let mut _has = false;
                        if byte.is_ascii_digit() || (*byte == b'.' && !_has) {
                            _has = *byte == b'.';
                            digit.push(*byte as char);
                            i += 1;
                        } else {
                            break;
                        }
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
                    if str == "declare" {
                        MathToken::VarDecl
                    } else {
                        MathToken::Alpha(str)
                    }
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
#[derive(Debug)]
pub struct Error(String);
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
    VarDeclaration(String, Box<Expr>),
    Identifier(String),
    UnaryNeg,
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
    pub fn expect(&mut self, tk: MathToken) -> Result<MathToken, Error> {
        if let Some(token) = self.eat() {
            if token == tk {
                return Ok(token);
            }
            Err(Error(format!("Expected {tk:?} instead got {token:?}")))
        } else {
            Err(Error("Token is None, expected find some".to_string()))
        }
    }
    pub fn parse(&mut self) -> Result<Expr, Error> {
        if let Some(MathToken::VarDecl) = self.peak() {
            self.eat();
            if let Some(MathToken::Alpha(tk)) = self.eat() {
                self.expect(MathToken::Eq)?;
                return Ok(Expr::VarDeclaration(tk, Box::new(self.parse_expr()?)));
            }
            return Err(Error("Expected a alphanumeric name".to_string()));
        }
        if let Some(MathToken::Eq) = self.next() {
            if let Some(MathToken::Alpha(varname)) = self.peak() {
                self.eat_n(2);
                return Ok(Expr::Def(varname, Box::new(self.parse_expr()?)));
            }
            return Err(Error("Expected identifier for var definition".to_string()));
        } else {
            self.parse_func()
        }
    }
    fn parse_func(&mut self) -> Result<Expr, Error> {
        if let Some(MathToken::Alpha(fname)) = self.eat() {
            self.eat(); //eat l paren
            if let Some(MathToken::Alpha(pname)) = self.eat() {
                self.eat();
                self.expect(MathToken::Eq)?; //eat r paren
                let expr = self.parse_expr()?;
                return Ok(Expr::Func(fname, pname, Box::new(expr)));
            }
            return Err(Error("Expected token to be alphanumeric".to_string()));
        }
        return Err(Error("No tokens to parse".to_string()));
    }
    fn parse_expr(&mut self) -> Result<Expr, Error> {
        self.parse_additive()
    }
    fn parse_pow(&mut self) -> Result<Expr, Error> {
        let mut left = match self.parse_primary() {
            Ok(expr) => expr,
            Err(e) => return Err(e),
        };
        while let Some(MathToken::Operator(MathOp::POW)) = self.peak() {
            self.eat();
            left = Expr::BinExpr {
                lhs: Box::new(left),
                rhs: Box::new(match self.parse_primary() {
                    Ok(expr) => expr,
                    Err(e) => return Err(e),
                }),
                operator: MathOp::POW,
            }
        }
        Ok(left)
    }
    fn parse_multiplicative(&mut self) -> Result<Expr, Error> {
        let mut left = match self.parse_pow() {
            Err(e) => return Err(e),
            Ok(expr) => expr,
        };
        while let Some(MathToken::Operator(op @ (MathOp::MUL | MathOp::DIV | MathOp::REM))) =
            self.peak()
        {
            self.eat();
            left = Expr::BinExpr {
                lhs: Box::new(left),
                rhs: Box::new(match self.parse_pow() {
                    Err(e) => return Err(e),
                    Ok(expr) => expr,
                }),
                operator: op,
            };
        }
        Ok(left)
    }
    fn parse_additive(&mut self) -> Result<Expr, Error> {
        let mut left = match self.parse_multiplicative() {
            Ok(expr) => expr,
            Err(e) => return Err(e),
        };
        while let Some(MathToken::Operator(op @ (MathOp::ADD | MathOp::SUB))) = self.peak() {
            self.eat();
            left = Expr::BinExpr {
                lhs: Box::new(left),
                rhs: Box::new(match self.parse_multiplicative() {
                    Ok(expr) => expr,
                    Err(e) => return Err(e),
                }),
                operator: op,
            };
        }
        Ok(left)
    }
    fn parse_primary(&mut self) -> Result<Expr, Error> {
        if let Some(token) = self.peak() {
            return match token {
                MathToken::LeftParen => {
                    //left paren for expr (5 * x - 4y)
                    self.eat();
                    let expr = self.parse_expr()?;
                    self.expect(MathToken::RightParen)?;
                    Ok(expr)
                }
                MathToken::Alpha(name) => {
                    let next = if let Some(next) = self.next() {
                        next
                    } else {
                        self.eat();
                        return Ok(Expr::Identifier(name));
                    };
                    let expr = match next {
                        MathToken::LeftParen => {
                            self.eat_n(2);
                            Expr::FunCall(name, Box::new(self.parse_expr()?))
                        }
                        MathToken::Num(next) => {
                            self.eat_n(2);
                            Expr::BinExpr {
                                lhs: Box::new(Expr::Identifier(name)),
                                rhs: Box::new(Expr::Numeric(match next.parse::<f64>() {
                                    Ok(num) => num,
                                    Err(e) => return Err(Error(format!("{e:?}"))),
                                })),
                                operator: MathOp::MUL,
                            }
                        }
                        _ => {
                            self.eat();
                            return Ok(Expr::Identifier(name));
                        }
                    };
                    Ok(expr)
                }
                MathToken::Num(num) => {
                    if let Some(MathToken::Alpha(next)) = self.next() {
                        self.eat_n(2);
                        Ok(Expr::BinExpr {
                            lhs: Box::new(Expr::Numeric(match next.parse::<f64>() {
                                Ok(num) => num,
                                Err(e) => return Err(Error(format!("{e:?}"))),
                            })),
                            rhs: Box::new(Expr::Identifier(next)),
                            operator: MathOp::MUL,
                        })
                    } else {
                        if let Ok(val) = num.parse::<f64>() {
                            self.eat();
                            return Ok(Expr::Numeric(val));
                        }
                        return Err(Error(format!("Invalid number {num}")));
                    }
                }
                MathToken::Operator(MathOp::SUB) => Ok(Expr::UnaryNeg),
                _ => Err(Error(format!("Unexpected token {token:?} during parse"))),
            };
        }
        return Err(Error("Not able to parse due to lack of tokens".to_string()));
    }
}

pub fn gen_ast(content: &str) -> Result<Expr, Error> {
    Parser::new(tokenize(content)).parse()
}
