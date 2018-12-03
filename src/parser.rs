use std::collections::HashMap;
use lexer;

#[derive(Debug)]
pub enum Type {
    i32
}

impl Type {
    fn force_from_string(s:&str) -> Type {
        match s {
            "i32" => Type::i32,
             _ => panic!("Unknown type {:?}", s)
        }
    }

}


#[derive(Debug)]
pub enum Expr<'a> {
    Funcall(&'a str, Vec<Expr<'a>>),
    VarLookup(&'a str),
    I32_lit(i32)
}



#[derive(Debug)]
pub struct DeclData<'a> {
    pub name: &'a str,
    pub v_type: Type,
    pub value: Expr<'a>
}

#[derive(Debug)]
pub struct LoopData<'a> {
    pub var_name: &'a str,
    pub start_expr: Expr<'a>,
    pub end_expr: Expr<'a>,
    pub block: Block<'a>
}


#[derive(Debug)]

pub enum Condition<'a> {
    Equals(Expr<'a>, Expr<'a>)
}

#[derive(Debug)]
pub struct IfData<'a> {
    pub condition: Condition<'a>,
    pub block: Block<'a>,
}

#[derive(Debug)]
pub enum Statement<'a> {
    Declaration(DeclData<'a>),
    Assignment(&'a str, Expr<'a>),
    ForLoop(LoopData<'a>),
    If(IfData<'a>),
    Evaluation(Expr<'a>)
}

#[derive(Debug)]
pub struct Block<'a> {
    pub stmts: Vec<Statement<'a>>
}


#[derive(Debug)]
pub struct Function<'a> {
    pub name: &'a str,
    pub block: Block<'a>
}

#[derive(Debug)]
pub struct Program<'a> {
    pub functions: HashMap<&'a str, Function<'a>>
}

impl<'a> Program<'a> {
    fn new() -> Program<'a> {
        Program { functions: HashMap::new() }
    }

    fn add_function(&mut self, func: Function<'a>) {
        self.functions.insert(func.name, func); // Todo this should have error handle
    }
}


struct Parser<'b, 'a : 'b> {
    toks: &'b [lexer::Token<'a>],
    tok_i: usize
}

impl<'b, 'a> Parser<'b, 'a> {

    fn new(toks: &'b [lexer::Token<'a>]) -> Parser<'b, 'a> {
        Parser {toks, tok_i: 0_usize}
    }
    fn is_done(&self) -> bool {
        self.tok_i >= self.toks.len()
    }

    fn current_tok(&self) -> &'b lexer::Token<'a> {
        &self.toks[self.tok_i]
    }


    fn bump(&mut self) {
        self.tok_i += 1;

    }




    fn try_eat(&mut self, t: &lexer::Token) -> bool {
        if self.toks[self.tok_i] == *t {
            self.bump();
            return true;
        }
        else {
            return false;
        }
    }


    fn expect(&self, t: &lexer::Token)  {
        let current = self.current_tok();

        if current != t {
            panic!("Expected a {:?} but found a {:?} at i={}" , t, current, self.tok_i);
        }

    }


    fn force_eat(&mut self, t: &lexer::Token) {
        let did_eat = self.try_eat(t);
        if !did_eat {
            panic!("Expected to eat a {:?} but found a {:?} at {:?}", t, self.toks[self.tok_i], &self.toks[self.tok_i..]);
        }
    }

    fn force_eat_return_ident(&mut self) -> &'a str {
        let t = self.current_tok();
        match t {
            &lexer::Token::Ident(s) => {self.bump(); s},
            a@ _ => panic!("Expected an ident but found a {:?}", a),
        }
    }

    fn force_eat_return_i32_lit(&mut self) -> i32 {
        let t= self.current_tok();

        match t {
            &lexer::Token::I32Literal(num) => {self.bump(); num},
            a@ _ => panic!("Expected an i32lit but found a {:?}", a)
        }
    }




    fn peek(&self) -> Option<&'b lexer::Token<'a>> {
        self.toks.get(self.tok_i + 1)
    }



    fn parse_expr(&mut self) -> Expr<'a> {
        let next = self.peek();
        match self.current_tok() {
            &lexer::Token::I32Literal(i) => { self.bump(); Expr::I32_lit(i)}
            &lexer::Token::Ident(s) if next == Some(&lexer::Token::OpenParen) => { self.parse_fn_call() }
            &lexer::Token::Ident(s) => {
                return Expr::VarLookup(self.force_eat_return_ident())
            },
            _ => panic!("Don't know how to parse this expression {:?}", &self.toks[self.tok_i..])

        }


    }

    fn parse_let(&mut self) -> Statement<'a> {
        self.force_eat(&lexer::Token::Let);
        let var_name = self.force_eat_return_ident();

        self.force_eat(&lexer::Token::Colon);
        let var_type_as_str = self.force_eat_return_ident();


        let var_type = Type::force_from_string(var_type_as_str);



        self.force_eat(&lexer::Token::Equal);
        let exp = self.parse_expr();
        self.force_eat(&lexer::Token::SemiColon);

        Statement::Declaration(DeclData {name: var_name, v_type: var_type, value: exp})
    }


    fn parse_fn_call(&mut self) -> Expr<'a> {
        let fn_name = self.force_eat_return_ident();
        let mut args = Vec::new();


        self.force_eat(&lexer::Token::OpenParen);
        while self.current_tok() != &lexer::Token::CloseParen {
            let expr = self.parse_expr();
            args.push(expr);
            self.try_eat(&lexer::Token::Comma);
        }

        self.force_eat(&lexer::Token::CloseParen);


        Expr::Funcall(fn_name, args)
    }


    fn parse_for(&mut self) -> Statement<'a> {
        self.force_eat(&lexer::Token::For);
        let ident = self.force_eat_return_ident();
        self.force_eat(&lexer::Token::In);
        let start = self.parse_expr();
        self.force_eat(&lexer::Token::DotDot);
        let end = self.parse_expr();

        let block = self.parse_block();


        Statement::ForLoop(LoopData{var_name: ident, start_expr: start, end_expr: end, block})

    }



    fn parse_condition(&mut self) -> Condition<'a> {
        let lhs = self.parse_expr();
        let op = self.current_tok();
        self.bump();

        let rhs = self.parse_expr();


        match op {
            &lexer::Token::EqualEqual => Condition::Equals(lhs, rhs),
            _ => unimplemented!("Condition's other than == are not implemented")

        }

    }

    fn parse_if(&mut self) -> Statement<'a> {
        self.force_eat(&lexer::Token::If);

        let inner_condition = self.parse_condition();

        let block = self.parse_block();


        return Statement::If(IfData{condition: inner_condition, block})

    }


    fn parse_assignment(&mut self) -> Statement<'a> {
        let idet = self.force_eat_return_ident();
        self.force_eat(&lexer::Token::Equal);
        let expr = self.parse_expr();
        self.force_eat(&lexer::Token::SemiColon);

        Statement::Assignment(idet, expr)
    }


    fn parse_stmt(&mut self) -> Statement<'a> {
        if self.current_tok() == &lexer::Token::Let {
            return self.parse_let();
        }

        if let (&lexer::Token::Ident(s), Some(&lexer::Token::Equal)) = (self.current_tok(), self.peek()) {
            return self.parse_assignment();
        }

        if self.current_tok() == &lexer::Token::For {
            return self.parse_for();
        }

        if self.current_tok() == &lexer::Token::If {
            return self.parse_if();
        }


        let stmt = Statement::Evaluation(self.parse_expr());
        self.force_eat(&lexer::Token::SemiColon);

        return stmt;

    }

    fn parse_block(&mut self) -> Block<'a> {
        self.force_eat(&lexer::Token::OpenBracket);

        let mut statements = Vec::new();

        while self.current_tok() != &lexer::Token::CloseBracket {
            statements.push(self.parse_stmt());
        }

        self.force_eat(&lexer::Token::CloseBracket);

        Block {
            stmts: statements
        }
    }

    fn parse_fn(&mut self) -> Function<'a> {
        self.force_eat(&lexer::Token::Fn);
        let name = self.force_eat_return_ident();
        self.force_eat(&lexer::Token::OpenParen);

        // Todo parse args
        self.force_eat(&lexer::Token::CloseParen);
        let block = self.parse_block();


        Function {
            name,
            block
        }





    }
}

pub fn parse<'b, 'a : 'b>(toks:&'b[lexer::Token<'a>]) -> Program<'a> {
    let mut program = Program::new();
    let mut parser = Parser::new(toks);


    while !parser.is_done() {
        let fun = parser.parse_fn();
        program.add_function(fun);
    }



    program
}