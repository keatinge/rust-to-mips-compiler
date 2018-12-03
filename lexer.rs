
#[derive(Debug, PartialEq, Eq)]
pub enum Token<'a> {
    // Language keywords
    Fn,
    Let,
    For,
    In,
    If,


    // Syntax
    Ident(&'a str),
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    SemiColon,
    Comma,
    Colon,
    DotDot,

    // Operatrors
    Equal,
    EqualEqual,

    // Literals
    I32Literal(i32),

}


struct Lexer<'a> {
    src: &'a str,
    chars: Vec<char>,
    index: usize
}


impl<'a> Lexer<'a> {
    fn is_finished(&self) -> bool {
        return self.index >= self.chars.len()
    }


    fn is_break_char(c:char) -> bool {
        ['(', ')', ';', ' ', '}', '{', ':', '\n', '\r', ','].contains(&c)
    }

    fn is_tokenless_break_char(c:char) -> bool {
        [' ', '\n', '\r'].contains(&c)
    }

    fn at_break(&self) -> bool {
        if Self::is_break_char(self.chars[self.index]) {
            return true;
        }

        if self.current_char() == '.' && self.peek() == Some(&'.'){
            return true;
        }

        return false;
    }

    fn at_tokenless_break_char(&self) -> bool {
        Self::is_tokenless_break_char(self.chars[self.index])
    }


    fn eat_return_break_token(&mut self) -> Token<'a> {
        let current_char = self.current_char();

        let tok = match current_char {
            '(' => Token::OpenParen,
            ')' => Token::CloseParen,
            ';' => Token::SemiColon,
            '{' => Token::OpenBracket,
            '}' => Token::CloseBracket,
            ',' => Token::Comma,
            ':' => Token::Colon,
            '.' if self.peek() == Some(&'.') => {self.bump(); Token::DotDot},
            ' ' | '\n' => panic!("There is no token for whitespace!!!"),
            a@ _=> panic!("Unrecognized token {:?}", a)
        };

        self.bump();
        return tok




    }


    fn current_char(&self) -> char {
        self.chars[self.index]
    }

    fn peek(&self) -> Option<&char> {
        (self.chars.get(self.index+1))
    }

    fn bump(&mut self) {
        self.index += 1;
    }

    fn bump_ws(&mut self) {
        while !self.is_finished() && Self::is_whitespace(self.chars[self.index])   {
            self.index += 1;
        }
    }



    fn is_whitespace(c:char) -> bool {
        [' ', '\n', '\r'].contains(&c)
    }
    fn parse_one(&mut self) -> Token<'a> {

        let mut component = Vec::new();
        if self.at_break() {
            if self.at_tokenless_break_char() {
                self.bump();
            }

            let tok = self.eat_return_break_token();

            return tok;
        }


        let start_i = self.index;

        while !self.at_break() {
            component.push(self.current_char());
            self.bump();
        }

        let as_str = component.iter().collect::<String>();

        let maybe_num = as_str.parse::<i32>();
        if maybe_num.is_ok() {
            return Token::I32Literal(maybe_num.unwrap());
        }

        return match as_str.as_ref() {
                "fn" => Token::Fn,
                "let" => Token::Let,
                "for" => Token::For,
                "in" => Token::In,
                "if" => Token::If,
                "=" => Token::Equal,
                "==" => Token::EqualEqual,
                "*" => unimplemented!(),
                "+" => unimplemented!(),
                _=> Token::Ident(&self.src[start_i..self.index])
        };

    }

}

pub fn lex<'a>(src:&'a str) -> Vec<Token<'a>> {
    let mut lexer = Lexer {src, index: 0, chars: src.chars().collect::<Vec<_>>()};
    let mut tokens = Vec::<Token<'a>>::new();

    lexer.bump_ws();

    while !lexer.is_finished() {
        tokens.push(lexer.parse_one());
        lexer.bump_ws();
    }

    println!("{:#?}", tokens);
    tokens
}


pub fn is_identifier(t: &Token) -> bool {
    match t {
        &Token::Ident(_) => true,
        _ => false
    }
}
