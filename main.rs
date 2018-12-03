use std::fs::File;
use std::io::Write;

mod lexer;
mod parser;
mod mips;




fn main() {


    let txt = include_str!("../sample.txt");
    let lexed:Vec<lexer::Token> = lexer::lex(txt);
    let prog:parser::Program = parser::parse(&lexed);


    println!("AST================");

    println!("{:#?}", prog);
    let mips_str = mips::to_mips(&prog);


    println!("{}", mips_str);


    let mut output = File::create("mips_compiled_out.s").unwrap();
    output.write_all(mips_str.as_bytes());



    println!("Hello, world!");
}
