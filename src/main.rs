mod compiler;
mod interpreter;
mod virtual_machine;
mod webserver;

use std::iter::Peekable;
use std::slice::Iter;

fn main() {
    let tokens = compiler::lexer::tokenize_file("res/main.z".to_string());
    if let Err(v) = tokens {
        println!("Error during lexical analysis:");
        for e in v {
            println!("{}", e);
        }
        return;
    };
    println!("\nLexer finished without error");
    let tokens = tokens.unwrap();
    //println!("{:#?}", tokens);
    //let expr_tree = compiler::parser::parse_expression(tokens);
    //println!("{:#?}", expr_tree);
    let parser = compiler::parser::Parser::new(&tokens);
    //let program = parser.parse_program();
    let program = parser.parse_program();
    match program {
        Ok(_p) => {
            println!("No Erros in parsing stage");
        },
        Err(e) => {
            println!("{}", e);
            return;
        }
    }
    //parser.parse_expression();
}
