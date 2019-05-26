mod compiler;
mod interpreter;
mod virtual_machine;
mod webserver;

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
    let mut parser = compiler::parser::Parser::new(tokens);
    let program = parser.parse_program();
    match program {
        Ok(_p) => {
            println!("No Erros in parsing stage");
        }
        Err(e) => {
            println!("{}", e);
            return;
        }
    }
}
