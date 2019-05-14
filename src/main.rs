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
    let tokens = tokens.unwrap();
    for i in tokens {
        println!("{:?}", i);
    }
    println!("Lexer finished without error");
}