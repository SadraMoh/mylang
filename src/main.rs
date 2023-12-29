use mylang::transpiler::transpile;
use std::{
    env, fs,
    io::{self, Write},
};

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let filename = args[1].clone();

    let content = fs::read_to_string(filename).expect("Unable to read file");

    let js = transpile(&content);

    io::stdout().write_all(js.as_bytes())?;

    Ok(())
}
