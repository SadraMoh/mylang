use std::fs;

use clap::Parser;
use mylang::cli::Args;

fn main() {
    let args = Args::parse();

    let contents = fs::read_to_string(args.file).expect("Unable to read file");

    println!("{}", contents);
}
