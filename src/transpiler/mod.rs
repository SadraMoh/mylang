mod emitter;
mod translator;

pub use emitter::emit;
pub use translator::{to_js_bin_op, to_js_expr, to_js_program, to_js_stmt};

pub fn transpile(src: &str) -> String {
    let parsed = crate::syntax::parse(&src).unwrap();

    let translated = to_js_program(parsed);

    emit(&translated)
}

#[cfg(test)]
mod tests {
    #[test]
    fn transpiles() {
        let result = super::transpile(
            "
            fun addition x y do
                ret x add y
            end

            5 pipe addition 10 it

            let result load 1
                pipe addition it 5
                pipe addition it 10
        ",
        );

        println!("Emitted: {result}")
    }
}
