pub mod cli;
pub mod syntax;
pub mod transpiler;

#[cfg(test)]
mod tests {
    use swc_common::SourceMap;
    use swc_ecma_codegen::{text_writer::JsWriter, Config, Emitter};

    use crate::{syntax, transpiler::to_js_program};

    #[test]
    fn transpile() {
        let result = syntax::parse(
            "
                let a load 'hello'
            ",
        )
        .expect("Expected parsable string");

        println!("AST: {:?}", result);

        let result = to_js_program(result);

        println!("Transpiled: {:?}", result);

        let config = Config::default();
        let source_map: std::rc::Rc<SourceMap> = Default::default();
        let comments = None; // Replace with your comments instance

        // Create a buffer to write JavaScript code
        let mut buf = Vec::<u8>::new();

        // Create a new JsWriter instance
        let mut binding = Vec::<(swc_common::BytePos, swc_common::LineCol)>::new();
        let writer = JsWriter::new(
            Default::default(),             // Lrc<SourceMap>
            "\n",                           // New line delimiter
            std::io::Cursor::new(&mut buf), // Provide your writer that implements the `Write` trait
            Some(&mut binding),             // Optional source map
        );

        let mut emitter = Emitter {
            cfg: config,
            cm: source_map,
            comments: comments,
            wr: writer,
        };

        emitter
            .emit_program(&result)
            .expect("Could not emit program");

        // Get the written JavaScript code from the buffer
        let js_code = String::from_utf8(buf).expect("Invalid UTF-8 in generated JavaScript");

        println!("Emitted: {js_code}")
    }
}
