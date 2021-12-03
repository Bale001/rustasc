pub mod codegen;
pub mod scope;

pub use codegen::Script;

#[cfg(test)]
mod tests {

    #[test]
    fn gen_test() {
        use swf::*;
        let t = r#"
        function b(a) {
            a.c = 0;
        }

        "#;
        let mut parser = parser::Parser::new(t);
        let script = parser.parse_script().unwrap();
        let mut s = crate::codegen::Script::new();
        s.write_script(script).unwrap();
        let bytes = s.into_bytes();
        let header = Header {
            compression: Compression::Zlib,
            version: 6,
            stage_size: Rectangle {
                x_min: Twips::from_pixels(0.0),
                x_max: Twips::from_pixels(400.0),
                y_min: Twips::from_pixels(0.0),
                y_max: Twips::from_pixels(400.0),
            },
            frame_rate: Fixed8::from_f32(60.0),
            num_frames: 1,
        };
        let tags = [Tag::DoAction(&bytes)];
        let file = std::fs::File::create("maybe.swf").unwrap();
        let writer = std::io::BufWriter::new(file);
        swf::write_swf(&header, &tags, writer).unwrap();
    }
}
