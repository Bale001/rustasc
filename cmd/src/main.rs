use codegen::Script;
use encoding_rs::UTF_8;
use parser::{parser::ParseError, Parser};
use std::fs;
use std::path::PathBuf;
use swf::*;

fn invalid_input(message: &str) -> ! {
    eprintln!("Invalid input: {}", message);
    std::process::exit(1);
}

fn compile_error(message: ParseError) -> ! {
    eprintln!("Error while parsing:\n{}", message);
    std::process::exit(1);
}

fn main() {
    match std::env::args().last().map(PathBuf::from) {
        Some(path) => {
            if !path.is_file() {
                invalid_input("File not found");
            }
            let file_name = PathBuf::from(path.file_name().unwrap());
            let mut dir = path.clone();
            dir.pop();
            let data = fs::read(path).unwrap();
            let (file_data, _, _) = UTF_8.decode(&data);
            let mut parser = Parser::new(&file_data);
            let script = parser
                .parse_script()
                .unwrap_or_else(|message| compile_error(message));
            let mut gen_script = Script::new();
            gen_script.write_script(script).unwrap();

            let bytes = gen_script.into_bytes();
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
            let file = std::fs::File::create(file_name.with_extension("swf")).unwrap();
            let writer = std::io::BufWriter::new(file);
            swf::write_swf(&header, &tags, writer).unwrap();
        }
        None => invalid_input("No input"),
    }
}
