# About
RustASC is a simple AS2 compiler. It currently does not support type checking. 

This can be used either using the command line utility, or by using it as a crate in another rust application (see [here](https://github.com/EmperorBale/rustasc/tree/master/codegen) or [here](https://github.com/EmperorBale/rustasc/tree/master/codegen)). RustASC internally uses the the [swf](https://docs.rs/swf/latest/swf/) crate for writing the SWF.

This compiler is **experimental**, and may have bugs. 

# Usage

Make sure you have [Rust](https://www.rust-lang.org/tools/install) installed. Use the following command to compile RustASC:
```
cargo build --package=cmd --release
```
The RustASC executable should appear in `target/release`.

Simply input the file you want to compile. This will compile `example.as`, and create `example.swf`.
```
./rustasc.exe example.as
```
NOTE: The linker is currently experimental, and is not available via the command line yet.
