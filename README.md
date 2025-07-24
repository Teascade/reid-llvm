# Reid-LLVM
Reid is a toy-language compiler I'm working on to learn LLVM (and compiler
development).

Reid only uses [llvm-sys](https://gitlab.com/taricorp/llvm-sys.rs), which
provide very minimal bindings from the LLVM C-API to Rust. `reid_llvm`-crate
contains the relevant abstraction to produce a more Rust'y API from that.

Much of the syntax in Reid is directly inspired by rust, but mostly it is driven
by simplicity.

Reid is currently able to (non-exhaustively):
- Do basic algebra (e.g. Add, Sub, Mult)
- Resolve complex one-liners correctly using PEDMAS (e.g. `5 + 2 * 5 - 5 *
  5` is calculated correctly)
- Declare and call functions with varying parameters and return types
- Perform type-checking and type-inference such that return-types and
  parameter types must always match.
- Do simple logic-operations (e.g. If/And/Or)

An example program of Reid, that calculates the 5th fibonacci number (and uses
Rust for highlighting) is:
```rust
fn main() -> u16 {
    return fibonacci(5);
}
fn fibonacci(n: u16) -> u16 {
    if n <= 2 {
        return 1;
    }
    return fibonacci(n-1) + fibonacci(n-2);
}
```

Currently missing big features (TODOs) are:
- ~~Arrays~~ (DONE)
- ~~Structs~~ (DONE)
- ~~Extern functions~~ (DONE)
- ~~Strings~~ (DONE)
- ~~Borrows~~ (DONE)
- ~~Pointers~~ (DONE)
- ~~Unary operators~~
- ~~Floats~~ (DONE)
- ~~Type casting~~ (DONE)
- ~~Built-in Int/Float division and modulo~~ (DONE)
- ~~Loops~~ (DONE)
- ~~Intrinsic functions~~ (DONE)
- ~~Ability to specify types in literals and variable definitions~~ (DONE)
- Debug Information (PARTIALLY DONE)
- Not-Unary
- Importing types from other modules
- Importable binops?

Big features that I want later but are not necessary:
- Associated functions
- ~~User-defined binary operations~~ (DONE)
- ~~Asymmetric binary operations (e.g. string + u32)~~ (DONE)
- Error handling
- Lexing & parsing of whitespace and comments as well
- LSP implementation

Smaller features:
- ~~Hex-numbers~~
- Bitwise operations
- ~~Easier way to initialize arrays with a single value~~
- ~~Void-returns (`return;` for void-returning functions)~~
- ~~Only include standard library at all if it is imported~~
- Lexical scopes for Debug Information

### Why "Reid"

[ᚱ is an Elder Futhark rune](https://en.wikipedia.org/wiki/Raido) which means
"ride" or **"journey"**. As this language is meant for me to primarily learn
language design and compiler development, it is more about the *journey* rather
than the destination. ᚱ is written as "Reið" in Icelandic, which is the
inspiration behind "Reid" here.

### Why "Reid-LLVM"?

Because I have another project also called Reid, which only compiles to a
Virtual Instruction Set Architecture (V-ISA) that is executed via a custom-made
Virtual Machine. It is still hosted
[here](https://git.teascade.net/teascade/reid), but do note that it is very old
and not as representative of my skills as a programmer today as this one.

## What is currently being tested?

Currently when testing the compiler I run `./libtest.sh examples/{file}.reid`,
where the `{file}` is one of the various examples I've written to help me test
features of the compiler.

What `./libtest.sh $1` does, is it compiles and runs the rust example found at
path `$1`. Some pre-existing examples can be found in [`examples`](./examples)

All examples currently end up producing a `hello.o` and `hello.asm` file to the
root directory, which is then linked with `ldd` to produce a `main`, which is
finally executed.

This is currently very work-in-progress and many things about this repository
change erratically.

## Various notes in order to get this working properly
This is what worked for me, might not (probably) work for you, depending on
various versions of various libraries.

### Compiling LLVM 20.1.8

#### Context
Context for my computer. I am on ArchLinux, and here are some libraries and
their current versions that I have installed as of compiling, I'm not sure what
of them are relevant, if any, but saving them here still feels like a good idea
for the future:
- `clang 19.1.7-2`
- `cmake 4.0.2-1`
- `extra-cmake-modules 6.14.0-1`
- `gcc 15.1.1+r7+gf36ec88aa85a-1`
- `gcc-libs 15.1.1+r7+gf36ec88aa85a-1`
- `lib32-gcc-libs 15.1.1+r7+gf36ec88aa85a-1`
- `lib32-llvm-libs 1:19.1.7-2`
- `libgccjit 15.1.1+r7+gf36ec88aa85a-1`
- `lld 19.1.7-1`
- `lldb 19.1.7-2`
- `llvm 19.1.7-2`
- `llvm-libs 19.1.7-2`
- `llvm14 14.0.6-5`
- `llvm14-libs 14.0.6-5`
- `llvm15 15.0.7-3`
- `llvm15-libs 15.0.7-3`
- `make 4.4.1-2`


#### Commands

```sh
git clone https://github.com/llvm/llvm-project.git --depth=1 --branch=llvmorg-20.1.8

cd llvm_project

cmake llvm -B build -DCMAKE_BUILD_TYPE=MinSizeRel -DLLVM_ENABLE_ASSERTIONS=ON -DLLVM_INCLUDE_TESTS=OFF -DLLVM_BUILD_BENCHMARKS=OFF -G Ninja -DLLVM_USE_LINKER="ld.lld" -DLLVM_PARALLEL_LINK_JOBS=8

ninja -j23
```

*Also Note:* Building LLVM with `Ninja` was not successful for me, but this
method was. Ninja may be successful with you, to try it, add `-G Ninja` to the
`cmake`-command, and instead of `make` run `ninja install`.

### Building this crate itself

Assuming `llvm-project` from the previous step was at
`/path/llvm-project`, building this crate can be done via the following command:

```sh
LLVM_SYS_201_PREFIX=/path/llvm-project/build cargo build
```

## In conclusion
Good luck! It took me a good 10 hours to figure this out for myself, I sure hope
these instructions help both myself and someone else in the future!