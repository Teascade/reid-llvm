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

Currently missing relevant features (TODOs) are:
- Arrays
- Structs (and custom types as such)
- Extern functions
- Strings
- Loops

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

Currently when testing the compiler I run `./libtest.sh fibonacci` or
`./libtest.sh arithmetic`.

What `./libtest.sh $1` does, is it compiles and runs the rust example found with
name `$1`, which may exist in any of the two projects. The two mentioned above
are in `reid/examples`.

All examples currently end up producing a `hello.o` and `hello.asm` file to the
root directory, which is then linked with `ldd` to produce a `main`, which is
finally executed.

This is currently very work-in-progress and many things about this repository
change erratically.

## Various notes in order to get this working properly
This is what worked for me, might not (probably) work for you, depending on
various versions of various libraries.

### Compiling LLVM 16.0.0

#### Context
Context for my computer. I am on ArchLinux, and here are some libraries and
their current versions that I have installed as of compiling, I'm not sure what
of them are relevant, if any, but saving them here still feels like a good idea
for the future:
- `cmake 3.27.0-1`
- `lib32-llvm-libs 15.0.7-1`
- `llvm 15.0.7-3`
- `llvm-libs 15.0.7-3`
- `gcc 13.1.1-2`
- `gcc-libs 13.1.1-2`
- `lib32-gcc-libs 13.1.1-2`
- `lld 15.0.7-2`
- `lldb 15.0.7-3`
- `clang 15.0.7-9`
- `make 4.4.1-2`
- `automake 1.16.5-2`

#### Commands

```sh
wget https://github.com/llvm/llvm-project/releases/download/llvmorg-16.0.0/llvm-16.0.0.src.tar.xz
wget https://github.com/llvm/llvm-project/releases/download/llvmorg-16.0.0/cmake-16.0.0.src.tar.xz

tar xvf llvm-16.0.0.src.tar.xz
tar xvf cmake-16.0.0.src.tar.xz

mv cmake-16.0.0.src cmake

cd llvm-16.0.0.src

cmake -B build -DCMAKE_INSTALL_PREFIX=$HOME/llvm-16 -DCMAKE_BUILD_TYPE=MinSizeRel -DLLVM_ENABLE_ASSERTIONS=ON -DLLVM_INCLUDE_TESTS=OFF

make -j8
```

*Also Note:* Building LLVM with `Ninja` was not successful for me, but this
method was. Ninja may be successful with you, to try it, add `-G Ninja` to the
`cmake`-command, and instead of `make` run `ninja install`.

### Building this crate itself

Assuming `llvm-16.0.0.src` from the previous step was at
`/path/llvm-16.0.0.src`, building this crate can be done via the following command:

```sh
LLVM_SYS_160_PREFIX=/path/llvm-16.0.0.src/build cargo build
```

## In conclusion
Good luck! It took me a good 10 hours to figure this out for myself, I sure hope
these instructions help both myself and someone else in the future!