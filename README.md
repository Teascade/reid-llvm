# Reid-LLVM
Attempt at re-creating Reid, this time using LLVM.

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