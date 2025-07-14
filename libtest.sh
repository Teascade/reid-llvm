#!/bin/sh

# Compiles example libtest, which produces hello.o and hello.asm, which is then
# compiled with main.cpp and executed for final result
#
# Do note this file is extremely simply for my own personal convenience

export .env
cargo run --release --example cli $1 && \
# clang hello.o -o main && \
ld -dynamic-linker /lib64/ld-linux-x86-64.so.2 \
   -o main /usr/lib/crt1.o hello.o -lc  && \
./main ; echo "Return value: ""$?"


## Command from: clang -v hello.o -o test
## Original command:
# ld --hash-style=gnu \
#    --build-id \
#    --eh-frame-hdr \
#     -m elf_x86_64 \
#     -pie \
#     -dynamic-linker /lib64/ld-linux-x86-64.so.2 \
#     -o test \
#     /usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/15.1.1/../../../../lib64/Scrt1.o \
#     /usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/15.1.1/../../../../lib64/crti.o \
#     /usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/15.1.1/crtbeginS.o \
#     -L/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/15.1.1 \
#     -L/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/15.1.1/../../../../lib64 \
#     -L/lib/../lib64 \
#     -L/usr/lib/../lib64 \
#     -L/lib \
#     -L/usr/lib \
#     hello.o \
#     -lgcc \
#     --as-needed \
#     -lgcc_s \
#     --no-as-needed \
#     -lc \
#     -lgcc \
#     --as-needed \
#     -lgcc_s \
#     --no-as-needed \
#     /usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/15.1.1/crtendS.o \
#     /usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/15.1.1/../../../../lib64/crtn.o \
#     && \