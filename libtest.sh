#!/bin/sh

# Compiles example libtest, which produces hello.o and hello.asm, which is then
# compiled with main.cpp and executed for final result
#
# Do note this file is extremely simply for my own personal convenience

export .env
cargo run --example libtest && \
clang++ main.cpp hello.o -o main && \
./main
