#!/usr/bin/env sh
cc=$(which clang-3.6||which gcc-4.9||which clang||which gcc)
case $(uname) in
Linux)
    so=$(printf so) 
    ARFLAGS='rvs -o'
    ;;
Darwin)
    so=$(printf dylib) 
    ARFLAGS=rcs
    ;;
MINGW*)
    so=$(printf lib) 
    ARFLAGS='rcs -o'
    ;;
*)
    so=$(printf so) 
    ARFLAGS='rvs -o'
    ;;
esac
# TODO: Fix the compiler to work with any of the above compilers $cc
gcc -c keccak-tiny.c
ar ${ARFLAGS} libkeccak-tiny.$so keccak-tiny.o
