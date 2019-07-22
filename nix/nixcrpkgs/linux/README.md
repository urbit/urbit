## Linux GCC toolchain

The files in this directory define how we build our GCC cross-compiler that
targets Linux, using the musl libc.

### A note about `-rdynamic`

Do not pass `-rdynamic` to GCC when building an executable; it will cause the compiled executable to depend on a musl libc dynamic loader in `/lib` that probably doesn't exist, and defeats the point of static linking.  The `-static` option overrides `-rdynamic`, so adding`-static` to the linker flags of a project using `-rdynamic` is one way to fix the issue.

CMake will pass `-rdynamic` unless you set [CMP0065](https://cmake.org/cmake/help/v3.8/policy/CMP0065.html) to new as shown below, or set your [CMake policy version](https://cmake.org/cmake/help/v3.8/command/cmake_policy.html) to 3.4 or later.

    # Don't use -rdynamic since it causes Musl static linking to not work.
    cmake_policy(SET CMP0065 NEW)

