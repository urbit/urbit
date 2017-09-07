### OSXCROSS BUGS ###

-------------

***ISSUE:***

GCH generation does not work with `gcc` or `clang++-gstdc++`/`-foc-use-gcc-libstdc++`
if `-c` or `-x<lang>-header` is **not** passed to the compiler.

Example:

    $ o64-gcc test.h
    Undefined symbols for architecture x86_64:
    "_main", referenced from:
    start in crt1.10.6.o

This is due to `-Wl,-no_compact_unwind` being passed to the compiler under the hood.

Example:

    $ x86_64-apple-darwin14-base-gcc
    x86_64-apple-darwin14-base-gcc: fatal error: no input files

    $ x86_64-apple-darwin14-base-gcc -Wl,-no_compact_unwind
    Undefined symbols for architecture x86_64:
    "_main", referenced from:
    start in crt1.10.6.o
    ld: symbol(s) not found for architecture x86_64

***WORKAROUND:***

Add `-c` or `-x<lang>-header` to the compiler flags.

Example:

    o64-clang++-gstdc++ test.hpp              # BAD
    o64-clang++-gstdc++ -xc++-header test.hpp # OK
    o64-clang++-gstdc++ test.hpp -c           # OK
    o64-g++ test.hpp -c                       # OK

-------------
