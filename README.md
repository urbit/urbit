# Install instructions

To install and run Urbit please follow the instructions at
[urbit.org/docs/using/install](http://urbit.org/docs/using/install).  Packages
and source tarballs are available there. You'll be on the live network in a few
minutes.

If you're doing development on Urbit, keep reading.

# Build instructions

[![Build Status](https://travis-ci.org/urbit/urbit.svg?branch=master)](https://travis-ci.org/urbit/urbit)

## External dependencies

`vere`, the Urbit virtual machine, depends on the following:

- C compiler ([gcc](https://gcc.gnu.org) or [clang](http://clang.llvm.org))
- [GNU Make](https://www.gnu.org/software/make/)
- [GMP](https://gmplib.org)
- [CMake](https://cmake.org)
- [libuv](https://libuv.org) 1.x
- [OpenSSL](https://www.openssl.org)
- [libsigsegv](https://www.gnu.org/software/libsigsegv/)
- [libcurl](https://curl.haxx.se/libcurl/)
- curses implementation (ncurses on Linux distributions, OS curses otherwise)
- [Ragel](https://www.colm.net/open-source/ragel/)
- [re2c](http://re2c.org)

Most of these dependencies are unfortunate; we aim to drastically shrink the
list in upcoming versions. `vere` proper makes use of GMP, OpenSSL, libcurl, and
libsigsegv. The multiple build tools are a result of bundled libraries, slated
for future unbundling or removal wherever possible.

## Building

Our Makefile should handle the build smoothly on all supported platforms. It's
just a simple Makefile, written by hand for GNU Make, and the most complicated
parts of its internal machinery have to do with the varied build systems of the
bundled libraries.

Useful targets are the default `all`, `clean`, and `distclean`. The last may not
produce an entirely clean distribution directory, and runs a bundled library's
configure script; `git clean` may be a better option.

The `vere` binary is produced in `bin/urbit`.

## Contact

If you have any questions, problems, patches, or proposals for patches, please
feel free to get in touch in whatever way is most convenient:

- Post to `/urbit-meta` on Urbit `:talk`.  (You can do this via
[urbit.org/stream](https://urbit.org/stream) without a running Urbit).
- Post to [urbit.org/fora](https://urbit.org/fora/).
- Email us directly [questions@urbit.org](mailto:questions@urbit.org).  
