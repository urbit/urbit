> The Urbit address space is now live on the Ethereum blockchain. We’re calling it ‘Azimuth’ and you can find it at [`0x223c067f8cf28ae173ee5cafea60ca44c335fecb`](https://etherscan.io/address/0x223c067f8cf28ae173ee5cafea60ca44c335fecb) or [`azimuth.eth`](https://etherscan.io/address/azimuth.eth). Owners of Azimuth ‘points’ (galaxies, stars or planets) can use [Bridge](https://github.com/urbit/bridge/releases) to manage them and view their balance now. Sometime in the next few days, owners of Azimuth points will be able to boot Arvo, the Urbit OS, from their Azimuth point and request access to one of our ‘cities’: private communities for chat and discussion. These new cities use Landscape, a brand new UI for using Urbit in the browser.

# Install instructions

To install and run Urbit please follow the instructions at
[urbit.org/docs/getting-started/](https://urbit.org/docs/getting-started/).
Packages and source tarballs are available there. You'll be on the live network
in a few minutes.

If you're doing development on Urbit, keep reading.

# Build instructions

[![Build Status](https://travis-ci.org/urbit/urbit.svg?branch=master)](https://travis-ci.org/urbit/urbit)

## External dependencies

`vere`, the Urbit virtual machine, depends on the following:

- C compiler ([gcc](https://gcc.gnu.org) or [clang](http://clang.llvm.org))
- [Meson](http://mesonbuild.com/)
- [GMP](https://gmplib.org)
- [OpenSSL](https://www.openssl.org)
- [libsigsegv](https://www.gnu.org/software/libsigsegv/)
- [libcurl](https://curl.haxx.se/libcurl/)
- [libuv](http://libuv.org)
- curses implementation (ncurses on Linux distributions, OS curses otherwise)

Most of these dependencies are unfortunate; we aim to drastically shrink the
list in upcoming versions. `vere` proper makes use of GMP, OpenSSL, libcurl, and
libsigsegv.

## Building

Urbit uses Meson build system.

Some libraries which are not found in major distributions:

- ed25519
- libh2o
- murmur3
- softfloat3
- scrypt

are included as git submodules. To build urbit from source, perform the following steps:

## Configuration & compilation
(For instructions for legacy meson, also see below)

1. Install all required dependencies.
2. Run `./scripts/bootstrap`
3. Run `./scripts/build`
4. The executable should appear in `./build` directory.

### Using meson & ninja

To configure the project, enter the build directory and enter
`meson configure -Dbuildtype=release`.  To compile a debug build of urbit, use
`meson configure -Dbuildtype=debug`.
To set a prefix for installation use
`meson configure -Dprefix=/usr`.

## Configuration & compilation for legacy meson

The syntax for legacy meson (Version `0.29`) is a bit different.

1. Manually create `build` directory and invoke meson as `meson . ./build`
2. If you want to set options, this is done in one step.
   Use `meson -D [options] . ./build` to prepare customized build.

Once the project is configured, use `ninja` to build it.
To install it into the default prefix, use `ninja install`.
If you want to specify custom `DESTDIR`, use `DESTDIR=... ninja install`.

## Contact

We are using our new UI, Landscape to run a few experimental cities.
If you have an Azimuth point, please send us your planet name at
[support@urbit.org](mailto:support@urbit.org) to request access.
