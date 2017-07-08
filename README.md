# Nixcrpkgs

[www.pololu.com](https://www.pololu.com/)

*Nixcrpkgs* is a collection of tools for cross-compiling statically-linked,
standalone software applications.  With Nixcrpkgs, you can specify what
platforms you want to target, what libraries and build tools you depend on, and
the commands that build your software.  When you build your software, Nixcrpkgs
will automatically take care of building everything you need, including
cross-compilers and libraries.

Nixcrpkgs primarily consists of *Nix expressions*, which are recipes for
building software with [Nix, the purely functional package
manager][nix].  These recipes build on top of the [Nix
Packages collection (Nixpkgs)][nixpkgs].

Currently, the only supported target platforms are 32-bit Windows and 64-bit
Windows, but we plan to support macOS and Linux in the future.  The only
supported build platform is Linux.  The only languages currently supported for
cross-compiling are C and C++, but almost any language can be used in the build
process.

## Getting started

To get started, you should first install Nix on a Linux machine by following the
instructions on the [Nix website][nix].

Next, clone or download the this repository and use `cd` to change into the
top-level directory of this repository.

To build a simple "Hello, World!" program for Windows, run:

    nix-build -A win32.hello

The first time you run this command, it will take a while because Nix has to
build a cross-compiling toolchain.  When `nix-build` is done, it will print the
name of a directory in `/nix/store` that holds the resulting program, and it
will create a symbolic link in the current directory named `result` that points
to that directory.

If you copy `result/bin/hello.exe` to a Windows machine and run it, you should
see a message box appear that says "Hello, World!".

If you run `nix-build -A win32.hello` a second time, Nix will detect that
nothing about the build recipes has changed, so it will simply print the
directory name and remake the symbolic link.

To see how the `hello` package is specified in Nixcrpkgs, you can look in
`pkgs.nix` and the `pkgs/hello` directory.  To see how the mingw-w64
cross-compiler was specified, you can look in the `mingw-w64` directory.  If you
change any of the build recipes for `hello` or its dependencies and then run the
`nix-build` command again, Nix will automatically rebuild those dependencies and
anything that depends on them, ensuring that you always get a consistent build.

### Integrating Nixcrpkgs into your project

The instructions above show how to cross-compile a "Hello, World!" program that
is included with Nixcrpkgs.  Instead of adding your project to Nixcrpkgs, you
will probably want to just use Nixcrpkgs as a tool in your project.  To get an
idea of how to do that, you can look at other projects that have done the same.
In the projects listed below, you should look for a file named `default.nix` in
the top-level directory and look for build instructions that explain how to use
it.

* The [Pololu USB AVR Programmer v2 software](https://github.com/pololu/pololu-usb-avr-programmer-v2) is a C++ project that uses CMake and Nixcrpkgs.

[nix]: http://nixos.org/nix/
[nixpkgs]: http://nixos.org/nixpkgs/