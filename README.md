# nixcrpkgs

[www.pololu.com](https://www.pololu.com/)

*nixcrpkgs* is a collection of tools for cross-compiling statically-linked,
standalone software applications.  With nixcrpkgs, you can specify what
platforms you want to target, what libraries and build tools you depend on, and
the commands that build your software.  When you build your software, nixcrpkgs
will automatically take care of building or retrieving everything you need,
including cross-compilers and libraries.

nixcrpkgs primarily consists of *Nix expressions*, which are recipes for
building software with [Nix, the purely functional package
manager][nix].  These recipes build on top of the [Nix
Packages collection (Nixpkgs)][nixpkgs].

## Features

- Supported target platforms:
  - Windows (32-bit or 64-bit) using [mingw-w64](https://mingw-w64.org/) and [GCC](https://gcc.gnu.org/) 6.3.0
  - Linux (32-bit, 64-bit, and ARM) using [musl](https://www.musl-libc.org/) and [GCC](https://gcc.gnu.org/) 6.3.0
  - macOS
- Supported languages for cross-compiling:
  - C
  - C++
- Supported build platforms:
  - Linux
- Supported build tools:
  - [CMake](https://cmake.org/)
  - [GNU Make](https://www.gnu.org/software/make/)
  - [Ninja](https://ninja-build.org/)
  - pkg-config (as implemented by [pkgconf](https://github.com/pkgconf/pkgconf))
  - [GNU Bash](https://www.gnu.org/software/bash/)
  - [Ruby](https://www.ruby-lang.org/)
  - [GDB](https://www.gnu.org/software/gdb/)
- Notable supported libraries:
  - [Qt](https://www.qt.io/) 5.9.2
  - [libusbp](https://github.com/pololu/libusbp)
  - [Windows API](https://en.wikipedia.org/wiki/Windows_API) (thanks to mingw-w64)


## Getting started

To get started, you should first install Nix on a Linux machine by following the
instructions on the [Nix website][nix].

Next, run `df -h` to make sure you have enough disk space.

- The filesystem that holds `/nix` should also have several gigabytes of free
space.  Each GCC cross-compiler takes about 300 MB while each Qt installation
takes about 800 MB.
- The filesystem that holds `/tmp` should have at least 4 gigabytes of free
space, which will be needed while building cross-compilers.  If that is not the
case on your system, you can set the `TMPDIR` environment variable to tell
`nix-build` to perform its builds in a different directory on a filesystem with
more free space.

Next, clone or download this repository and use `cd` to change into the
top-level directory.

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

To see how the `hello` package is specified in nixcrpkgs, you can look in
`pkgs.nix` and the `pkgs/hello` directory.  To see how the GCC cross-compiler
for Windows was specified, you can look in the `mingw-w64` directory.  If you
change any of the build recipes for `hello` or its dependencies and then run the
`nix-build` command again, Nix will automatically rebuild those dependencies and
anything that depends on them, ensuring that you always get a consistent build.


### Obtaining the macOS SDK

If you are trying to build software for macOS, you will need to get
macOS SDK tarball and put it in the the right place.  Otherwise, you
will get an error like this:

    error: getting attributes of path '/home/yourname/nixcrpkgs/macos/MacOSX.sdk.tar.xz': No such file or directory

To generate the tarball, follow these steps:

1. On a macOS machine, install [Xcode](https://developer.apple.com/xcode/).
2. Download this repository to the machine.
3. In a Terminal window, run the `macos/gen_sdk_package.sh` script from this repository.
4. After several minutes, the current directory should have a tarball with a name like
  `MacOSX10.12.sdk.tar.xz` and a size of about 25 MB.
5. Copy the SDK tarball file to the machine where you will be building software,
  and put it in the `macos` directory.
6. The nixcrpkgs build recipe for the SDK is hardcoded to look for a file named
  `MacOSX.sdk.tar.xz`, so rename the tarball to that.
7. Consider keeping a backup of the tarball so you can always rebuild any software you
  made with it.

Now you should be able to build your software for macOS.


## Integrating nixcrpkgs into your project

The instructions above show how to cross-compile a "Hello, World!" program that
is included with nixcrpkgs.  Instead of including your project in nixcrpkgs like
the hello program, you will probably want to just use nixcrpkgs as a tool in
your project.  To get an idea of how to do that, you can look at other projects
that have done the same.  In the projects listed below, you should look for a
file named `default.nix` in the top-level directory and look for build
instructions that explain what `nix-build` commands to run.

* The [Pololu Tic Stepper Motor Controller software](https://github.com/pololu/pololu-tic-software) is a C/C++ project that uses CMake and nixcrpkgs.
* The [Pololu USB AVR Programmer v2 software](https://github.com/pololu/pololu-usb-avr-programmer-v2) is a C++ project that uses CMake and nixcrpkgs.
* The [Pololu USB Bootloader Utility (p-load)](https://github.com/pololu/p-load) is a C++ project that uses CMake and nixcrpkgs.

[nix]: http://nixos.org/nix/
[nixpkgs]: http://nixos.org/nixpkgs/


## Maintaining the nixcrpkgs system

You should occasionally run `nix-collect-garbage` to remove items that are no
longer needed and reclaim your disk space.  However, note that Nix will
typically remove all of your cross compilers and libraries when you run this
command, so be prepared to do a lengthy mass rebuild.  The Nix manual has more
information about [Nix garbage
collection](http://nixos.org/nix/manual/#sec-garbage-collection).

You should occasionally run `nix-channel --update` to update to the latest
version of Nixpkgs.  However, when doing this, be aware that the new version of
Nixpkgs might require you to do a mass rebuild.

You should occasionally update your checkout of the nixcrpkgs repository to get
the latest versions of build tools, new features, and bug fixes.  Once again,
this might require a mass rebuild.

If you want your builds to be very stable and reliable, you could make forks of
nixcrpkgs and/or Nixpkgs and update them at your own pace, carefully considering
any changes made by others before merging them in.  That's one of the beauties
of Nix when compared to other package management systems: you will never be
forced to upgrade your build tools, and using old tools is just as easy as using
new ones.  You can use the `NIX_PATH` environment variable to tell `nix-build`
to use your forked versions.
