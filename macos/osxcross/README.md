## OS X Cross toolchain for Linux, *BSD and Cygwin ##

### WHAT IS THE GOAL OF OSXCROSS? ###

The goal of OSXCross is to provide a well working OS X cross toolchain for
Linux, *BSD, and Cygwin.

### HOW DOES IT WORK? ###

For cross-compiling for OS X you need
* the Clang/LLVM compiler
* the the [cctools](http://www.opensource.apple.com/tarballs/cctools)
  (ld, lipo, …), and
* the OSX SDK.

[Clang/LLVM is a cross compiler by default](http://clang.llvm.org/docs/CrossCompilation.html)
and is now available on nearly every Linux distribution, so we just
need a proper [port](https://github.com/tpoechtrager/cctools-port) of
the cctools and the OS X SDK.

OSXCross includes a collection of scripts for preparing the SDK and
building the cctools.

It also includes scripts for optionally building
* Clang using gcc (for the case your distribution does not include it),
* an up-to-date vanilla GCC as a cross-compiler for target OS X,
* the "compiler-rt" runtime library, and
* the `llvm-dsymutil` tool required for debugging.


### WHAT CAN I BUILD WITH IT? ###

Basically everything you can build on OS X with clang/gcc should build with
this cross toolchain as well.

### PACKET MANAGERS ###

OSXCross comes with a minimalistic MacPorts Packet Manager.
See [README.MACPORTS](README.MACPORTS.md) for more.

### INSTALLATION: ###

*Windows/Cygwin users should follow [README.CYGWIN](README.CYGWIN.md).*

Move your
[packaged SDK](https://github.com/tpoechtrager/osxcross#packaging-the-sdk)
to the tarballs/ directory.

Then ensure you have the following installed on your system:

`Clang 3.2+`, `patch`, `libxml2-devel` (<=10.6 only) and the `bash shell`.

You can run 'sudo tools/get\_dependencies.sh' to get these (and the
optional packages) automatically.

*Optional:*

- `llvm-devel`: For Link Time Optimization support
- `uuid-devel`: For ld64 `-random_uuid` support
- `llvm-devel` + `xar-devel`: For ld64 `-bitcode_bundle` support

You can find xar [here](https://github.com/mackyle/xar).
Do not install libxar-dev on Ubuntu, it's a different package.


##### Building Clang #####

OSXCross uses `clang` as the default compiler for building its tools, and also
as a cross-compiler to create OSX binaries.

In `clang` there is no difference between cross-compilation and native
compilation, so OSXCross can use a normal `clang` install for both.  You can
use either a `clang` installation you already have, or build your own from
source.

To build and install your own `clang` from a recent source tree, using `gcc`,
run:

```shell
    ./build_clang.sh
```

This installs `clang` into `/usr/local`.  If you want to install somewhere
else, set the `INSTALLPREFIX` variable.  For example:

```shell
    INSTALLPREFIX=/opt/clang ./build_clang.sh
```

On debian-like systems you can also use [llvm.org/apt](http://llvm.org/apt) to
get a newer version of clang.
But be careful, that repository is known to cause
[troubles](https://github.com/tpoechtrager/osxcross/issues/16).


##### Building OSXCross #####

To build the cross toolchain (using `clang`), run:

```shell
    ./build.sh
```

Or, set variable `UNATTENDED` to `1` to skip the prompt and proceed straight to
the build:

```shell
    UNATTENDED=1 ./build.sh
```

(This will search 'tarballs' for your SDK and then build in its own directory.)

**Once this is done:** add `<path>/target/bin` to your PATH variable so that
you can invoke the cross-compiler.

That's it. See usage examples below.

##### Building GCC: #####

If you also want to build GCC as a cross-compiler, you can do that by running:

```shell
    ./build_gcc.sh
```

The script lets you select a GCC version by setting the variable `GCC_VERSION`.
 By default you get C and C++ compilers, but you can tell the script to build a
Fortran compiler as well:

```shell
    GCC_VERSION=5.2.0 ENABLE_FORTRAN=1 ./build_gcc.sh
```

\[A gfortran usage example can be found [here](https://github.com/tpoechtrager/osxcross/issues/28#issuecomment-67047134)]

Before you do this, make sure you have the GCC build depedencies installed on
your system.

On debian like systems you can install these using:

```shell
    sudo apt-get install gcc g++ zlib1g-dev libmpc-dev libmpfr-dev libgmp-dev
```

ATTENTION:

OSXCross links libgcc and libstdc++ statically by default (this affects
`-foc-use-gcc-libstdc++` too).  You can turn this behavior off with
`OSXCROSS_GCC_NO_STATIC_RUNTIME=1` (env).

The build also creates aliases `*-g++-libc++` which link with the `clang`
implementation of the C++ standard library instead of the GCC version.  Don't
use these variants unless you know what you're doing.

### PACKAGING THE SDK: ###

**[Please ensure you have read and understood the Xcode license
   terms before continuing.](https://www.apple.com/legal/sla/docs/xcode.pdf)**

##### Packaging the SDK on Mac OS X: #####

1. [Download [Xcode](https://developer.apple.com/downloads/index.action?name=Xcode%207.3) \*\*]
2. [Mount Xcode.dmg (Open With -> DiskImageMounter) \*\*\*]
3. Run: `./tools/gen_sdk_package.sh` (from the OSXCross package)
4. Copy the packaged SDK (\*.tar.\* or \*.pkg) on a USB Stick
5. (On Linux/BSD) Copy or move the SDK into the tarballs/ directory of
   OSXCross.

\*\* Xcode up to 7.3.x is known to work.

\*\*\* If you get a dialog with a crossed circle, ignore it.  You don't need
to install Xcode.

Step 1. and 2. can be skipped if you have Xcode installed.

##### Packing the SDK on Linux, Method 1 (works up to Xcode 7.3): #####

1. Download Xcode like described in 'Packaging the SDK on Mac OS X'
2. Install `cmake`, `libxml2-dev` and `fuse`
3. Run `./tools/gen_sdk_package_darling_dmg.sh <xcode>.dmg`
4. Copy or move the SDK into the tarballs/ directory

##### Packing the SDK on Linux, Cygwin (and others), Method 2 (works up to Xcode 7.2): #####

1. Download Xcode like described in 'Packaging the SDK on Mac OS X'
2. Ensure you have `clang` and `make` installed
3. Run `./tools/gen_sdk_package_p7zip.sh <xcode>.dmg`
4. Copy or move the SDK into the tarballs/ directory

##### Packing the SDK on Linux, Method 3 (works up to Xcode 4.2): #####

1. Download Xcode 4.2 for Snow Leopard
2. Ensure you are downloading the "Snow Leopard" version
3. Install `dmg2img`
4. Run (as root): `./tools/mount_xcode_image.sh /path/to/xcode.dmg`
5. Follow the instructions printed by `./tools/mount_xcode_image.sh`
6. Copy or move the SDK into the tarballs/ directory


### USAGE EXAMPLES: ###

##### Example.  To compile a file called test.cpp, you can run: #####

* Clang:

  * 32 bit: `o32-clang++ test.cpp -O3 -o test` OR
    `i386-apple-darwinXX-clang++ test.cpp -O3 -o test`
  * 64 bit: `o64-clang++ test.cpp -O3 -o test` OR
    `x86_64-apple-darwinXX-clang++ test.cpp -O3 -o test`

* GCC:

  * 32 bit:  `o32-g++ test.cpp -O3 -o test` OR
    `i386-apple-darwinXX-g++ test.cpp -O3 -o test`
  * 64 bit:  `o64-g++ test.cpp -O3 -o test` OR
    `x86_64-apple-darwinXX-g++ test.cpp -O3 -o test`

XX= the target version, you can find it out by running  `osxcross-conf` and
then see `TARGET`.

You can use the shortcuts `o32-...` for `i386-apple-darwin...`, depending on
which you prefer.

*I'll continue from here on with `o32-clang`, but remember,
 you can simply replace it with `o32-gcc` or `i386-apple-darwin...`.*

##### Building Makefile based projects: #####

  * `make CC=o32-clang CXX=o32-clang++`

##### Building automake based projects: #####

  * `CC=o32-clang CXX=o32-clang++ ./configure --host=i386-apple-darwinXX`

##### Building test.cpp with libc++: #####

Note: libc++ requires Mac OS X 10.7 or newer! If you really need C++11 for
an older OS X version, then you can do the following:

1. Build GCC so you have an up-to-date libstdc++
2. Build your source code with GCC or
   `clang++-gstdc++` / `clang++ -foc-use-gcc-libstdc++`

Usage Examples:

* Clang:

  * C++98: `o32-clang++ -stdlib=libc++ test.cpp -o test`
  * C++11: `o32-clang++ -stdlib=libc++ -std=c++11 test1.cpp -o test`
  * C++14: `o32-clang++ -stdlib=libc++ -std=c++14 test1.cpp -o test`
  * C++1z: `o32-clang++ -stdlib=libc++ -std=c++1z test1.cpp -o test`

* Clang (shortcut):

  * C++98: `o32-clang++-libc++ test.cpp -o test`
  * C++11: `o32-clang++-libc++ -std=c++11 test.cpp -o test`
  * C++14: `o32-clang++-libc++ -std=c++14 test.cpp  -o test`
  * C++1z: `o32-clang++-libc++ -std=c++1z test.cpp  -o test`

* GCC

  * C++11: `o32-g++-libc++ -std=c++11 test.cpp`
  * C++14: `o32-g++-libc++ -std=c++14 test.cpp -o test`
  * C++1z: `o32-g++-libc++ -std=c++1z test.cpp -o test`

##### Building test1.cpp and test2.cpp with LTO (Link Time Optimization): #####

  * build the first object file: `o32-clang++ test1.cpp -O3 -flto -c`
  * build the second object file: `o32-clang++ test2.cpp -O3 -flto -c`
  * link them with LTO: `o32-clang++ -O3 -flto test1.o test2.o -o test`

##### Building a universal binary: #####

* Clang:
  * `o64-clang++ test.cpp -O3 -arch i386 -arch x86_64 -o test`
* GCC:
  * build the 32 bit binary: `o32-g++ test.cpp -O3 -o test.i386`
  * build the 64 bit binary: `o64-g++ test.cpp -O3 -o test.x86_64`
  * use lipo to generate the universal binary:
    `x86_64-apple darwinXX-lipo -create test.i386 test.x86_64 -output test`

### DEPLOYMENT TARGET: ###

The default deployment target is `Mac OS X 10.5`.

However, there are several ways to override the default value:

1. by passing `OSX_VERSION_MIN=10.x` to `./build.sh`
2. by passing `-mmacosx-version-min=10.x` to the compiler
3. by setting the `MACOSX_DEPLOYMENT_TARGET` environment variable

\>= 10.9 also defaults to `libc++` instead of `libstdc++`, this behavior
can be overriden by explicitly passing `-stdlib=libstdc++` to clang.

x86\_64h defaults to `Mac OS X 10.8` and requires clang 3.5+.
x86\_64h = x86\_64 with optimizations for the Intel Haswell Architecture.

### BUILDING OSXCROSS WITH GCC: ###

You can build OSXCross with GCC this way:

`CC=gcc CXX=g++ ./build.sh`

You will need gcc/g++/gcc-objc 4.7+.

### PROJECTS USING OSXCROSS: ###

* [multiarch/crossbuild](https://github.com/multiarch/crossbuild): various
  cross-compilers (**Systems**: Linux, OS X, Windows, **Archs**: x86\_64,
  i386, arm, ppc, mips) in Docker. OSXCross powers the Darwin builds.
* [Smartmontools](https://www.smartmontools.org)

### LICENSE: ####
  * scripts/wrapper: GPLv2
  * cctools/ld64: APSL 2.0
  * xar: New BSD

### CREDITS: ####
 * [cjacker for the initial cctools port](https://code.google.com/p/ios-toolchain-based-on-clang-for-linux/source/browse/#svn%2Ftrunk%2Fcctools-porting%2Fpatches)
