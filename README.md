# Install instructions

To install and run Urbit please follow the instructions at
[urbit.org/docs/using/install](http://urbit.org/docs/using/install).  Packages
and source tarballs are available there. You'll be on the live network in a few
minutes.

If you're doing development on Urbit, keep reading.

# Build instructions

[![Build Status](https://travis-ci.org/urbit/urbit.svg?branch=maint-0.4)](https://travis-ci.org/urbit/urbit)

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
- [re2c](http://re2c.org)

Most of these dependencies are unfortunate; we aim to drastically shrink the
list in upcoming versions. `vere` proper makes use of GMP, OpenSSL, libcurl, and
libsigsegv. The multiple build tools are a result of bundled libraries, slated
for future unbundling or removal wherever possible.

## Building

Urbit uses Meson build system.

Some libraries which are not found in major distributions:
- ed25519
- http-parser legacy version 0.1.0
- murmur3
- softfloat3
- urbit-scrypt
- commonmark legacy version 0.12.0

are included as git submodules. To build urbit from source, perform the following steps:

## MacOS specifics
On macos, you need to make sure `pkg-config` uses the correct homebrew path.
 The `export PKG_CONFIG_PATH=/usr/local/opt/openssl/lib/pkgconfig/:$PKG_CONFIG_PATH`
 should setup the `pkg-config` path correctly, solving errors with homebrew package discovery (notably with `openssl` paths).

## Configuration & compilation
(For instructions for legacy meson, also see below)

1. Install all required dependencies.
2. `git submodule init` in the urbit repository
3. `git submodule update`
4. `meson ./build`
5. If the last step was successful, type `ninja -C build` to compile urbit.
6. The executable should appear in `./build` directory.

### Using meson & ninja
To configure project, enter the build directory and enter
`meson configure`. Without any arguments this command will display available
options. For example, to compile debug build of urbit, use
`meson configure -Ddebug=true`.
To set the prefix for installation use
`meson configure -Dprefix=/usr`, and so on.

## Configuration & compilation for legacy meson

The syntax for legacy meson (Version `0.29`) is a bit different.
1. Manually create `build` directory and invoke meson as `meson . ./build`
2. If you want to set options, this is done in one step.
   Use `meson -D [options] . ./build` to prepare customized build.

Once the project is configured, use `ninja` to build it.
To install it into the default prefix, use `ninja install`.
If you want to specify custom `DESTDIR`, use `DESTDIR=... ninja install`.

## Building the Debian Package

To build a .deb file for installation on Debian platforms, perform the
following steps:
+ Run `sudo apt install devscripts` to install the `debuild` utility.
+ Update the `debian/changelog` to reflect the changes in this release.
+ If necessary, update the year of the copyright in `debian/copyright`.
+ Clean any build artifacts: Run `make clean` and delete the `bin` directory,
if it exists.
+ Run `tar -xcvf ../urbit-x.y.z.orig.tar.gz .` from the top-level folder in
the repo.  This command will create an archive in the directory above the
current directory, which will be used in packaging.
+ Run `debuild -us -uc`, also from the top-level folder in the repo. This
creates a .deb file in the folder above the current directory.

The resulting .deb file should now exist in the folder above the current
directory. To test that the .deb file works properly, you can perform the
following steps:
+ Uninstall urbit: `sudo apt remove urbit`.
+ Run `sudo dpkg -i ../urbit-x.y.z_amd64.deb` to install the new version.
+ Boot up a ship using the `urbit` command.

## Contact

If you have any questions, problems, patches, or proposals for patches, please
feel free to get in touch in whatever way is most convenient:

- Post to `/urbit-meta` on Urbit `:talk`.  (You can do this via
[urbit.org/stream](https://urbit.org/stream) without a running Urbit).
- Post to [urbit.org/fora](https://urbit.org/fora/).
- Email us directly [questions@urbit.org](mailto:questions@urbit.org).  
