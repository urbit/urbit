# The following program, which was distilled from Qt's qrandom.cpp, causes an
# internal compiler error in i686-w64-mingw32-g++ version 7.3.0 and 8.2.0:
#   __attribute__((__target__("rdrnd"))) void f(unsigned int * b) noexcept {
#     __builtin_ia32_rdrand32_step(b);
#   }

{ native, arch, stage ? 2, binutils, libc }:

let
  nixpkgs = native.nixpkgs;
  isl = nixpkgs.isl;
  inherit (nixpkgs) stdenv lib fetchurl;
  inherit (nixpkgs) gettext gmp libmpc libelf mpfr texinfo which zlib;

  stageName = if stage == 1 then "-stage1"
              else assert stage == 2; "";
in

native.make_derivation rec {
  name = "gcc-${version}-${target}${stageName}";

  target = "${arch}-w64-mingw32";

  version = "7.3.0";

  src = fetchurl {
    url = "mirror://gnu/gcc/gcc-${version}/gcc-${version}.tar.xz";
    sha256 = "0p71bij6bfhzyrs8676a8jmpjsfz392s2rg862sdnsk30jpacb43";
  };

  builder = ./builder.sh;

  patches = [
    ./mingw-search-paths.patch
    ./libstdc++-target.patch
    ./no-sys-dirs.patch
    ./cppdefault.patch
  ];

  native_inputs = [
    binutils texinfo gettext which
  ];

  configure_flags =
    "--target=${arch}-w64-mingw32 " +
    "--with-sysroot=${libc} " +
    "--with-native-system-header-dir=/include " +
    "--with-gnu-as " +
    "--with-gnu-ld " +
    "--with-as=${binutils}/bin/${arch}-w64-mingw32-as " +
    "--with-ld=${binutils}/bin/${arch}-w64-mingw32-ld " +
    "--with-isl=${isl} " +
    "--with-gmp-include=${gmp.dev}/include " +
    "--with-gmp-lib=${gmp.out}/lib " +
    "--with-mpfr-include=${mpfr.dev}/include " +
    "--with-mpfr-lib=${mpfr.out}/lib " +
    "--with-mpc=${libmpc} " +
    "--with-zlib-include=${zlib.dev}/include " +
    "--with-zlib-lib=${zlib.out}/lib " +
    "--enable-lto " +
    "--enable-plugin " +
    "--enable-static " +
    "--enable-sjlj-exceptions " +
    "--enable-__cxa_atexit " +
    "--enable-long-long " +
    "--with-dwarf2 " +
    "--enable-fully-dynamic-string " +
    (if stage == 1 then
      "--enable-languages=c " +
      "--enable-threads=win32 "
    else
      "--enable-languages=c,c++ " +
      "--enable-threads=posix "
    ) +
    "--without-included-gettext " +
    "--disable-libstdcxx-pch " +
    "--disable-nls " +
    "--disable-shared " +
    "--disable-multilib " +
    "--disable-libssp " +
    "--disable-win32-registry " +
    "--disable-bootstrap";

  make_flags =
    if stage == 1 then
      ["all-gcc" "all-target-libgcc"]
    else
      [];

  install_targets =
    if stage == 1 then
      ["install-gcc install-target-libgcc"]
    else
      ["install-strip"];

  hardeningDisable = [ "format" ];
}

# TODO: why is GCC providing a fixed limits.h?
