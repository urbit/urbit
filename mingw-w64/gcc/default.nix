{ native, arch, stage ? 2, binutils, libc }:

let
  nixpkgs = native.nixpkgs;
  isl = nixpkgs.isl_0_14;
  inherit (nixpkgs) stdenv lib fetchurl;
  inherit (nixpkgs) gettext gmp libmpc libelf mpfr texinfo which zlib;

  stageName = if stage == 1 then "-stage1"
              else assert stage == 2; "";
in

native.make_derivation rec {
  name = "gcc-${version}-${target}${stageName}";

  target = "${arch}-w64-mingw32";

  version = "6.3.0";

  src = fetchurl {
    url = "mirror://gnu/gcc/gcc-${version}/gcc-${version}.tar.bz2";
    sha256 = "17xjz30jb65hcf714vn9gcxvrrji8j20xm7n33qg1ywhyzryfsph";
  };

  builder = ./builder.sh;

  patches = [
    # TODO: combine three of these patches into one called search-dirs.patch
    ./mingw-search-paths.patch
    ./use-source-date-epoch.patch
    ./libstdc++-target.patch
    ./no-sys-dirs.patch
    ./cppdefault.patch

    # Fix a compiler error in GCC's ubsan.c: ISO C++ forbids comparison
    # between pointer and integer.
    ./ubsan.patch
  ];

  # TODO: can probably remove libelf here, and might as well remove
  # the libraries that are given to GCC as configure flags
  # TODO: just let GCC use its own gettext (intl)
  native_inputs = [
    binutils gettext libelf texinfo which zlib
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
    "--disable-bootstrap";  # TODO: not needed, --disable-bootstrap
                            # only applies to native builds

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
