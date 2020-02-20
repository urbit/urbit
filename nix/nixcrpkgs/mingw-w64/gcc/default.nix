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

  version = "8.2.0";

  src = fetchurl {
    url = "mirror://gnu/gcc/gcc-${version}/gcc-${version}.tar.xz";
    sha256 = "10007smilswiiv2ymazr3b6x2i933c0ycxrr529zh4r6p823qv0r";
  };

  builder = ./builder.sh;

  patches = [
    # Make it so GCC does not force us to have a "mingw" symlink.
    ./mingw-search-paths.patch

    # Make --with-sysroot and --with-native-system-header-dir work
    # as described in the GCC documentation.
    ./cppdefault.patch

    # Remove hardcoded absolute paths.
    ./no-sys-dirs.patch

    # Fix a bug in GCC that causes an internal compiler error when
    # compiling Qt's qrandom.cpp:
    # https://gcc.gnu.org/bugzilla/show_bug.cgi?id=58372#c14
    # Patch is from comment #42 by Uros Bizjak.
    # https://gcc.gnu.org/viewcvs/gcc/branches/gcc-8-branch/gcc/cfgexpand.c?view=patch&r1=264557&r2=266014&pathrev=266014
    ./stack-alignment-58372.patch

    # This patch is from nixpkgs.
    ./libstdc++-target.patch

    # Fix compilation errors about missing ISL function declarations.
    # https://gcc.gnu.org/git/?p=gcc.git;a=patch;h=05103aed1d34b5ca07c9a70c95a7cb1d47e22c47
    ./isl-headers.patch
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
