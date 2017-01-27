{ nixpkgs, arch, stage ? 2, binutils, libc }:

# TODO: why is GCC providing a fixed limits.h?

let
  isl = nixpkgs.isl_0_14;
  inherit (nixpkgs) stdenv lib fetchurl;
  inherit (nixpkgs) gettext gmp libmpc libelf mpfr texinfo which zlib;

  stageName = if stage == 1 then "-stage1"
              else assert stage == 2; "";
in

stdenv.mkDerivation rec {
  name = "gcc-${version}-${target}${stageName}";

  target = "${arch}-w64-mingw32";

  version = "6.3.0";

  src = fetchurl {
    url = "mirror://gnu/gcc/gcc-${version}/gcc-${version}.tar.bz2";
    sha256 = "17xjz30jb65hcf714vn9gcxvrrji8j20xm7n33qg1ywhyzryfsph";
  };

  builder = ./builder.sh;

  patches = [
    ./use-source-date-epoch.patch
    ./libstdc++-target.patch
    ./no-sys-dirs.patch
    ./cppdefault.patch
  ];

  buildInputs = [
    binutils gettext gmp isl libmpc libelf mpfr texinfo which zlib
  ];

  configureFlags =
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
    "--with-system-zlib " +
    "--enable-lto " +
    "--enable-plugin " +
    "--enable-static " +
    "--enable-threads=win32 " +
    "--enable-sjlj-exceptions " +
    "--enable-__cxa_atexit " +
    "--enable-long-long " +
    "--with-dwarf2 " +
    "--enable-fully-dynamic-string " +
    (if stage == 1 then
      "--enable-languages=c "
    else
      "--enable-languages=c,c++ "
    ) +
    "--without-included-gettext " +
    "--disable-libstdcxx-pch " +
    "--disable-nls " +
    "--disable-shared " +
    "--disable-multilib " +
    "--disable-libssp " +
    "--disable-win32-registry " +
    "--disable-bootstrap";

  makeFlags =
    if stage == 1 then
      ["all-gcc" "all-target-libgcc"]
    else
      [];

  installTargets =
    if stage == 1 then
      ["install-gcc install-target-libgcc"]
    else
      ["install-strip"];

  enableParallelBuilding = true;

  # TODO: some day submit patches to GCC, remove this line
  hardeningDisable = [ "format" ];

  dontDisableStatic = true;

  dontStrip = true;

  meta = {
    homepage = http://gcc.gnu.org/;
    license = lib.licenses.gpl3Plus;
  };
}
