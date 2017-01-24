{ nixpkgs, arch, stage ? 2, binutils, libc }:

# TODO: why is GCC providing a fixed limits.h?

let
  version = "5.4.0";
  sha256 = "0fihlcy5hnksdxk0sn6bvgnyq8gfrgs8m794b1jxwd1dxinzg3b0";
  isl = nixpkgs.isl_0_14;
  inherit (nixpkgs) stdenv lib fetchurl;
  inherit (nixpkgs) gettext gmp libmpc libelf mpfr texinfo which zlib;
  stageName = if stage == 1 then "-stage1"
              else assert stage == 2; "";
in

stdenv.mkDerivation rec {
  name = "gcc-${version}-${target}${stageName}";

  inherit version;

  target = "${arch}-w64-mingw32";

  targetConfig = "${arch}-w64-mingw32";  # TODO: remove

  builder = ./builder.sh;

  src = fetchurl {
    url = "mirror://gnu/gcc/gcc-${version}/gcc-${version}.tar.bz2";
    inherit sha256;
  };

  patches = [
    ./format-security.patch
    ./use-source-date-epoch.patch
    ./libstdc++-target.patch
    ./no-sys-dirs.patch
    ./cppdefault.patch
  ];

  setOutputFlags = false;
  NIX_NO_SELF_RPATH = true;

  noSysDirs = true;

  langJava = false;
  crossStageStatic = (stage == 1);
  libcCross = libc;
  crossMingw = true;

  buildInputs = [
    binutils gettext gmp isl libmpc libelf mpfr texinfo which zlib
  ];

  libc_dev = nixpkgs.stdenv.cc.libc_dev;  # TODO: this is dumb, get rid of this

  dontDisableStatic = true;

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

  AR = "ar";
  LD = "ld";
  CC = "gcc";

  EXTRA_TARGET_CFLAGS = [ "-idirafter ${libc}/include" ]
      ++ lib.optionals (stage > 1) ["-B${libc}/lib"];

  EXTRA_TARGET_LDFLAGS =
    ["-Wl,-L${libc}/lib"]
    ++
    (
      if stage == 1 then
        ["-B${libc}/lib"]
      else
        ["-Wl,-rpath,${libc}/lib" "-Wl,-rpath-link,${libc}/lib"]
    )
    # TODO: add libpthread here?
    ;

  enableParallelBuilding = true;
  enableMultilib = false;

  hardeningDisable = [ "format" ];  # TODO: remove this line some day and patch GCC

  dontStrip = true;
  NIX_STRIP_DEBUG = 0;

  meta = {
    homepage = http://gcc.gnu.org/;
    license = lib.licenses.gpl3Plus;
  };

}
