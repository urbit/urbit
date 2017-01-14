{ nixpkgs, arch, stage ? 3, binutils, libc }:

let
  version = "5.4.0";
  sha256 = "0fihlcy5hnksdxk0sn6bvgnyq8gfrgs8m794b1jxwd1dxinzg3b0";
  isl = nixpkgs.isl_0_14;
  inherit (nixpkgs) stdenv lib fetchurl;
  inherit (nixpkgs) binutils gettext gmp libmpc libelf mpfr texinfo which zlib;
  stageName = if stage == 1 then "-stage1"
              else if stage == 2 then "-stage2"
              else assert stage == 3; "";
in

stdenv.mkDerivation {
  name = "gcc-${version}-${arch}-w64-mingw32${stageName}";

  builder = ./builder.sh;

  src = fetchurl {
    url = "mirror://gnu/gcc/gcc-${version}/gcc-${version}.tar.bz2";
    inherit sha256;
  };

  patches = [
    ./use-source-date-epoch.patch
    ./libstdc++-target.patch
    ./no-sys-dirs.patch
  ];

  outputs = [ "out" "lib" "man" "info" ];
  setOutputFlags = false;
  NIX_NO_SELF_RPATH = true;

  noSysDirs = true;

  staticCompiler = false;
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
    "--enable-lto " +
    "--enable-plugin " +
    "--with-isl=${isl} " +
    "--with-gmp-include=${gmp.dev}/include " +
    "--with-gmp-lib=${gmp.out}/lib " +
    "--with-mpfr-include=${mpfr.dev}/include " +
    "--with-mpfr-lib=${mpfr.out}/lib " +
    "--with-mpc=${libmpc} " +
    "--with-system-zlib " +
    "--enable-static " +
    "--enable-threads=win32 " +
    "--enable-sjlj-exceptions " +
    "--with-sysroot=${libc}/include --with-native-system-header-dir=/include " +
    (if stage == 1 then
      "--enable-languages=c " +
      "--with-gcc " +
      "--with-gnu-as " +
      "--with-gnu-ld " +
      "--with-gnu-ld " +
      "--disable-debug " +
      "--disable-win32-registry "
    else
      "--enable-languages=c,c++ " +
      "--with-as=${binutils}/bin/${arch}-w64-mingw32-as " +
      "--with-ld=${binutils}/bin/${arch}-w64-mingw32-ld " +
      "--enable-__cxa_atexit " +
      "--enable-long-long " +
      "--enable-hash-synchronization " +
      "--disable-libssp " +
      "--with-dwarf2 " +
      "--enable-fully-dynamic-string "
    ) +
    "--without-included-gettext " +
    "--disable-libstdcxx-pch " +
    "--disable-nls " +
    "--disable-shared " +
    "--disable-multilib " +
    "--disable-bootstrap";

  targetConfig = "${arch}-w64-mingw32";

  buildFlags = "";

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

  # TODO: add libpthread here
  CPATH = lib.makeSearchPathOutput "dev" "include" [zlib];

  # TODO: add libpthread here?
  LIBRARY_PATH = lib.makeLibraryPath [zlib];

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

  meta = {
    homepage = http://gcc.gnu.org/;
    license = lib.licenses.gpl3Plus;
  };

  dontStrip = true;
  NIX_STRIP_DEBUG = 0;
}
