{ nixpkgs, arch, stage ? 3, binutils, libc }:

let
  version = "5.4.0";
  sha256 = "0fihlcy5hnksdxk0sn6bvgnyq8gfrgs8m794b1jxwd1dxinzg3b0";

  crossConfigureFlags =
    " --with-arch=${arch}" +
    " --target=${arch}-w64-mingw32" +
    " --enable-threads=win32" +
    " --disable-nls" +
    " --disable-shared" +
    " --enable-sjlj-exceptions" +
    " --with-sysroot=${libc}/include --with-native-system-header-dir=/include" +
    (if stage == 1 then
      " --with-gcc" +
      " --with-gnu-as" +
      " --with-gnu-ld" +
      " --with-gnu-ld" +
      " --disable-debug" +
      " --disable-win32-registry"
    else
      " --with-as=${binutils}/bin/${arch}-w64-mingw32-as" +
      " --with-ld=${binutils}/bin/${arch}-w64-mingw32-ld" +
      " --enable-__cxa_atexit" +
      " --enable-long-long" +
      " --enable-hash-synchronization" +
      " --disable-libssp" +
      " --with-dwarf2" +
      " --enable-fully-dynamic-string"
    );

in

nixpkgs.stdenv.mkDerivation {
  name = "gcc-${version}-${arch}-w64-mingw32";

  builder = ./builder.sh;

  src = nixpkgs.fetchurl {
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

  buildInputs = with nixpkgs; [
    gmp mpfr libmpc libelf texinfo which gettext zlib binutils
  ];

  dontDisableStatic = true;

  configureFlags = "
    --enable-lto
    --enable-plugin
    --disable-libstdcxx-pch
    --without-included-gettext
    --with-system-zlib
    --enable-static
    --enable-languages=c,c++
    ${crossConfigureFlags}
    --disable-bootstrap
  ";

  targetConfig = "${arch}-w64-mingw32";

  buildFlags = "";

  makeFlags =
    if stage == 1 then
      ["all-gcc" "all-target-libcc"]
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
  CPATH = nixpkgs.lib.makeSearchPathOutput "dev" "include" [nixpkgs.zlib];

  # TODO: add libpthread here?
  LIBRARY_PATH = nixpkgs.lib.makeLibraryPath [nixpkgs.zlib];

  EXTRA_TARGET_CFLAGS = [ "-idirafter ${libc}/include" ]
      ++ nixpkgs.lib.optionals (stage > 1) ["-B${libc}/lib"];

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
  enableMultiLib = false;

  meta = {
    homepage = http://gcc.gnu.org/;
    license = nixpkgs.lib.licenses.gpl3Plus;
  };

  dontStrip = true;
  NIX_STRIP_DEBUG = 0;
}
