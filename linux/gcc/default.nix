{ nixpkgs, host, stage ? 2, binutils, libc }:

let
  isl = nixpkgs.isl_0_14;
  inherit (nixpkgs) stdenv lib fetchurl;
  inherit (nixpkgs) gettext gmp libmpc libelf mpfr texinfo which zlib;

  stageName = if stage == 1 then "-stage1"
              else assert stage == 2; "";
in

stdenv.mkDerivation rec {
  name = "gcc-${version}-${host}${stageName}";

  version = "6.3.0";

  gcc_src = fetchurl {
    url = "mirror://gnu/gcc/gcc-${version}/gcc-${version}.tar.bz2";
    sha256 = "17xjz30jb65hcf714vn9gcxvrrji8j20xm7n33qg1ywhyzryfsph";
  };

  linux_src = fetchurl {
    url = "https://cdn.kernel.org/pub/linux/kernel/v4.x/linux-4.4.10.tar.xz";
    sha256 = "1kpjvvd9q9wwr3314q5ymvxii4dv2d27295bzly225wlc552xhja";
  };

  musl_src = nixpkgs.fetchurl {
    url = "https://www.musl-libc.org/releases/musl-1.1.16.tar.gz";
    sha256 = "048h0w4yjyza4h05bkc6dpwg3hq6l03na46g0q1ha8fpwnjqawck";
  };

  # TODO: do this use /usr/include/stdio.h at any point?  I think it might, so try
  # adding #error to it

  builder = ./builder.sh;

  patch_dir = ./patches;

  gcc_patches = [
    ./use-source-date-epoch.patch
    ./libstdc++-target.patch
    ./no-sys-dirs.patch
    ./cppdefault.patch
  ];

  buildInputs = [ binutils ];

  TARGET = host;
  LINUX_ARCH = "x86";  # TODO

  configure_flags2 =
    "--with-gnu-as " +
    "--with-gnu-ld " +
    "--with-as=${binutils}/bin/${host}-as " +
    "--with-ld=${binutils}/bin/${host}-ld " +
    "--with-isl=${isl} " +
    "--with-gmp-include=${gmp.dev}/include " +
    "--with-gmp-lib=${gmp.out}/lib " +
    "--with-libelf=${libelf}" +
    "--with-mpfr=${mpfr.dev} " +
    "--with-mpfr-include=${mpfr.dev}/include " +
    "--with-mpfr-lib=${mpfr.out}/lib " +
    "--with-mpc=${libmpc.out} " +
    "--with-zlib=${zlib.dev}" +
    "--with-zlib-lib=${zlib.out}";

  configure_flags =
    "--target=${host} " +
    # "--with-sysroot=${libc} " +
    "--with-native-system-header-dir=/include " +
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

  meta = {
    homepage = http://gcc.gnu.org/;
    license = lib.licenses.gpl3Plus;
  };
}

# TODO: why is GCC providing a fixed limits.h?
