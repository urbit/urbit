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

  gmp_src = fetchurl {
    url = "https://ftp.gnu.org/gnu/gmp/gmp-6.1.1.tar.xz";
    sha256 = "0cg84n482gcvl0s4xq4wgwsk4r0x0m8dnzpizwqdd2j8vw2rqvnk";
  };

  linux_src = fetchurl {
    url = "https://cdn.kernel.org/pub/linux/kernel/v4.x/linux-4.4.10.tar.xz";
    sha256 = "1kpjvvd9q9wwr3314q5ymvxii4dv2d27295bzly225wlc552xhja";
  };

  mpc_src = fetchurl {
    url = "https://ftp.gnu.org/gnu/mpc/mpc-1.0.3.tar.gz";
    sha256 = "1hzci2zrrd7v3g1jk35qindq05hbl0bhjcyyisq9z209xb3fqzb1";
  };

  mpfr_src = fetchurl {
    url = "https://ftp.gnu.org/gnu/mpfr/mpfr-3.1.4.tar.xz";
    sha256 = "1x8pcnpn1vxfzfsr0js07rwhwyq27fmdzcfjpzi5773ldnqi653n";
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

  buildInputs = [
    binutils gettext gmp isl libmpc libelf mpfr texinfo which zlib
  ];

  TARGET = host;
  LINUX_ARCH = "x86";  # TODO

  configure_flags2 =
    "--with-gnu-as " +
    "--with-gnu-ld " +
    "--with-as=${binutils}/bin/${host}-as " +
    "--with-ld=${binutils}/bin/${host}-ld ";

  configure_flags =
    "--target=${host} " +
    # "--with-sysroot=${libc} " +
    "--with-native-system-header-dir=/include " +
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

  meta = {
    homepage = http://gcc.gnu.org/;
    license = lib.licenses.gpl3Plus;
  };
}

# TODO: why is GCC providing a fixed limits.h?
