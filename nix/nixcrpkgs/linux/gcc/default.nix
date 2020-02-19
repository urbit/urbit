{ native, host, binutils, headers, gcc_options }:

let
  nixpkgs = native.nixpkgs;
  isl = nixpkgs.isl;
  inherit (nixpkgs) stdenv lib fetchurl;
  inherit (nixpkgs) gmp libmpc libelf mpfr zlib;
in

native.make_derivation rec {
  name = "gcc-${version}-${host}";

  version = "8.2.0";
  src = fetchurl {
    url = "mirror://gnu/gcc/gcc-${version}/gcc-${version}.tar.xz";
    sha256 = "10007smilswiiv2ymazr3b6x2i933c0ycxrr529zh4r6p823qv0r";
  };

  musl_version = "1.1.24";
  musl_src = nixpkgs.fetchurl {
    url = "https://www.musl-libc.org/releases/musl-${musl_version}.tar.gz";
    sha256 = "18r2a00k82hz0mqdvgm7crzc7305l36109c0j9yjmkxj2alcjw0k";
  };

  inherit host headers;

  builder = ./builder.sh;

  patches = [
    # This patch is from nixpkgs.
    ./libstdc++-target.patch

    # Without this, we cannot build a simple hello world program for ARM.
    # See https://gcc.gnu.org/bugzilla/show_bug.cgi?id=31798
    ./link_gcc_c_sequence_spec.patch

    # Fix compilation errors about missing ISL function declarations.
    # https://gcc.gnu.org/git/?p=gcc.git;a=patch;h=05103aed1d34b5ca07c9a70c95a7cb1d47e22c47
    ./isl-headers.patch
  ];

  native_inputs = [ binutils ];

  gcc_conf =
    "--target=${host} " +
    gcc_options +
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
    "--with-zlib-include=${zlib.dev}/include " +
    "--with-zlib-lib=${zlib.out}/lib " +
    "--enable-deterministic-archives " +
    "--enable-languages=c,c++ " +
    "--enable-libstdcxx-time " +
    "--enable-static " +
    "--enable-tls " +
    "--disable-gnu-indirect-function " +
    "--disable-libmudflap " +
    "--disable-libmpx " +
    "--disable-libsanitizer " +
    "--disable-multilib " +
    "--disable-shared " +
    "--disable-werror";

  musl_conf =
    "--target=${host} " +
    "--disable-shared";

  hardeningDisable = [ "format" ];
}

