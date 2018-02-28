{ native, host, binutils, headers, gcc_options }:

let
  nixpkgs = native.nixpkgs;
  isl = nixpkgs.isl_0_14;
  inherit (nixpkgs) stdenv lib fetchurl;
  inherit (nixpkgs) gmp libmpc libelf mpfr zlib;
in

native.make_derivation rec {
  name = "gcc-${gcc_version}-${host}";

  gcc_version = "6.3.0";
  gcc_src = fetchurl {
    url = "mirror://gnu/gcc/gcc-${gcc_version}/gcc-${gcc_version}.tar.bz2";
    sha256 = "17xjz30jb65hcf714vn9gcxvrrji8j20xm7n33qg1ywhyzryfsph";
  };

  musl_version = "1.1.16";
  musl_src = nixpkgs.fetchurl {
    url = "https://www.musl-libc.org/releases/musl-${musl_version}.tar.gz";
    sha256 = "048h0w4yjyza4h05bkc6dpwg3hq6l03na46g0q1ha8fpwnjqawck";
  };

  inherit host headers;

  builder = ./builder.sh;

  gcc_patches = [
    # These patches are from nixpkgs.
    ./use-source-date-epoch.patch
    ./libstdc++-target.patch

    # Without this, we cannot build a simple hello world program for ARM.
    # See https://gcc.gnu.org/bugzilla/show_bug.cgi?id=31798
    ./link_gcc_c_sequence_spec.patch

    # Fix a compiler error in GCC's ubsan.c: ISO C++ forbids comparison
    # between pointer and integer.
    ./ubsan.patch
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

