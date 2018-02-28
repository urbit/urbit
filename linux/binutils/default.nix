{ native, host }:

native.make_derivation rec {
  name = "binutils-${version}-${host}";

  version = "2.27";

  src = native.nixpkgs.fetchurl {
    url = "mirror://gnu/binutils/binutils-${version}.tar.bz2";
    sha256 = "125clslv17xh1sab74343fg6v31msavpmaa1c1394zsqa773g5rn";
  };

  patches = [
    ./deterministic.patch
  ];

  native_inputs = [ native.nixpkgs.bison native.nixpkgs.zlib ];

  configure_flags =
    "--target=${host} " +
    "--enable-shared " +
    "--enable-deterministic-archives " +
    "--disable-werror ";

  builder = ./builder.sh;
}
