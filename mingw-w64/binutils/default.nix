# TODO: build without stdenv

{ nixpkgs, host }:

nixpkgs.stdenv.mkDerivation rec {
  name = "binutils-${version}-${host}";

  version = "2.27";

  src = nixpkgs.fetchurl {
    url = "mirror://gnu/binutils/binutils-${version}.tar.bz2";
    sha256 = "125clslv17xh1sab74343fg6v31msavpmaa1c1394zsqa773g5rn";
  };

  patches = [
    ./deterministic.patch
  ];

  buildInputs = [ nixpkgs.bison nixpkgs.zlib ];

  configure_flags =
    "--target=${host} " +
    "--enable-shared " +
    "--enable-deterministic-archives " +
    "--disable-werror ";

  builder = ./builder.sh;

  meta = with nixpkgs.stdenv.lib; {
    homepage = https://www.gnu.org/software/binutils/;
    license = licenses.gpl3Plus;
  };
}
