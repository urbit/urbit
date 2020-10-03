{ pkgs }:

pkgs.stdenv.mkDerivation rec {
  name = "libaes_siv-50955";
  buildInputs = [ pkgs.cmake pkgs.openssl ];
  builder = ./builder.sh;
  src = pkgs.fetchFromGitHub {
    owner = "frodwith";
    repo = "libaes_siv";
    rev = "509550e92a416172b9b8255e275f3a04d5fd4545";
    sha256 = "11clbvasyyc7ml2x9g5z3il6hs9gzsa10fcnj4grmijzm7gkb3qq";
  };
}
