let

  pkgs = import ../../nix/nixpkgs.nix;

  easy-ps = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "2af967a3947f64921da77bdea632e37686f864ff";
    sha256 = "1z5i4l8ljyfv0s52rgdmb7mpj723r37nnldpqpp0ily7jrrlfvsm";
  });

in

pkgs.stdenv.mkDerivation {
  name = "purescript-context";
  builder = ./builder.sh;
  src = ./.;
  buildInputs = [
    easy-ps.purs
    easy-ps.spago
    easy-ps.psc-package2nix
  ];
}
