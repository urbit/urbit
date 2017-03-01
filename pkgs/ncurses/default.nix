{ crossenv }:

crossenv.nixpkgs.stdenv.mkDerivation rec {
  name = "ncurses-${version}-${crossenv.host}";

  version = "6.0";

  src = crossenv.nixpkgs.fetchurl {
    url = "mirror://gnu/ncurses/ncurses-${version}.tar.gz";
    sha256 = "0q3jck7lna77z5r42f13c4xglc7azd19pxfrjrpgp2yf615w4lgm";
  };

  builder = ./builder.sh;

  buildInputs = [
    crossenv.gcc
    crossenv.binutils
  ];

  inherit (crossenv) host os;
}

