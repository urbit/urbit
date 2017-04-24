{ crossenv }:

crossenv.make_derivation rec {
  name = "libusbp-${version}-${crossenv.host}";

  version = "1.0.1ish";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/pololu/libusbp/archive/245f5e4.tar.gz";
    sha256 = "1xnv5w1c9zcdsdmgacgb1y5qlwpj0868v3cwcmbxxdx89hcsw2mp";
  };

  patches = [
  ];

  builder = ./builder.sh;
}
