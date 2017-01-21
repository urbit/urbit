{ crossenv }:

# TODO: get rid of boilerplate, refer to nixpkgs less (for fetchurl it is fine)
crossenv.nixpkgs.stdenv.mkDerivation rec {
  name = "libusbp-${version}-${crossenv.host}";

  version = "1.0.1";

  host = crossenv.host;

  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/pololu/libusbp/archive/${version}.tar.gz";
    sha256 = "1fa86imgkzbhnldb534gm8r8rzanql3vz55m856pq011ymgnifah";
  };

  buildInputs = [
    crossenv.gcc
    crossenv.binutils
    crossenv.nixpkgs.cmake
  ];

  builder = ./builder.sh;
}
