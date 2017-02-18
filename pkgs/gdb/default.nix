{ crossenv, zlib }:

crossenv.nixpkgs.stdenv.mkDerivation rec {
  name = "gdb-${version}-${crossenv.host}";

  # TODO: build gdb
}
